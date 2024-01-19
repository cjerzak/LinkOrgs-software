#'
#' A primarily internal function which builds the organizational record linkage models used in Libgober and Jerzak (2023+).
#'
#' @export
#'
#' @md


BuildML <- function(){

# packages
{
  library(fastmatch)
  library(tensorflow)
  library(keras)
  #try(tensorflow::use_python(python = "/Users/cjerzak/miniforge3/bin/python", required = T),T)
  #try(tensorflow::use_condaenv("tensorflow_m1", required = T, conda = "/opt/miniconda3/envs/tensorflow_m1"), T)
  tensorflow::use_condaenv(conda_env, required = T)
  py_gc <- reticulate::import("gc")
  Sys.sleep(4)
  try(print(   tf$config$list_physical_devices('GPU')  ), T)


  # set memory growth - dangerous!
  #tf$config$experimental$set_memory_growth(tf$config$list_physical_devices('GPU')[[1]], T)

  CurrentLocs <- .libPaths()[ sapply(.libPaths(),function(zer){"LinkOrgs" %in% list.files(zer)})]
  FilesInLocs <- sapply(CurrentLocs,function(loc_){
    "linkedIn_processedData_checkpoint.Rdata" %in% list.files(paste(loc_,"LinkOrgs/data/",sep="/"))
  })
  CurrentLoc <- CurrentLocs[which(FilesInLocs)[1]]
  load(paste(CurrentLoc,"LinkOrgs/data/linkedIn_processedData_checkpoint.Rdata",sep="/"))
}

# IDF LOADS
if(T == F){
    idf_values <- data.table::fread("./IDF_values.csv")
    idf_values_names <- as.character(enc2utf8(idf_values[,1][[1]]))
    idf_values <- as.numeric( idf_values[,2][[1]] )
    names(idf_values) <- idf_values_names; rm(idf_values_names)
    idf_values <<- idf_values
    median_idf <<- median(idf_values)
}

# set important parameters
{
  nSGD <- 5000L;LEARNING_RATE_BASE = 0.001; widthCycle <- 100L
  NA20 <- function(zer){zer[is.na(zer)] <- 0;zer[is.infinite(zer)] <- 0;zer}
  sgd_learning_seq <- tf$constant(LEARNING_RATE_BASE*abs(cos(1:nSGD/nSGD*widthCycle))*(1:nSGD<nSGD/2)+ NA20(LEARNING_RATE_BASE*(1:nSGD>=nSGD/2)/(1:nSGD-nSGD/2+1)^.3),tf$float32)
  adv_frac <- 0.5
  batch_size <- 50
  batch_size_posPool <- (batch_size_posPool_red<-batch_size_negPool <- 4*(batch_size)) * 3 #1.5 adjustment for drops
  print(sprintf("Log Implied Selection Threshold: %f",log((batch_size/2)/(batch_size_negPool^2/2-batch_size_negPool))))
  bn_momentum = 0.90
  max_nWordsPerAlias <- 20L
  max_nCharsPerWord <- 20L + 2L
  nEmbed_chars <- as.integer(  128L  )
  nEmbed_relpos <- as.integer(  128L  )
  nDim_lstm <- 128L; activation_lstm <- "swish"
  ffd <- as.integer(round(nEmbed_chars  * 2))     # nEmbed_chars*2L is good
  transformerDepth_selfe <- 1L
  FinalAliasDim <- 256L
  nHidden <- 1L
  hiddenProjDims <- rep(FinalAliasDim*2L, nHidden)
  #hiddenProjDims <- rep(nEmbed_chars*4,nHidden)
  LSTM_DROPOUT <- 0.
  dropout_transformer <- 0.
  dropout_dense <- 0. # 0 was fine
  attention_axes <- NULL
  LN_axis_LSTM <- 2L; LN_axis_transformer <- 2L #
  head_size <- nEmbed_chars #the depth of the d-dimensional space used for positional encoding
  nHeads = 3L #The number of heads to use in the multi-head-attention block
  myActivation <- "swish"
  alpha <- 0.5; # alpha -> gives weight to attention i minus 1
  paddingType <- "valid"
  optimizer_tf = tf$optimizers$Nadam()
  #optimizer_tf$iterations$assign(tf$constant(661L, tf$int64))
  ep_BN <- 0.001
  ep_LN <- 0.001
  ep_SQRT <- tf$constant(0.001^2,tf$float32)
  ep_LabelSmooth <- tf$constant(0.1,tf$float32)
  tf_true <- tf$constant(T,tf$bool)

  # deal with randomness - IMPORTANT: MUST SET SEED FOR ALL RANDOM INITIALIZATIONS TO WORK
  tf$random$set_seed(round(runif(1,0,10^10)))
  # with(tf$device('/cpu:0'), {g1 <- tf$random$Generator$from_seed(1)})
  embedInit <- tf$keras$initializers$random_uniform(minval = -0.05,
                                                    maxval = 0.05) # "uniform", https://aclanthology.org/W17-7508.pdf
}

# load in utility functions
{
  # write key helper functions
  charIndicators[,1] <- enc2utf8(charIndicators[,1])
  START_CODON_WORD_zi <- ai(nrow(charIndicators))
  STOP_CODON_WORD_zi <- ai(nrow(charIndicators)+1L)
  BLANK_CODON_WORD_zi <- ai(nrow(charIndicators)+2L)
  BLANK_CODON_WORD_zi_tf <- tf$constant(BLANK_CODON_WORD_zi,tf$int32)
  baseVec <- rep(BLANK_CODON_WORD_zi,times = max_nCharsPerWord)
  name2char_indices <- function(names_,returnWordPositions = F){
    # convert characters to indices of associated character vectors
    do.call(rbind,sapply(names_,function(name_){
      indexes <- ai(charIndicators[,2][ fmatch(strsplit(name_,split="")[[1]],
                                               charIndicators[,1])]) - 1L
      indexes <- c(START_CODON_WORD_zi,indexes,STOP_CODON_WORD_zi)
      presentChars_ <- 1:min(length(indexes),max_nCharsPerWord)
      baseVec[presentChars_] <- indexes[presentChars_]
      return( baseVec )
    },simplify = F ) )
  }
  image2 = function(mat,xaxt=NULL,yaxt = NULL,main=NULL,scale_vec=c(1,1.04),cex.axis = 1){
    # pretty plotting of 2D data
    image((t(mat)[,nrow(mat):1]), axes = F, main = main,xaxs = "i",cex.main = 2)
    if(!is.null(xaxt)){ axis(1, at = 0:(nrow(mat)-1)/nrow(mat)*scale_vec[1], tick=F,labels = (xaxt),cex.axis = cex.axis,las = 1)  }
    if(!is.null(yaxt)){ axis(2, at = 0:(nrow(mat)-1)/nrow(mat)*scale_vec[2], tick=F,labels = rev(yaxt),cex.axis = cex.axis,las = 2)  }
  }
  HASH_TABLE <- as.data.frame( cbind(1:length(HASH_TABLE <-c(letters,LETTERS,
                                                             1:(max_nCharsPerAlias-26*2)) ),HASH_TABLE) )
  pushWords <- function(names_){
    # push shorter sequence to be of max_nWords in word length
    sapply(names_, function(name_){
      tmp_ <- (strsplit(name_,split=" ")[[1]] )
      nWords_name_ <- length(tmp_)
      neededWords <- max_nWords - nWords_name_; add_ <- ""
      add_ <- c(); c_ <- 0
      if(length(tmp_) ==1 ){tmp_<- c(tmp_,tmp_)}
      tmp_rev <- paste(rev(tmp_)[-1],collapse=" ")
      tmp_ <- paste(tmp_[-1],collapse= " ")
      ok <- F; while(!ok){
        c_ <- c_ + 1 ; if(c_ %% 2 == 1){ add_ <- paste(add_, tmp_rev,sep = " ") }
        if(c_ %% 2 == 0){ add_ <- paste(add_, tmp_,sep =" ") }
        ok <- length(strsplit(add_,split=" ")[[1]]) > neededWords
      }
      final_ <- paste(name_, add_,sep= "")
      final_split <- strsplit(final_,split=" ")[[1]]
      if(length(final_split) < max_nWords){browser()}
      if(length(final_split) > max_nWords){final_ = paste(final_split[1:max_nWords],collapse=" ")}
      return(   final_    )
    },USE.NAMES = F)
  }
  pushSeq <- function(era, returnIndices = "seqPush1", maxSeqLen){
    # push shorter sequence to be of maxSeqLen in character length
    sapply(era,function(eraa){
      if(returnIndices %fin% c("wordPush","seqPush2") ){
        tmp__ <- HASH_TABLE[,2]
        if(returnIndices == "wordPush"){ eraa_split <- strsplit(eraa,split=" ")[[1]]
        eraa_split <- sapply(1:length(eraa_split),function(zer){
          paste(rep(tmp__[zer],times=nchar(eraa_split[zer])),collapse="") })
        eraa <- paste(eraa_split,collapse = " ")
        }
        if(returnIndices == "seqPush2"){ eraa_split <- strsplit(eraa,split="")[[1]]
        eraa_split <- sapply(1:length(eraa_split),function(zer){
          paste(rep(tmp__[zer],times=nchar(eraa_split[zer])),collapse="") })
        eraa <- paste(eraa_split,collapse = "")
        eraa <- paste(replicate(ceiling(max_nCharsPerAlias/nchar(eraa)),eraa),collapse="")
        eraa <- paste(strsplit(eraa,split="")[[1]][1:max_nCharsPerAlias],collapse="")
        }
      }

      nChars_eraa <- nchar(eraa)
      neededChars <- maxSeqLen - nChars_eraa; add_ <- ""
      pushType <- "repeat"
      if(neededChars>0 & pushType == "repeat"){
        eraa <- paste(replicate(ceiling(max_nCharsPerAlias/nchar(eraa)),eraa),collapse="")
        eraa <- paste(strsplit(eraa,split="")[[1]][1:max_nCharsPerAlias],collapse="")
      }
      if(neededChars>0 & pushType == "reflection"){
        if(T == T){ add_ <- c(); c_ <- 0
        tmp_ <- (strsplit(eraa,split=" ")[[1]] )
        if(length(tmp_) ==1 ){tmp_<- c(tmp_,tmp_)}
        tmp_rev <- paste(rev(tmp_)[-1],collapse=" ")
        tmp_ <- paste(tmp_[-1],collapse= " ")
        ok <- F; while(!ok){
          c_ <- c_ + 1
          if(c_ %% 2 == 1){ add_ <- paste(add_, tmp_rev,collapse = " ") }
          if(c_ %% 2 == 0){ add_ <- paste(add_, tmp_,sep =" ") }
          ok <- nchar(add_) >= neededChars
        }
        add_ <- c("", strsplit(paste(add_,collapse=" "),split="")[[1]][0:(neededChars)])
        } }
      final_ <- paste(eraa, paste(add_,collapse=""),sep="")
      if(nchar(final_) > maxSeqLen){final_ = substr(final_,start=0,stop=maxSeqLen)}
      return( final_ )
    } ) }
}

tfp <- tf_probability()
# define key variables
trainingVars <- ls(); {
  # define some tf constants
  nEmbed_chars_tf <- tf$constant(nEmbed_chars,dtype=tf$float32)
  one_tf <- tf$constant(1.,dtype=tf$float32)
  neg1_tf <- tf$constant(-1L,dtype=tf$int32)
  zero_tf <- tf$constant(0L,dtype=tf$int32)
  two_tf <- tf$constant(2.,dtype=tf$float32)
  ep_tf <- tf$constant(1e-5,dtype=tf$float32)
  oneEfive_tf <- tf$constant(1e5,dtype=tf$float32)

  # transformer layers
  FFDropout <- tf$keras$layers$Dropout(dropout_transformer, noise_shape = list(1L,1L,NULL),name="FFDropout")
  DenseDropout <- tf$keras$layers$Dropout(dropout_dense, noise_shape = list(1L,NULL),name="DenseDropout")

# uncomment for transformer
  if(T == F){
    IDF_SCALE <- tf$Variable(tfp$math$softplus_inverse(1), trainable = T,name="IDF_SCALE")
    IDF_POW <- tf$Variable(tfp$math$softplus_inverse(0.0001), trainable = T,name="IDF_POW")

    NormType <- tf$keras$layers$LayerNormalization
    LN_wMask <- ( function(X,MASK){
        X_m <- tf$multiply(X,MASK)
        m_norm <- tf$divide(one_tf,tf$add(ep_LN, tf$reduce_sum(MASK,1L)))
        mu_ <- tf$expand_dims( tf$multiply(m_norm, tf$reduce_sum(X_m,1L)),1L)
        var_ <- tf$subtract( tf$expand_dims( tf$multiply(m_norm,
                        tf$reduce_sum(tf$square(X_m),1L)),1L), tf$square(mu_) )
        return(X_normed <- tf$divide( tf$subtract(X, mu_), tf$sqrt( tf$add(0.000001, var_)  )))
    } )
    LN_noMask <- ( function(X,MASK){
        mu_ <- tf$expand_dims(tf$reduce_mean(X,1L),1L)
        var_ <- tf$subtract(tf$expand_dims(tf$reduce_mean(tf$square(X),1L),1L) ,tf$square(mu_))
        return( X_normed <- tf$divide( tf$subtract(X, mu_), tf$sqrt( tf$add(ep_LN, var_)  )) )
    } )
    LN_2D <- ( function(X){
      mu_ <- tf$expand_dims(tf$expand_dims(tf$reduce_mean(X,1L:2L),1L),2L)
      var_ <- tf$expand_dims(tf$expand_dims(tf$math$reduce_mean(tf$square(X),1L:2L),1L),2L)
      var_ <- tf$subtract(var_ ,tf$square(mu_))
      return( X_normed <- tf$divide( tf$subtract(X, mu_), tf$sqrt( tf$add(ep_LN, var_)  )) )
    } )

    if((use_mask <- T) == F){ LN_fxn <- LN_noMask}
    if(use_mask == T){ LN_fxn <- LN_wMask}
    alpha_tf <- tf$Variable(alpha,trainable=F,name='alpha')
    for(attn_ in c("word","alias")){
    for(l_ in 1:transformerDepth_selfe){
      # define multihead attention block
      eval(parse(text = sprintf("MultiHeadAttentionLayer_%s_%s <- tf$keras$layers$MultiHeadAttention(key_dim=head_size,
                     num_heads=as.integer(nHeads), dropout=0.,attention_axes = attention_axes,name='MH_%s_%s')",attn_,l_,attn_,l_)))

      # pointwise feed-forward layer
      eval(parse(text = sprintf("Conv%s_%s_nonlinear = tf$keras$layers$Conv1D(filters=ffd, kernel_size=1L, activation = myActivation)",attn_,l_)))
      eval(parse(text = sprintf("Conv%s_%s_linear = tf$keras$layers$Conv1D(filters=nEmbed_chars, kernel_size=1L, activation = 'linear')",attn_,l_)))

      for(h_ in 1:nHeads){
        eval(parse(text = sprintf("LN1Transformer%s_%s_%s = NormType(epsilon=ep_LN,axis=LN_axis_transformer,name='LN1_%s_%s_%s')",attn_, l_,h_,attn_,l_,h_)))
        eval(parse(text = sprintf("LN2Transformer%s_%s_%s = NormType(epsilon=ep_LN,axis=LN_axis_transformer,name='LN2_%s_%s_%s')",attn_, l_,h_,attn_,l_,h_)))
        eval(parse(text = sprintf("Abeta%s_%s_%s = tf$Variable(rep(0,times=nEmbed_chars),trainable=T)",attn_,l_,h_)))
        eval(parse(text = sprintf("Bbeta%s_%s_%s = tf$Variable(rep(0,times=nEmbed_chars),trainable=T)",attn_,l_,h_)))
        eval(parse(text = sprintf("Agamma%s_%s_%s = tf$Variable(rep(1,times=nEmbed_chars),trainable=T)",attn_,l_,h_)))
        eval(parse(text = sprintf("Bgamma%s_%s_%s = tf$Variable(rep(1,times=nEmbed_chars),trainable=T)",attn_,l_,h_)))
        eval(parse(text = sprintf("qRelWordCharsPos%s_%s_%s <- tf$Variable(initial_value=tf$initializers$random_uniform(minval=-0.01,maxval=0.01)(shape=list(2L*nEmbed_relpos,1L)), trainable = T,name='qRelWord_%s_%s_%s')", attn_,l_,h_,attn_,l_,h_)))
      }
    }

      # define functions
      getAttentionBlock <- function(EMBEDDING,Mask1D,TRAINING){
        # define masks
        Mask2D <- tf$cast(tf$expand_dims( Mask1D , 2L), dtype = tf$float32)
        Mask2D_TIMES_Mask2D_t = tf$multiply(tf$expand_dims(tf$cast(Mask1D,tf$float32),1L),
                                            tf$expand_dims(tf$cast(Mask1D,tf$float32),2L))

        # sin / cos approach for char pos embeds
        PosSeq <- tf$add(tf$cast(tf$range(tf$shape(EMBEDDING)[2]),tf$float32),one_tf)
        PosSeq <- tf$expand_dims(tf$expand_dims(PosSeq,0L),2L)
        DimSeq <- tf$expand_dims(tf$expand_dims(tf$add(tf$cast(tf$range(tf$shape(EMBEDDING)[3]),tf$float32),1.),0L),0L)
        IsOddDim <- tf$math$mod(DimSeq,2L)
        PE <- tf$add(tf$multiply(IsOddDim,tf$sin(PosSeq / tf$pow(10000., DimSeq/nEmbed_chars_tf))),
                     tf$multiply((1-IsOddDim),tf$math$cos(PosSeq / tf$pow(10000., DimSeq/nEmbed_chars_tf))))

        # input encoding
        xCt <- tf$add(EMBEDDING, PE )

        # execute transformer layers
        normType <- "orig"
        transformerDepth_use <- transformerDepth_selfe
        for(l_ in 1L:transformerDepth_use){
          # add different gamma and beta for q, k, v for ed layer
          # iterate over heads

          if(T == F){
            # obtain multihead
            qt0 <- xCt
            VProj <- eval(parse(text = sprintf("MultiHeadAttentionLayer_%s_%s(
                                      query = xCt, key = xCt, value = xCt,  attention_mask = Mask2D_TIMES_Mask2D_t,  training = TRAINING
                                        )",transformerType,l_)))
          }

          if(T == T){
          for(h_ in 1:nHeads){
            # define q, k, v
            qt0 <- xCt
            if(normType != "orig"){
              eval(parse(text = sprintf("xCt <-tf$add(tf$multiply(LN_fxn( xCt , Mask2D), Agamma%s_%s_%s), Abeta%s_%s_%s)",transformerType, l_,h_,transformerType,l_,h_)))
            }
            #xCt <- tf$multiply(xCt, Mask2D) # mask multiplication avoids infinities in the network evolution
            qt <- kt <- vt <- xCt # mask multiplication avoids infinities in the network evolution

            # setup multihead on h = 1
            if(h_ == 1){
              # obtain multihead
              MultiHeadAttentionLayer_t <- eval(parse(text = sprintf("MultiHeadAttentionLayer_%s_%s",transformerType,l_)))

              # initialize multihead if needed
              if(length(MultiHeadAttentionLayer_t$variables) == 0){ MultiHeadAttentionLayer_t(query = qt, key = kt, value = vt,  attention_mask = NULL,  training = TRAINING) }

              # obtain variables from multihead
              MultiheadVars_t <- MultiHeadAttentionLayer_t$variables
              names(MultiheadVars_t) <- unlist(lapply(MultiHeadAttentionLayer_t$variables,function(zer){zer$name}))
            }

            # query and key projections
            eval(parse(text=sprintf("Q%s = tf$add( tf$matmul(qt,MultiheadVars_t[[grep(names(MultiheadVars_t),pattern='query/kernel')]][,%s,]),
                        MultiheadVars_t[[grep(names(MultiheadVars_t),pattern='query/bias')]][%s,])",h_,h_,h_)))
            eval(parse(text = sprintf("K%s = tf$add(tf$matmul(kt,MultiheadVars_t[[grep(names(MultiheadVars_t),pattern='key/kernel')]][,%s,]),
                        MultiheadVars_t[[grep(names(MultiheadVars_t),pattern='key/bias')]][%s,])", h_,h_,h_)))

            # generate attention
            if( l_ == 1 & T == F ){
              # comment out when using sin/cos embeddings
              eval(parse(text = sprintf("logitA%s_%s_ <-
                                      tf$truediv(
                                      tf$add(
                                        tf$matmul(Q%s, tf$transpose(K%s,c(0L,2L,1L))),
                                        tf$squeeze(tf$matmul(tf$concat(list(RelCharPosEmbedding_,RelWordPosEmbedding_),3L),
                                        qRelWordCharsPos%s_%s_%s),3L)),
                                        tf$sqrt(nEmbed_chars_tf))", l_,h_,h_,h_,transformerType, l_, h_ )))
            }
            if( l_ > 1 | T == T ){
              # no relative position embeddings are added
              eval(parse(text = sprintf("logitA%s_%s_ <-
                                      tf$truediv(tf$matmul(Q%s, tf$transpose(K%s,c(0L,2L,1L))),
                                        tf$sqrt(nEmbed_chars_tf))", l_, h_,h_,h_)))
            }

            # residual connection
            if(l_ == 1){ eval(parse(text=sprintf("logitA%s_%s = logitA%s_%s_",l_,h_,l_,h_))) }
            if(l_>1){
              eval(parse(text = sprintf("
                              logitA%s_%s <- tf$add(tf$multiply( tf$subtract(one_tf,alpha_tf), logitA%s_%s_),  tf$multiply(alpha_tf, logitA%s_%s))",
                                        l_,h_, l_,h_, l_-1, h_ )))
            }

            ## attention softmax - with normalization
            # approach 1 - first column fixed to 0
            #eval(parse(text = sprintf("A%s_%s = tf$keras$activations$softmax(tf$concat(list(tf$zeros(list(tf$shape(logitA%s_%s)[1],tf$shape(logitA%s_%s)[2],1L)), tf$gather(logitA%s_%s,
                                      #tf$range(tf$shape(logitA%s_%s)[2]neg1_tf)+1L, axis = 2L)),2L),2L)", l_,h_, l_, h_,l_, h_, l_, h_,l_, h_)))

            # approach 2 - subtract off first column
            eval(parse(text=sprintf("A%s_%s = tf$keras$activations$softmax(logitA%s_%s - tf$expand_dims(tf$gather(logitA%s_%s,0L,axis=2L),2L),2L)", l_,h_, l_, h_, l_, h_)))

            # approach 3 - each element subtracts off max
            #eval(parse(text = sprintf("A%s_%s = tf$keras$activations$softmax(logitA%s_%s - tf$reduce_max(logitA%s_%s,2L,keepdims=T),2L)", l_,h_,l_,h_, l_, h_)))

            # approach 4 - element norm
            #eval(parse(text = sprintf("A%s_%s = tf$keras$activations$softmax(tf$math$l2_normalize(logitA%s_%s,2L),2L)", l_, h_, l_, h_)))

            # approach 5 - no normalization
            #eval(parse(text=sprintf("A%s_%s = tf$keras$activations$softmax(logitA%s_%s,2L)", l_,h_, l_, h_)))

            # mask attention
            if(use_mask == T){
              eval(parse(text=sprintf("A%s_%s = tf$multiply(A%s_%s, Mask2D_TIMES_Mask2D_t)", l_, h_, l_, h_)))
              eval(parse(text=sprintf("A%s_%s = tf$truediv(A%s_%s, tf$add(ep_tf,tf$expand_dims(tf$reduce_sum(A%s_%s, 2L),2L)))", l_, h_, l_, h_, l_, h_)))
            }

            # value function
            eval(parse(text = sprintf("H%s = tf$add(tf$matmul( tf$matmul(A%s_%s,vt),
                 MultiheadVars_t[[grep(names(MultiheadVars_t),pattern='value/kernel')]][,%s,]),
                       MultiheadVars_t[[grep(names(MultiheadVars_t),pattern='value/bias')]][%s,])", h_,l_,h_,h_,h_)))
          }

          # value projection
          VProj = tf$add(tf$matmul(tf$concat(eval(parse(text=sprintf("list(%s)",paste(paste("H",1:nHeads,sep=""),collapse=",")))),2L),
                                   tf$reshape(MultiheadVars_t[[grep(names(MultiheadVars_t),pattern="output/kernel")]],list(nHeads*nEmbed_chars,nEmbed_chars)) ),
                         MultiheadVars_t[[grep(names(MultiheadVars_t),pattern="output/bias")]]  )
          }


          # residual connection for feed forward layer
          #if(paddingType == "valid" & beta > 0)InputPlusMultihead <- tf$add(VProj, eval(parse(text=sprintf("tf$multiply( tf$constant(1./KernelSize,tf$float32), Conv_Mean%s_1D( xCt ) )",l_))))
          {
            if(normType != "orig"){
              InputPlusMultihead <- tf$add(VProj, qt)
              eval(parse(text = sprintf("InputPlusMultihead_n <- tf$add(tf$multiply(LN_fxn( InputPlusMultihead, Mask2D), Bgamma%s_%s_%s), Bbeta%s_%s_%s)",transformerType,l_,h_,transformerType,l_, h_)))
            }
            if(normType == "orig"){
              eval(parse(text = sprintf("VProj <-tf$add(tf$multiply(LN_fxn( VProj , Mask2D), Agamma%s_%s_%s), Abeta%s_%s_%s)",transformerType, l_,h_,transformerType,l_,h_)))
              InputPlusMultihead_n <- InputPlusMultihead <- tf$add(VProj, qt0)
            }
            #InputPlusMultihead_n <- tf$multiply(InputPlusMultihead_n,Mask2D)
          }

          # Feed Forward 1
          eval(parse(text = sprintf("FFLayer <- Conv%s_%s_nonlinear( FFDropout( InputPlusMultihead_n,training = TRAINING) )", transformerType,l_)))

          # Feed Forward 2
          eval(parse(text = sprintf("FFLayer <- Conv%s_%s_linear(  FFLayer )", transformerType, l_)))

          if(normType == "orig"){
            eval(parse(text = sprintf("FFLayer <- tf$add(tf$multiply(LN_fxn( FFLayer, Mask2D), Bgamma%s_%s_%s), Bbeta%s_%s_%s)",transformerType,l_,h_,transformerType,l_, h_)))
          }

          # residual connection for feed forward layer, part 2
          xCt <- tf$add(InputPlusMultihead, FFLayer)

          if(use_mask == T){  xCt <- tf$multiply(xCt,Mask2D) }
        }

        return(  xCt  )
      }
      getAttentionBlock <- deparse1(getAttentionBlock,collapse="\n")
      getAttentionBlock <- gsub(getAttentionBlock,pattern="function \\(\\)",replace="")
      getAttentionBlock <- gsub(getAttentionBlock,pattern="transformerType",replace = sprintf("'%s'",attn_))
      if(attn_ == "word"){ getAttentionBlock_word <- eval(parse( text = getAttentionBlock )) }
      if(attn_ == "alias"){ getAttentionBlock_alias <- eval(parse( text = getAttentionBlock )) }
    }


    IDFProj <- tf$keras$layers$Dense(nEmbed_chars,name="WordCharPosProj",use_bias=F,
                                  kernel_initializer = tf$keras$initializers$random_uniform(minval = -0.01,maxval = 0.01))
    ExperimentalProj <- tf$Variable(tf$keras$initializers$random_uniform(minval = -0.01,maxval = 0.01)(list(10L,max_nCharsPerAlias)), trainable=T,name="SpaceEmbed")
    IdentityInit <- function(shape_,...){
      #DiagInit <- tf$transpose(tf$eye(as.integer(shape_[[1]]),batch_shape=list(as.integer(shape_[[3]]),
      #as.integer(shape_[[4]]))),c(2L,3L,0L,1L))*1./(tf$cast(shape_[[1]]^2.,tf$float32)*nEmbed_chars)
      #DiagInit <- DiagInit + tf$random_uniform_initializer(minval=-0.0000001, maxval=-0.00001)(shape_)

      # zero on diag initialization
      #max(as.array(tf$keras$initializers$glorot_uniform()(shape_)))
      DiagInit <- tf$transpose(tf$eye(as.integer(shape_[[1]]),batch_shape=list(as.integer(shape_[[3]]),
                                                                               as.integer(shape_[[4]]))),c(2L,3L,0L,1L))
      DiagInit <- DiagInit * (nf_ <- 1./(tf$cast(shape_[[1]]^2,tf$float32)*shape_[[4]]))
      DiagInit <- DiagInit + tf$random_normal_initializer(mean = 0., stddev = 0.25*nf_)(shape_)
      #image2( as.matrix(  DiagInit[,,1,1] ) )
      #ZeroDiag <- tf$ones(shape_) - DiagInit
      #DiagInit <- ZeroDiag * tf$random_uniform_initializer(minval = -nf_, maxval= nf_)(shape_)
      return( DiagInit )
    }
    for(type_ in c("r","t")){
      #For an odd-sized filter, all the previous layer pixels would be symmetrically around the output pixel. Without this symmetry, we will have to account for distortions across the layers which happens when using an even sized kernel
      ConvLayers <- 3;filterDim_seq <- c(32L,32L,32L)
      for(k_ in 1:length((ksseq<-as.integer(c(3,5,9))))){ # word distributions <http://www.ravi.io/language-word-lengths>
        for(a_ in 1:ConvLayers){ # iterate over depth
          #eval(parse(text=sprintf('DistinguishConv%s_%s <- tf$keras$layers$Conv2D(nEmbed_chars,kernel_size = as.integer(ksseq[k_]),padding="valid",activation="linear",kernel_initializer=IdentityInit)',k_,a_)))
          #eval(parse(text=sprintf('DistinguishConvProj%s_%s <- tf$identity',k_,a_)))
          # this works
          #eval(parse(text=sprintf('DistinguishConv%s_%s_%s <- tf$keras$layers$Conv2D(nEmbed_chars,kernel_size = as.integer(ksseq[k_]),padding="valid",activation="linear",kernel_initializer=IdentityInit)',k_,a_,type_)))
          #eval(parse(text=sprintf('DistinguishConvProj%s_%s_%s <- tf$keras$layers$Conv2D(nEmbed_chars,kernel_size = as.integer(1L),padding="valid",activation="swish")',k_,a_,type_)))

          # experimental
          filterDim <- filterDim_seq[a_]
          nl_ <- 1./(ksseq[k_][[1]]^2*filterDim)
          strides_v <- c(1L,1L)
          #eval(parse(text=sprintf('DistinguishConv%s_%s_%s <- tf$keras$layers$Conv2D(filterDim,kernel_size = as.integer(ksseq[k_]), padding="valid",activation="linear",strides = strides_v,kernel_initializer=IdentityInit,trainable = T)',k_,a_,type_)))
          #eval(parse(text=sprintf('DistinguishConv%s_%s_%s <- tf$keras$layers$Conv2D(filterDim,kernel_size = as.integer(ksseq[k_]), padding="valid",activation="linear",strides = strides_v)',k_,a_,type_)))
          if(a_ == 1){eval(parse(text=sprintf('DistinguishConv%s_%s_%s <- tf$keras$layers$Conv2D(filterDim,kernel_size = as.integer(ksseq[k_]), padding="valid",activation="linear",strides = strides_v,kernel_initializer=IdentityInit)',k_,a_,type_)))}
          if(a_ > 1){eval(parse(text=sprintf('DistinguishConv%s_%s_%s <- tf$keras$layers$Conv2D(filterDim,kernel_size = as.integer(ksseq[k_]), padding="valid",activation="linear",strides = strides_v)',k_,a_,type_)))}
          #eval(parse(text=sprintf('DistinguishConvProj%s_%s_%s <- tf$keras$layers$Conv2D(3L,kernel_size = as.integer(1L),padding="valid",activation="swish")',k_,a_,type_)))

          #if(a_ == 1){eval(parse(text=sprintf('DistinguishConv%s_%s <- tf$keras$layers$Conv2D(nEmbed_chars,kernel_size = as.integer(ksseq[k_]),padding="valid",activation="linear",kernel_initializer=IdentityInit)',k_,a_)))}
          #if(a_ > 1){eval(parse(text=sprintf('DistinguishConv%s_%s <- tf$keras$layers$Conv2D(nEmbed_chars,kernel_size = as.integer(ksseq[k_]),padding="valid",activation="linear",kernel_initializer=tf$keras$initializers$random_uniform(0.5/(k_^2*nEmbed_chars), 2/(k_^2*nEmbed_chars)))',k_,a_)))}
          #if(a_ > 1){eval(parse(text=sprintf('DistinguishConv%s_%s <- tf$keras$layers$Conv2D(nEmbed_chars,kernel_size = as.integer(ksseq[k_]),padding="valid",activation="linear",kernel_initializer=tf$keras$initializers$variance_scaling)',k_,a_)))}
        }}
    }
    GlobalAvePoolLayer <- tf$keras$layers$GlobalAveragePooling1D()
    GlobalMaxPoolLayer <- tf$keras$layers$GlobalMaxPooling1D()
    GlobalAvePolLayer2D <- tf$keras$layers$GlobalAveragePooling2D()
    GlobalMaxPoolLayer2D <- tf$keras$layers$GlobalMaxPooling2D()
  }

  Cbeta_f = tf$Variable(rep(0,times=nEmbed_chars),trainable=T)
  Cgamma_f = tf$Variable(rep(1,times=nEmbed_chars),trainable=T)
  Dbeta_f = tf$Variable(rep(0,times=nEmbed_chars),trainable=T)
  Dgamma_f = tf$Variable(rep(1,times=nEmbed_chars),trainable=T)

  ## embedding layers
  # character embedding - one vector for each character
  CharEmbeddingLayer <- tf$keras$layers$Embedding(output_dim = nEmbed_chars,
                                                  input_dim = nchars+3L,
                                                  name = "CharEmbeds",
                                                  embeddings_initializer = embedInit)

  # relative character embeddings - one vector for each character position relation (i before j by n/j after i by n)
  relCharsPosMat <- matrix(apply(rm_<-expand.grid(1:max_nCharsPerAlias,1:max_nCharsPerAlias),1,diff),
                           byrow = F, nrow = max_nCharsPerAlias)
  relCharsPosMat[relCharsPosMat<0] <- abs(relCharsPosMat[relCharsPosMat<0]) + (RelCharPosAdd<-max(relCharsPosMat))
  RelCharsPosEmbeddingLayer <- tf$keras$layers$Embedding(output_dim = nEmbed_relpos,
                                                         input_dim = as.integer(max_nCharsPerAlias*2) + 1L, # plus 1 for null vector
                                                         name="CharsPosEmbeds",embeddings_initializer = embedInit)
  rm(rm_)

  # relative word embeddings - one vector for each word position relation (i before j by n/j after i by n)
  maxWordsInSeq <- max_nCharsPerAlias; RelWordPosAdd <- maxWordsInSeq
  RelPos_guide <- expand.grid(1:maxWordsInSeq,1:maxWordsInSeq)
  RelWordPosEmbeddingLayer <- tf$keras$layers$Embedding(output_dim = nEmbed_relpos,
                                                        input_dim = wordLocInputDim <- as.integer(maxWordsInSeq*2+1L),# plus 1 for null vector
                                                        #input_dim = wordLocInputDim <- (as.integer((maxWordsInSeq<-max_nCharsPerAlias)*2)+1L),# plus 1 for null vector
                                                        name="RelWordPosEmbeds",embeddings_initializer = embedInit)
  CharsInput1_BNLayer <- tf$keras$layers$BatchNormalization(axis = 2L, center = T, scale = T, momentum = bn_momentum, epsilon = ep_BN)
  CharsInput2_BNLayer <- tf$keras$layers$BatchNormalization(axis = 2L, center = T, scale = T, momentum = bn_momentum, epsilon = ep_BN)
  CharsInput3_BNLayer <- tf$keras$layers$BatchNormalization(axis = 2L, center = T, scale = T, momentum = bn_momentum, epsilon = ep_BN)
  WordInput1_BNLayer <- tf$keras$layers$BatchNormalization(axis = 2L, center = T, scale = T, momentum = bn_momentum, epsilon = ep_BN)
  WordInput2_BNLayer <- tf$keras$layers$BatchNormalization(axis = 2L, center = T, scale = T, momentum = bn_momentum, epsilon = ep_BN)
  WordInput3_BNLayer <- tf$keras$layers$BatchNormalization(axis = 2L, center = T, scale = T, momentum = bn_momentum, epsilon = ep_BN)
  BNLayer_Axis1_preHidden <- tf$keras$layers$BatchNormalization(axis = 1L, center = T, scale = T, momentum = bn_momentum, epsilon = ep_BN)

  # projection layers
  for(hidden_ in 1:nHidden){
    #eval(parse(text = sprintf('HiddenProj%s <-tf$keras$layers$Dense(hiddenProjDims*(hidden_<nHidden)+nEmbed_chars*(hidden_==nHidden),activation = myActivation,name="HiddenProj%s")',hidden_,hidden_)))
    eval(parse(text = sprintf('HiddenProj%s <-tf$keras$layers$Dense(hiddenProjDims[hidden_],activation = myActivation,name="HiddenProj%s")',hidden_,hidden_)))
    eval(parse(text = sprintf('BNLayer_Axis1_hidden%s <-tf$keras$layers$BatchNormalization(axis = 1L, center = T, scale = T, momentum = bn_momentum, epsilon = ep_BN,name="BN_axis1_hidden%s")',hidden_,hidden_)))
  }
  DenseProj <-tf$keras$layers$Dense(1L,name="DenseProj")#bias_initializer = tf$keras$initializers$constant(value = -0.2))
  LinearProj <-tf$keras$layers$Dense(FinalAliasDim,name="LinearProj")
  SpaceEmbedding1 <- tf$Variable(tf$keras$initializers$random_uniform(minval = -0.01,maxval = 0.01)(list(1L,nEmbed_chars)), trainable=T,name="SpaceEmbed")
  WordCharPosProj <-tf$keras$layers$Dense(nEmbed_chars,name="WordCharPosProj",use_bias=F,
                                        kernel_initializer = tf$keras$initializers$random_uniform(minval = -0.01,maxval = 0.01))

  #tf$keras$layers$Bidirectional
  #lstm will not use cuDNN kernels since it doesn't meet the criteria.
  WORD_LSTM_BLOCK_SEQ <- tf$keras$layers$Bidirectional( tf$keras$layers$LSTM(units=nDim_lstm, activation = activation_lstm,return_sequences=T,dropout=LSTM_DROPOUT,recurrent_dropout=LSTM_DROPOUT) )
  WORD_LSTM_BLOCK_VALUE <- tf$keras$layers$Bidirectional( tf$keras$layers$LSTM(units=nDim_lstm, activation = activation_lstm,dropout=LSTM_DROPOUT,recurrent_dropout=LSTM_DROPOUT) )
  ALIAS_LSTM_BLOCK_SEQ <- tf$keras$layers$Bidirectional( tf$keras$layers$LSTM(units=nDim_lstm, activation = activation_lstm,return_sequences=T,dropout=LSTM_DROPOUT,recurrent_dropout=LSTM_DROPOUT) )
  ALIAS_LSTM_BLOCK_VALUE <-  tf$keras$layers$Bidirectional( tf$keras$layers$LSTM(units=nDim_lstm, activation = activation_lstm,dropout=LSTM_DROPOUT,recurrent_dropout=LSTM_DROPOUT) )
  LSTMFeedForward <- tf$keras$layers$Dense(units = nDim_lstm, activation = "swish")
  LSTMFeedForward_bottleneck <- tf$keras$layers$Dense(units = nDim_lstm, activation = "linear")
  Flatten <- tf$keras$layers$Flatten()
  #plot( predict(prcomp(as.matrix(WordCharPosProj$trainable_variables[[1]])))[,1]) ;points( predict(prcomp(as.matrix(WordCharPosProj$trainable_variables[[1]])))[,2],col="red")

  # normalization layers
  FinalBNLayer <-tf$keras$layers$BatchNormalization(axis = 1L, center = T, scale = T,
                        momentum = bn_momentum,epsilon = ep_BN,
                        beta_initializer = tf$initializers$constant(0.),
                        gamma_initializer = tf$initializers$constant(-0.2), name="FinalBNLayer")
}
trainingVars <- ls()[!ls() %fin% c(trainingVars,"trainingVars","ep_LN","l_","bn_momentum","RelPos_guide",
                                   "relCharsPosMat","h_","l_","NormType", "attention_axes","rm_")]

# define model architecture
{
  rzip_tf <- ( rzip<-function(l1,l2){  fl<-list(); for(aia in 1:length(l1)){ fl[[aia]] <- list(l1[[aia]], l2[[aia]]) }; return( fl  ) } )
  getRepresentation <- function(CINDICES, WINDICES, TRAINING){
    # get masks
    CEmbedMask <- tf$not_equal(CINDICES, BLANK_CODON_WORD_zi_tf)
    WEmbedMask <- tf$not_equal(WINDICES, neg1_tf)

    # get word embedding
    CEmbed <- CharEmbeddingLayer( CINDICES )
    # confirm is zero (next line)
    #sd(replicate(1000,{mean(as.array(CharEmbeddingLayer(CINDICES)))}))

    # LSTM approach
    if(T == T){
      #CEmbed <- CEmbed + getAttentionBlock_word(EMBEDDING = CEmbed,Mask1D = CEmbedMask, TRAINING = TRAINING)
      CEmbed <- CharsInput1_BNLayer( CEmbed ,training = TRAINING)
      #CEmbed <- WORD_LSTM_BLOCK_SEQ( CEmbed,mask = tf$greater(CINDICES,0L), training = TRAINING )
      #CEmbed <- CharsInput2_BNLayer(CEmbed ,training = TRAINING)
      WEmbed <- WORD_LSTM_BLOCK_VALUE( CEmbed, mask = CEmbedMask, training = TRAINING )
      #image2(as.matrix(CEmbed[1,,]))
      #image2(as.matrix(WEmbed))
      #image2(as.matrix(WEmbed[2,,]))
      WEmbed <- tf$gather(WEmbed, WINDICES, axis = zero_tf)

      WEmbed <- WordInput1_BNLayer(WEmbed,training = TRAINING)
      #WEmbed <- WEmbed + getAttentionBlock_alias(EMBEDDING = WEmbed, Mask1D = WEmbedMask, TRAINING = TRAINING)
      #WEmbed <- WordInput2_BNLayer(WEmbed,training = TRAINING)
      #WEmbed <- (  LSTMFeedForward(  WEmbed  ) )
      #WEmbed <- WordInput2_BNLayer(WEmbed,training = TRAINING)

      #WEmbed <- ALIAS_LSTM_BLOCK_SEQ(WEmbed, mask = WEmbedMask, training = TRAINING )
      #WEmbed <- WordInput3_BNLayer(WEmbed,training = TRAINING)
      xCt <- ALIAS_LSTM_BLOCK_VALUE(WEmbed, mask = WEmbedMask, training = TRAINING )
    }

    #name1[1:2]
    #CINDICES[1,]
    #as.matrix(CEmbed[1,,]);image2(as.matrix(CEmbed[1,,]))
    #as.matrix(WEmbed[1,,]);image2(as.matrix(WEmbed[1,,]))

    #image2(as.matrix(WEmbed[1,,]))
    #image2(as.matrix(xCt))

    if(T == T){
      xCt <- BNLayer_Axis1_preHidden(xCt,  training = TRAINING)

      # Hidden layers
      xCt0 <- xCt
      for(hidden_ in 1L:nHidden){
        xCtm1 <- xCt

        # Apply dropout in hidden layers
        if(hidden_ > 1){ xCt <- DenseDropout(xCt,training = TRAINING) }

        # Projection/nonlinearity + batchnorm
        eval(parse(text = sprintf('xCt <- HiddenProj%s( xCt )',hidden_)))
        eval(parse(text = sprintf('xCt <- BNLayer_Axis1_hidden%s( xCt ,training = TRAINING)',hidden_)))
        if(hidden_ > 1){xCt <- tf$add(xCt, xCtm1)}
      }

      # final linear projection
      xCt <- LinearProj( DenseDropout(xCt,training = TRAINING) )
    }

      # done with transformer, save representation
      return( xCt  )
  }

  getMatchProb <-   getMatchProb_base <- function(CINDICES,WINDICES, TRAINING ){

      # get representations
      AR_list <- getRepresentation(CINDICES = CINDICES, WINDICES = WINDICES, TRAINING = TRAINING)

      # perform convnet
      half_is <- tf$cast(tf$divide(tf$gather(tf$shape(WINDICES),tf$cast(zero_tf,tf$int32)),
                                   tf$cast(two_tf,tf$int32) ),tf$int32)
      half_range <- tf$range(half_is)

      matchProb <- tf$sqrt(tf$add(ep_SQRT, tf$reduce_mean(tf$square(tf$subtract(
                  tf$gather(AR_list, half_range, axis = zero_tf), # Ar1
                  tf$gather(AR_list, tf$add(half_is, half_range), axis = zero_tf) # Ar2
                  )),tf$cast(one_tf,tf$int32),keepdims=T)))
      matchProb <- tf$sigmoid( FinalBNLayer( matchProb, training = TRAINING ) )

    return(  matchProb  ) }

  #https://stackoverflow.com/questions/68815886/cannot-convert-a-symbolic-tensor-to-numpy-array-using-rtx-30xx-gpu
  kl_div = tf$keras$losses$KLDivergence()
  getLoss <-   function(CINDICES, WINDICES, TRUTH, TRAINING){
    predictedProbs <- getMatchProb(CINDICES = CINDICES,WINDICES=WINDICES,
                                   TRAINING = TRAINING)
    #label smoothing
    TRUTH = tf$add(tf$multiply(tf$subtract(one_tf, ep_LabelSmooth),TRUTH),
              tf$divide(ep_LabelSmooth,two_tf))
    y_i <- tf$expand_dims(TRUTH,tf$cast(one_tf,tf$int32));

    # kl divergence
    TRUTH <- tf$expand_dims(TRUTH,1L)
    minimizeThis <- tf$reduce_mean(
      tf$add(
           tf$multiply(y_i,
                       tf$subtract(tf$math$log( y_i ), tf$math$log( predictedProbs )  )),
           tf$multiply(tf$subtract(one_tf, y_i),
                       tf$subtract(tf$math$log( tf$subtract(one_tf, y_i) ), tf$math$log( tf$subtract(one_tf, predictedProbs) )  ))),0L)
    #y_i <- tf$concat(list(y_i, tf$subtract(one_tf, y_i)),tf$cast(one_tf,tf$int32))
    #predictedProbs <- tf$concat(list(predictedProbs, tf$subtract(one_tf, predictedProbs) ), tf$cast(one_tf,tf$int32))
    #minimizeThis <- kl_div(y_i, predictedProbs)

    # binary cross entropy loss
    #minimizeThis <- tf$reduce_mean(tf$keras$losses$binary_crossentropy(tf$expand_dims(TRUTH,1L), predictedProbs) )
    #y_i <- tf$expand_dims(TRUTH,1L); minimizeThis <- tf$negative(tf$reduce_mean( tf$add(  tf$multiply(y_i,tf$math$log(predictedProbs)),
    #tf$multiply(tf$subtract(one_tf,y_i), tf$math$log( tf$subtract(one_tf,predictedProbs) )))))

    # sum of absolute errors
    # minimizeThis <- tf$reduce_mean( tf$abs( tf$expand_dims(TRUTH,1L) - predictedProbs) )
    return( minimizeThis ) }

  # define loss
  trainStep <- (function(CINDICES, WINDICES, truth){
    with(tf$GradientTape() %as% tape, {
      myLoss_forGrad <- getLoss( CINDICES = tf$constant(CINDICES),
                                  WINDICES = tf$constant(WINDICES),
                                  TRUTH = tf$constant(truth),
                                  TRAINING = tf$constant(tf_true))
    })
    my_grads <- tape$gradient( myLoss_forGrad, trainingVars )
    if(T ==F){
      #my_grads_L2 <<- as.numeric(tf$linalg$global_norm(my_grads))
      my_grads_L2 <<- sqrt(0.001 +  sum(unlist( lapply(my_grads,
                                                       function(zer){
                                                         as.numeric(tf$math$reduce_sum(tf$square(zer)^2))
                                                       }) ) ) )
      if(my_grads_L2 > Inf){
        browser()
        tmp <- unlist(lapply(my_grads,function(zer){as.numeric(tf$reduce_max(tf$abs(zer)))}))
        plot( tmp )
      }
    }
    #if(is.na(mean(unlist(lapply(my_grads,function(zer){
    #mean(as.numeric(tf$reduce_mean(tf$abs(zer)))) }))))){ browser() }
    #unlist(lapply(my_grads,function(zer){mean(as.numeric(tf$reduce_mean(tf$abs(zer)))) }))
    optimizer_tf$apply_gradients( rzip(my_grads, trainingVars)  )

    # update learning rate
    optimizer_tf$learning_rate$assign(    tf$gather(sgd_learning_seq,as.integer(i-1L))   )
  })

  experimental_compile = F;experimental_relax_shapes = F
  #trainStep <- tf_function(trainStep,experimental_relax_shapes=experimental_relax_shapes,experimental_compile = experimental_compile)
  #getLoss <- tf_function(getLoss,experimental_relax_shapes=experimental_relax_shapes,experimental_compile = experimental_compile)
  #getMatchProb <- tf_function(getMatchProb,experimental_relax_shapes=experimental_relax_shapes,experimental_compile = experimental_compile)
  #getRepresentation <- tf_function(getRepresentation,experimental_relax_shapes=experimental_relax_shapes,experimental_compile = experimental_compile)
}

# some more helper functions
{
  getNameMatricesList <- function(name_vec){
    name_vec <- gsub(name_vec,pattern="  ",replace="")
    lapply(strsplit(name_vec,split=" "),function(zer){
      tmp_ <- name2char_indices(zer)
      if(ncol(tmp_) != max_nCharsPerWord){ browser()}
      if(nrow(tmp_) < max_nWords){
        tmp_ <- rbind(tmp_, matrix(nchars+1L,nrow = max_nWords-nrow(tmp_), ncol = ncol(tmp_)))
      }
      if(nrow(tmp_) > max_nWords){ tmp_ <- tmp_[1:max_nWords,] }
      return( tmp_ )
    })
  }
  getList <- function(name_raw, name_pushed = NULL){
    name_raw <- enc2utf8(name_raw)
    Encoding(name_raw) <- "UTF-8"

    name_raw_split <- strsplit(name_raw,split = " ")
    WINDICES <- sapply(1:length(name_raw_split),function(zer){
      prior_contrib <- 0 ; if(zer>1){
        prior_contrib <- length(unlist(name_raw_split[1:(zer-1L)]))
      }
      list ( 1:length(name_raw_split[[zer]]) + prior_contrib )
      })
    WINDICES <- do.call(rbind,lapply(WINDICES,function(zer){
      c(zer-1L,rep(-1L,max(0,max_nWordsPerAlias-length(zer))))[1:max_nWordsPerAlias]
    }))
    CINDICES <- name2char_indices( unlist(strsplit(name_raw,split=" ")) )

    return( list("CINDICES" = tf$constant( CINDICES, tf$int32),
                "WINDICES" = tf$constant( WINDICES, tf$int32) ) ) }
  getWordPosMat_zeroIndexed <- function(input){
    do.call(rbind,lapply(strsplit(input,split = " "),function(zer){
      stri_ <- unlist( sapply(1:length(zer), function(ze){
        tmp_ <- rep(ze+(ze-1),times=nchar(zerf[ze]))
        if(ze != length(zer)){tmp_ <- c(tmp_,ze+(ze-1)+1)}
        return(  tmp_ )   }))
      stri_ <- c(stri_,
                 rep(maxWordsInSeq+1, max(0,max_nCharsPerAlias-length(stri_))))[1:max_nCharsPerAlias]  - 1L # plus one to get the null vector
    }))
  } # 0 indexed

  # return name vec for use later
  #names_raw1 <- c("apple","google");names_raw2 <- c("google corps","apple inc")
  getProbMat <- function(names_raw1,
                         names_raw2,
                         embed1 = NULL,
                         embed2 = NULL,
                         training,
                         roundAt = 6L,
                         AcceptThreshold = NULL){
    # !!! NAME2 SHOULD BE LONGER VECTOR !!!
    # !!! embed2 SHOULD ALREADY BE TRANSPOSED
    #my_grid <- expand.grid(1:length(names_raw1), 1:length(names_raw2))  - 1L
    #newGridOrder <- sample(1:nrow(my_grid))
    #my_grid <- my_grid[newGridOrder,]

    # compile functions
    ##getRepresentation_tf <- tf_function(getRepresentation)
    ##ConvNet_tf <- tf_function(ConvNet)

    # first, get representations
    ProbMatchBatchSize <- 500
    print("Getting alias embeddings, names_raw1")
    if(is.null(embed1)){
    embed1 <- tapply(1:length(names_raw1),
                      cut(1:length(names_raw1),breaks=max(2,ceiling(length(names_raw1)/ProbMatchBatchSize))),
                      function(i_){
        if(runif(1)<0.1){ py_gc$collect() }
        l1_i <- getList(name_raw = enc2utf8( names_raw1[i_] ) )
        st_r <- as.matrix(getRepresentation(
          CINDICES = l1_i$CINDICES,
          WINDICES = l1_i$WINDICES,
          TRAINING = tf$constant(training,tf$bool)))
    })
    embed1 <- as.matrix( tf$constant( do.call(rbind,embed1) ,tf$float32) )
    }
    print("Getting alias embeddings, names_raw2")
    if(is.null(embed2)){
    embed2 <- tapply(1:length(names_raw2),
                 cut(1:length(names_raw2),breaks=max(2,ceiling(length(names_raw2)/ProbMatchBatchSize))),
                 function(i_){
                   if(runif(1)<0.1){ py_gc$collect() }
                   l2_i <- getList(name_raw = enc2utf8( names_raw2[i_] ) )
                   return( as.matrix(getRepresentation(
                     CINDICES = l2_i$CINDICES,
                     WINDICES = l2_i$WINDICES,
                     TRAINING = tf$constant(training,tf$bool))) )
    })
    embed2 <- t(as.matrix( tf$constant( do.call(rbind,embed2), tf$float32)))
    }
    if(all(dim(embed1)==dim(embed2))){embed2<-t(embed2)}

    print("Getting contrast between names_raw1 and names_raw2")
    match_ <- data.frame("my_entry"=NA, "alias_name"=NA,"stringdist"=NA, "canonical_id"= NA)[-1,]

    #probMat <- sapply(1:nrow(embed1), function(ier){
    # setup for comparisons
    library(foreach);cl <- doMC::registerDoMC(ncl<-parallel::detectCores());
    sigmoid <- function(x){1/(1+exp(-x))}
    moving_mean <- as.numeric(FinalBNLayer$moving_mean)
    moving_var <- as.numeric(FinalBNLayer$moving_variance)
    gamma_ <- as.numeric(FinalBNLayer$gamma)
    beta_ <- as.numeric(FinalBNLayer$beta)

    n_iters <- length(names_raw1)
    if(! "ReturnProgress" %in% ls()){ ReturnProgress <- T }
    probMat <- foreach(ier = 1:n_iters) %dopar% {
      if(ier %% 100 == 0 & ReturnProgress){write.csv(data.frame("Current Iters"=ier,"Total Iters"=n_iters),file='./PROGRESS_LINKIT_ml.csv')}
      # NOTE: AVOID using tf in parallel mode
      #matchProb_vec <- as.vector(as.matrix(tf$nn$sigmoid(FinalBNLayer(tf$constant(as.matrix(matchProb_vec),tf$float32),training = tf$constant(training,tf$bool) )) ))
      matchProb_vec <- colMeans( (embed1[ier,] - embed2)^2 )^0.5
      matchProb_vec <- round(sigmoid( ((matchProb_vec - moving_mean)/sqrt(ep_BN+moving_var))*gamma_ + beta_),roundAt)

      if(is.null(AcceptThreshold)){  match_indices <- 1:length(matchProb_vec) }
      if(!is.null(AcceptThreshold)){  match_indices <- which(matchProb_vec >= AcceptThreshold) }
      if(length(match_indices) > 0){
        match_ <- data.frame(
                      "x1" = names_raw1[ier],
                      "i1" = ier,
                      "x2" = names_raw2[match_indices],
                      "i2" = match_indices,
                      "matchprob"= (matchProb_vec[match_indices]),
                      "stringdist"= (1-matchProb_vec[match_indices]))
      }
      return( list(match_ ))
    }
    probMat <- unlist(probMat,recursive=F)
    probMat <- as.data.frame( do.call(rbind,probMat) )

    # return
    return(  probMat  )
  }
}

# initial forward pass to initialize variables + define trainable variables
{
#LIST1$CINDICES[1,];LIST1$WINDICES[1,]
name1 <- c("the apple company","cheese cake co","jpmorgan chase", "r","rstudio","bash","happy inc.")
name2 <- c("apple inc.","the apple corporation","jp morcan", "r","rstudio","bash","happy inc.")
LIST1 <- getList(name_raw = c(name1,name2))
#OUT <- LN_fxn(tf$cast(LIST1$relCharPos,tf$float32),tf$cast(tf$expand_dims(LIST1$MASK,2L),dtype=tf$float32))
#tmp <- tf$add(tf$multiply(OUT, Agammaed_2_3), Abetaed_2_3)
#apply(as.matrix(tmp[1,1:17,]),2,sd)
if(T == F){
  tmp2 <- getMatchProb_base( CINDICES=LIST1$CINDICES, WINDICES = LIST1$WINDICES,
                                TRAINING = tf$constant(T,dtype=tf$bool))
  probMat <- getProbMat(name1,name2,training=T)
}
for(TRAIN_ in c(F,T)){
  system.time( with(tf$GradientTape() %as% tape, {
    myLoss_forGrad <- getLoss( CINDICES=LIST1$CINDICES, WINDICES=LIST1$WINDICES,
                               TRUTH = tf$constant(rep(0,length(name1)),dtype=tf$float32),
                               TRAINING = tf$constant(TRAIN_,dtype=tf$bool))
  }))
}
if(T == F){
trainingVars <- unlist(  sapply(trainingVars, function(zer){
  test_ <- try(eval(parse(text = sprintf("%s$trainable",zer))),T)
  if(class(test_) == "try-error"){ret_ <- NULL}
  if(class(test_) != "try-error"){
    ret_ <- try(eval(parse(text = sprintf("%s$trainable_variables",zer))),T)
    if(class(ret_) != 'try-error'){ret_ <- sprintf("%s$trainable_variables",zer)}
    if(class(ret_) == 'try-error'){ret_ <-  sprintf("%s",zer)}
  }
  return(    ret_    )  }  ) )
trainingVars <-  eval(parse(text = paste("c(", paste(trainingVars,collapse=","),")",sep="") ) )
}
trainingVars <- tape$watched_variables()

# drop non-entering variables
with(tf$GradientTape() %as% tape, {
  myLoss_forGrad <- getLoss( CINDICES=LIST1$CINDICES,WINDICES=LIST1$WINDICES,
                             TRUTH = tf$constant(rep(0,length(name1)),dtype=tf$float32),
                             TRAINING = tf$constant(T,dtype=tf$bool))
})
my_grads <- tape$gradient( myLoss_forGrad, trainingVars )
trainingVars <- trainingVars[!unlist(lapply(my_grads,is.null))]
tmp <- unlist( lapply(my_grads,function(zer){
  as.numeric( tf$math$reduce_euclidean_norm(zer) )  }))
print("Summary, Initial Gradient Magnitudes")
summary( tmp )
}

# setup save model
{
  # get BN layers to reinstantiate moving mean/var states
  BN_layers <- grep(ls(),pattern="BNLayer",value=T)
}

#replicate(10,myLoss_forGrad <- getLoss( CINDICES=LIST1$CINDICES, WINDICES=LIST1$WINDICES,TRUTH = tf$constant(rep(0,length(name1)),dtype=tf$float32),TRAINING = tf$constant(T,dtype=tf$bool)))
}
