#!/usr/bin/env Rscript
#' TrainML
#'
#' Internal function that performs the training of the machine learning models used for organizational record linkage algorithms of Libgober and Jerzak.
#'
#' @usage
#'
#' TrainML(...)
#'
#'
#' @export
#'
#' @md


TrainML <- function(){
  # define optimizer and training step
  NA20 <- function(zer){zer[is.na(zer)] <- 0;zer[is.infinite(zer)] <- 0;zer}

  ## begin optimization
  # define sampling scheme
  nPosSplits <- floor(nPositiveExamples / (batch_size_posPool))
  nNegSplits <- floor(nNegativeExamples / batch_size_negPool)
  PosIterVec <- sample( 1:nPositiveExamples %% nPosSplits + 1 )
  NegIterVec <- sample( 1:nNegativeExamples %% nNegSplits + 1 )
  grad_l2_vec <- out_loss_vec <- in_loss_vec <- rep(NA,times=nSGD)

  # setup for adverarsial sgd
  {
    grid_forAdversarial <- expand.grid(1:batch_size_negPool, 1:batch_size_negPool)
    grid_forAdversarial <- grid_forAdversarial[grid_forAdversarial[,1] != grid_forAdversarial[,2],]
    grid_forAdversarial <- t(apply(grid_forAdversarial,1,function(zer){sort(zer)}))
    grid_forAdversarial <- grid_forAdversarial[!duplicated(apply(grid_forAdversarial,1,function(zer){
                                                paste(zer,collapse="_") })),]

    # load in link data
    load("./linkedIn_dataProcessedForML.Rdata")
    my_data <- my_data[,c("raw_names","to")]
    all_raw_names <- enc2utf8(my_data$raw_names)
    all_raw_to <- enc2utf8(my_data$to); rm( my_data )
    GetPrSameOGivenAiAj <- function(a_i,a_j=NULL,justIndicateIfNonzero=F,justReturnLinks=F){
      if(justReturnLinks == T){
        # reduce search pool once
        red_indices <- which(all_raw_names %fin% a_i)
        all_raw_names_ <- all_raw_names[red_indices]
        all_raw_to_ <- all_raw_to[red_indices]

        # then search within reduced pool
        to_list <- sapply(a_i, function(a_i_){ list(all_raw_to_[all_raw_names_ %fin% a_i_]) })

        # which(unlist(lapply(to_list,length)) == 0)
        if(any(unlist(lapply(to_list,length)) == 0)){browser()}
        return(  to_list  )
      }
      Mi <- my_data[all_raw_names %fin% a_i,]
      if(justIndicateIfNonzero == F){
        PrUGivenAi_list <- {
          to_names <- Mi$to
          to_ <- c(prop.table( Mi$weights ))
          names(to_) <- to_names
          list(  as.data.frame(t(to_)) )
        }
      }
      if(is.null(a_j)){ return(PrUGivenAi_list) }
      if(!is.null(a_j)){
        Mj <- my_data[all_raw_names == a_j,]
        if(justIndicateIfNonzero == F){
          PrUGivenAj_list <- {
            to_names <- Mj$to
            to_ <- c(prop.table( Mj$weights ))
            names(to_) <- to_names
            list(  as.data.frame(t(to_)) )
          }
          list_AiAj <- c(PrUGivenAi_list,PrUGivenAj_list)
          PrUGivenAiAj_list <- do.call(plyr::rbind.fill,list_AiAj)
          PrUGivenAiAj_list[is.na(PrUGivenAiAj_list)] <- 0

          MatchProb_AiAj <- sum(PrUGivenAiAj_list[1,] * PrUGivenAiAj_list[2,])
          return(  MatchProb_AiAj  )
        }
        if(justIndicateIfNonzero == T){
          return( NonZeroIndicator <- length(intersect(Mi$to, Mj$to)) )
        }
      }
    }
  }

  # print info
  print( tail(log(sort( sapply(ls(),function(x){object.size(get(x))})) )) )
  print(sprintf("Total Epochs, Pos Examples: %.4f",(batch_size/2*nSGD) / nPositiveExamples ))
  # read in matrices to disk using ff - fast loading by index after this
  print("Reading in data via ffdf")
  library(ff);NegMatches_mat <- read.csv.ffdf(file="./NegMatches_mat.csv")
  PosMatches_mat <- read.csv.ffdf(file="./PosMatches_mat.csv")
  print("Done reading in data via ffdf")
  print( "STARTING OPTIMIZATION")
  print( sprintf("Parameters # = %i", sum( unlist( lapply(trainingVars,function(er){ dims_ = dim(er); if(length(dims_)==0){dims_<-1};prod(dims_)}) ) )  ) )
  i_ <- 1; in_ <- ip_ <- 0; last_out <- c();
  #i_ <- in_ <- ip_ <- 300;last_out<-c()
  for(i in i_:nSGD){
    i_ <- i
    if( i %% 10 == 0){ py_gc$collect() }
    timer <- Sys.time()

    # define batch
    ip_ <- ip_ + 1 ; if(ip_ > nPosSplits){ip_ <- 1;PosIterVec <- sample( 1:nPositiveExamples %% nPosSplits + 1  )}
    in_ <- in_ + 1 ; if(in_ > nNegSplits){in_ <- 1;NegIterVec <- sample( 1:nNegativeExamples %% nNegSplits + 1  ) }

    # get pos/neg examples
    #PosMatches_mat_ <- as.data.frame( data.table::fread(cmd = paste0("sed -n '" , paste0(  c(1, (posIndices <- which(PosIterVec == ip_)) + 1) , collapse = "p\n " ) ,"p' ", "./PosMatches_mat.csv", collapse = "" )) )
    PosMatches_mat_ <- as.data.frame(ffdfindexget(PosMatches_mat, ff(which(PosIterVec == ip_)[1:batch_size_posPool])),stringsAsFactors=F)
    NegMatches_mat_ <- as.data.frame(ffdfindexget(NegMatches_mat, ff(which(NegIterVec == in_)[1:batch_size_negPool])),stringsAsFactors=F)

    # process names (encodings and extra "\s)
    PosMatches_mat_$name1 <- enc2utf8(as.character(PosMatches_mat_$name1)); PosMatches_mat_$name2 <- enc2utf8(as.character(PosMatches_mat_$name2))
    PosMatches_mat_$name1 <- gsub(PosMatches_mat_$name1,pattern="\\\"\\\"",replace="\\\"")
    PosMatches_mat_$name2 <- gsub(PosMatches_mat_$name2,pattern="\\\"\\\"",replace="\\\"")
    NegMatches_mat_$name1 <- enc2utf8(as.character(NegMatches_mat_$name1)); NegMatches_mat_$name2 <- enc2utf8(as.character(NegMatches_mat_$name2))
    NegMatches_mat_$name1 <- gsub(NegMatches_mat_$name1,pattern="\\\"\\\"",replace="\\\"")
    NegMatches_mat_$name2 <- gsub(NegMatches_mat_$name2,pattern="\\\"\\\"",replace="\\\"")

    # non-adversarial sampling
    #PosMatches_mat_ <- PosMatches_mat_[1:min(round(batch_size/2),nrow(PosMatches_mat_)),]
    if(T == T){
      PosMatches_mat_ <- PosMatches_mat_[which(PosMatches_mat_$name1!=PosMatches_mat_$name2),]
      PosMatches_mat_ <- PosMatches_mat_[1:min(batch_size_posPool_red,nrow(PosMatches_mat_)),]
      NegMatches_mat_ <- NegMatches_mat_[1:min(batch_size_posPool_red,nrow(NegMatches_mat_)),]

      #PosMatches_mat_ <- PosMatches_mat_[c(replicate(3,1:nrow(PosMatches_mat_)))[1:nrow(NegMatches_mat_)],]
      nNonAdversarial_pos <- min(nrow(PosMatches_mat_),round((batch_size/2)*(1-adv_frac)))
      nNonAdversarial_neg <- round((batch_size/2)*(1-adv_frac))
      nAdversarial_pos <- min(nrow(PosMatches_mat_),round((batch_size/2)*(adv_frac)))
      nAdversarial_neg <- round((batch_size/2)*(adv_frac))
      PosMatches_mat_nonAdv <- NegMatches_mat_nonAdv <- c()
      if(adv_frac < 1){
        PosMatches_mat_nonAdv <- PosMatches_mat_[1:nNonAdversarial_pos,]
        NegMatches_mat_nonAdv <- NegMatches_mat_[1:nNonAdversarial_neg,]
      }
    }

    if(adv_frac == 0){
      PosMatches_mat_ <- PosMatches_mat_nonAdv
      NegMatches_mat_ <- NegMatches_mat_nonAdv
      #sum( PosMatches_mat_$name1 == PosMatches_mat_$name2 )
    }
    #print(c(dim(PosMatches_mat_)))

    # adversarial selection
    if(T == T & adv_frac > 0){
      useBN_adver <- tf$constant(i < 50,tf$bool)
      # adversarial - positive examples - searching over fixed pairs
      if(T == T){
        list1 <- getList(name_raw = c(PosMatches_mat_$name1,PosMatches_mat_$name2))
        tmp_j <- as.matrix(   getMatchProb(CINDICES = tf$constant(list1$CINDICES),
                                           WINDICES = tf$constant(list1$WINDICES),
                                           TRAINING = tf$constant(useBN_adver) ) )
        # select lowest prob treated matches
        PosMatches_mat_ <- as.data.frame( PosMatches_mat_[order(tmp_j,decreasing = F)[1:round((batch_size/2)*adv_frac)],] )
        PosMatches_mat_ <- rbind(PosMatches_mat_,PosMatches_mat_nonAdv);
        rm(listNeg, tmp_j,PosMatches_mat_nonAdv,list1)
      }

      # adversarial - negative examples - searching over fixed pairs
      if(T == T){
        list1 <- getList(name_raw = c(NegMatches_mat_$name1,NegMatches_mat_$name2))
        tmp_j <- as.matrix(   getMatchProb(CINDICES = tf$constant(list1$CINDICES),
                                           WINDICES = tf$constant(list1$WINDICES),
                                         TRAINING = tf$constant(useBN_adver)  ) )
        # select highest prob neg matches
        NegMatches_mat_ <- as.data.frame( NegMatches_mat_[order(tmp_j,decreasing = T)[1:nAdversarial_pos],] )
        NegMatches_mat_ <- rbind(NegMatches_mat_, NegMatches_mat_nonAdv)
        rm(listNeg, tmp_j,NegMatches_mat_nonAdv,list1)
      }

      # define outcome
      if(any(sum(NegMatches_mat_$name1 == NegMatches_mat_$name2))){print("ZZZ");browser()}
    }

    # perform training step
    #tmp_<-getList(name_raw = (c(PosMatches_mat_$name2, NegMatches_mat_$name2)))
    names1_ <- c(PosMatches_mat_$name1, NegMatches_mat_$name1)
    names2_ <- c(PosMatches_mat_$name2, NegMatches_mat_$name2)
    l_ <- getList(name_raw = c(names1_,names2_))
    trainStep(CINDICES = tf$constant(l_$CINDICES),
              WINDICES = tf$constant(l_$WINDICES),
              truth = tf$constant(c(PosMatches_mat_$matchProb, NegMatches_mat_$matchProb),tf$float32))

    # update learning rate
    #in_loss_vec[i] <- as.numeric(myLoss_forGrad)
    #grad_l2_vec[i] <- my_grads_L2

    # some diagnostics every 100 iterations
    if((i == 1 | i %% 100 == 0) & T == T){
      #tf$random$set_seed(round(runif(1,0,10^10)))

      try(dev.off(),T)
      if(T == F){
      plot(grad_l2_vec)
      # drop if no entry
      #plot(unlist( lapply(lapply(trainingVars,as.array),function(zer){mean(abs(zer),na.rm=T)})))
      #trainingVars <- trainingVars[unlist(lapply(my_grads,length))>0]

      #plot( unlist(lapply(trainingVars,function(zer){as.numeric(tf$reduce_mean(tf$abs(zer)))})) )
      my_grads_f <-lapply(my_grads,function(zer){tf$reshape(zer,-1L) })
      names(my_grads_f) <- lapply(trainingVars,function(zer){zer$name})
      #tmp <- unlist( lapply(my_grads_f,as.numeric))
      #sqrt(sum(tmp^2))

      my_grads_f_l1 <- unlist(lapply(my_grads_f,function(zer){
        as.numeric(tf$reduce_mean(tf$abs(zer)))
      }))
      trainable_f_l1 <- unlist(lapply(trainingVars,function(zer){
        as.numeric(tf$reduce_mean(tf$abs(zer)))
      }))
      print( sort(my_grads_f_l1) )
      plot(sort(my_grads_f_l1), main= i)
      }

      # out of sample ml results
      if(i > 1){
        PosMatches_mat_hold <- as.data.frame( data.table::fread("./PosMatches_mat_hold.csv"))
        colnames(PosMatches_mat_hold)[colnames(PosMatches_mat_hold)=="to"] <- c("to1","to2")
        PosMatches_mat_hold <- PosMatches_mat_hold[sample(1:nrow(PosMatches_mat_hold),100),]

        # obtain overall results
        outTestTime <-system.time( ProbMat_out <- getProbMat(PosMatches_mat_hold$name1,
                                  PosMatches_mat_hold$name2,# training = T))
                                  training = tf$constant(F,tf$bool)) )
        per_time <- outTestTime/choose(nrow(PosMatches_mat_hold),2)
        #nrow(expand.grid(1:8592, 1:1000))
        out_time <- per_time * 8592000 / (60*60)  # hours (88hrs)
        print(sprintf("Estimated test time: %s",out_time[3]));
        bestMatch_ml <- tapply(1:nrow(ProbMat_out),ProbMat_out$i1,function(zer){
          red_ <- ProbMat_out[zer,]
          red_$i2[which.max(red_$matchprob)]  })
        truth_ <- cbind(PosMatches_mat_hold$name1,PosMatches_mat_hold$name2)
        PredMat <- (cbind(PosMatches_mat_hold$name1, PosMatches_mat_hold$name2[bestMatch_ml]))
        PredMat_correct <- PredMat[PosMatches_mat_hold$to1 == PosMatches_mat_hold$to2[bestMatch_ml],]
        PredMat_incorrect <- PredMat[PosMatches_mat_hold$to1 != PosMatches_mat_hold$to2[bestMatch_ml],]
        print( sprintf("Out Sample Accuracy: %.3f", out_loss_vec[i] <- last_out <- mean(PosMatches_mat_hold$to1 == PosMatches_mat_hold$to2[bestMatch_ml])))
        #View( PredMat_correct )
        #View( PredMat_incorrect )
        plot( out_loss_vec )
        #if(any(na.omit(out_loss_vec) < 0.5)){browser()}
        rm(VecsA,VecsB,PosMatches_mat_hold,my_grid)

        #tf_manager$save()
        # save parameters directly
        sapply(1:length(trainingVars),function(zer){
          saveArray <- as.array(trainingVars[[zer]])
        try(eval(parse(text = sprintf("state_ <-
            try(save(saveArray,
                file = './tf_saved_checkpoint_otherstates/TV_%s.Rdata'),T)",
                                      zer))),T)
        })

        # save other states
        sapply(BN_layers,function(zer){
          try(eval(parse(text = sprintf("
            write.csv(cbind('moving_mean'=as.numeric(%s$moving_mean),
                          'moving_variance'=as.numeric(%s$moving_variance)),
                    file = './tf_saved_checkpoint_otherstates/%s.csv')",
                    zer,zer,zer))),T)
        })
      }
    }

    # return more  diagnostics every 50 iterations
    if(i %% 10 == 0){
      try(plot(in_loss_vec),T); try(points(smooth.spline(na.omit(in_loss_vec)),type="l",col="red",lwd=5),T)
      #tuneR::setWavPlayer('/usr/bin/afplay'); s <- seewave::synth(f=8000,d=0.25,cf=1000*as.numeric(myLoss_forGrad),listen=T)

      print("Neg Examples:")
      print( head( cbind( substring(NegMatches_mat_[,c("name1")],0,20), substring(NegMatches_mat_[,c("name2")],0,20) ),4) )

      print("Pos Examples:")
      print( head( cbind( substring(PosMatches_mat_[,c("name1")],0,20), substring(PosMatches_mat_[,c("name2")],0,20) ),4) )

      if(any(PosMatches_mat_[,c("name1")] == PosMatches_mat_[,c("name2")])){browser()}

      # print out current state
      if(length(last_out) == 0){last_out<-0}
      print(sprintf("Iteration %i, Pos Epoch %.3f --- Current obj: %.3f --- Last o.o.s.a: %s --- GradNorm: %.3f",
                    i, i / length(unique(PosIterVec)),
                    as.numeric(myLoss_forGrad),
                    last_out,
                    grad_l2_vec[i] <- sqrt( sum( unlist(lapply(my_grads[ unlist(  lapply(my_grads, function(er){sum(dim(er))}) ) > 0] ,as.numeric)))^2)))

      # timer checkin
      print(  round(Sys.time() - timer,3)  )
    }
  }
}

