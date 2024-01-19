#!/usr/bin/env Rscript
#' LinkOrgs
#'
#' Implements the organizational record linkage algorithms of Libgober and Jerzak (2023+) using half-a-billion open-collaborated records.
#'
#' @param x,y data frames to be merged
#' @param by,by.x,by.y character vector(s) that specify the column names used for merging data frames `x` and `y`. The merging variables should be organizational names. See `?base::merge` for more details regarding syntax.
#' @param algorithm character; specifies which algorithm described in Libgober and Jerzak (2023+) should be used. Options are "`markov`", "`bipartite`", "`ml`", and "`transfer`". Default is "` ml`", which uses a machine-learning approach using Transformer netes and 9 million parameters to predict match probabilities using half a billion open-collaborated recoreds as training data.
#' @param ml_version character; specifies which version of the ML algorithm should be used. Options are of the form `"v1"`, `"v2"`, `"v3"`.... Highest version currently supported is `"v1"` (11M parameters).
#' @param conda_env character string; specifies a conda environment where tensorflow and related packages have been installed. Used only when `algorithm='ml'` or `DistanceMeasure='ml'`.
#' @param ReturnDiagnostics logical; specifies whether various match-level diagnostics should be returned in the merged data frame.
#' @param ... For additional specification options, see
#'   ``Details''.
#'
#' @return `z` The merged data frame.
#' @export
#'
#' @details `LinkOrgs` automatically processes the name text for each dataset (specified by `by` or `by.x`, and `by.y`. Users may specify the following options:
#' - Set `DistanceMeasure` to control algorithm for computing pairwise string distances. Options include "`osa`", "`jaccard`", "`jw`". See `?stringdist::stringdist` for all options. Default is `"jaccard"`. To use the combined machine learning and network  methods, set `algorithm` to `"bipartite"` or `"markov"`, and `DistanceMeasure` to `"ml"`.
#' - Set `MaxDist` to control the maximum allowed distance between two matched strings
#' - Set `MaxDist_network` to control the maximum allowed distance between two matched strings in the integration with the LinkedIn network representation.
#' - Set `AveMatchNumberPerAlias` to control the maximum allowed distance between two matched strings. Takes priority over `MaxDist` if both specified.
#' - Set `AveMatchNumberPerAlias_network` to control the maximum allowed distance between two matched strings in the integration with the LinkedIn network representation. Takes priority over `MaxDist_network` if both specified.
#' - Set `qgram` to control the character-level q-grams used in the distance measure. Default is `2`.
#' - Set `RemoveCommonWords` to `TRUE` to remove common words (those appearing in >
#' 10% of aliases). Default is `FALSE`.
#' - Set `NormalizeSpaces` to `TRUE` to remove hanging whitespaces. Default is `TRUE`.
#' - Set `RemovePunctuation` to `TRUE` to remove punctuation. Default is `TRUE`.
#' - Set `ToLower` to `TRUE` to ignore case. Default is `TRUE`.
#'
#' @examples
#'
#' #Create synthetic data
#' x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
#' y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
#' x <- data.frame("orgnames_x"=x_orgnames)
#' y <- data.frame("orgnames_y"=y_orgnames)
#'
#' # Perform merge
#' linkedOrgs <- LinkOrgs(x = x,
#'                        y = y,
#'                        by.x = "orgnames_x",
#'                        by.y = "orgnames_y",
#'                        MaxDist = 0.6)
#'
#' print( linkedOrgs )
#'
#' @export
#'
#' @importFrom data.table ":="
#' @import Rfast
#' @import fastmatch
#' @import doMC
#' @import tensorflow
#' @import keras
#' @md

LinkOrgs <- function(x,y,by=NULL, by.x = NULL,by.y=NULL,
                    algorithm = "bipartite", conda_env = NULL,
                    ReturnDiagnostics = F, ReturnProgress = T,
                    ToLower = T,
                    NormalizeSpaces = T,
                    RemovePunctuation = T,
                    MaxDist = NULL,
                    MaxDist_network = NULL,
                    AveMatchNumberPerAlias = 10,
                    AveMatchNumberPerAlias_network = 2,
                    DistanceMeasure = "jaccard",
                    qgram = 2,
                   openBrowser = F,ReturnDecomposition = F){
  suppressPackageStartupMessages({
    library(plyr); library(dplyr)
    library(data.table)
    library(fastmatch)
    library(stringdist)
    library(stringr)
    f2n <- function(.){as.numeric(as.character(.))}
} )

  # type checks
  DistanceMeasure <- tolower( as.character( DistanceMeasure ) )
  algorithm <- tolower( as.character( algorithm ) )
  if(algorithm == "ml"){ DistanceMeasure <- "ml" }
  if(algorithm == "transfer"){ DistanceMeasure <- "transfer" }

  if(openBrowser == T){browser()}

  redownload <- T
  if(algorithm == "ml" | DistanceMeasure == "ml"){
      browser()

      # BUILD
      BuildText <- deparse1(BuildML,collapse="\n")
      BuildText <- gsub(BuildText, pattern="function \\(\\)",replace="")
      retrain <- F; eval(parse(text = BuildText))

      # RESTORE
      RestoreML <- deparse1(RestoreML,collapse="\n")
      RestoreML <- gsub(RestoreML,pattern="function \\(\\)",replace="")
      eval(parse(text = RestoreML))

      if(!algorithm %in% c("ml", "transfer") & DistanceMeasure %in% c("ml","transfer") ){
        if("Directory_LinkIt_markov_Embeddings.csv" %in% list.files("./") & !redownload){
          if(algorithm == "markov"){linkedIn_embeddings_t <- t(as.matrix(fread("./Directory_LinkIt_markov_Embeddings.csv"))) }
          if(algorithm == "bipartite"){linkedIn_embeddings_t <- t(as.matrix(fread("./Directory_LinkIt_bipartite_Embeddings.csv"))) }
        }
        if(!"Directory_LinkIt_markov_Embeddings.csv" %in% list.files("./") | redownload){
          if(algorithm == "markov"){
            embeddings_url <- "https://www.dropbox.com/s/yo7t5vzkrzf91m6/Directory_LinkIt_markov_Embeddings.csv.zip?dl=0"
          }
          if(algorithm == "bipartite"){
            embeddings_url <- "https://www.dropbox.com/s/iqf9ids77dckopf/Directory_LinkIt_bipartite_Embeddings.csv.zip?dl=0"
          }
          linkedIn_embeddings_t <- t(as.matrix(url2dt( embeddings_url )))
        }
      }
  }
  if(algorithm == "transfer" | DistanceMeasure == "transfer"){
    transferCoefs_url <- "https://www.dropbox.com/s/b2lvc4illml68w5/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv.zip?dl=0"
    transferCoefs <- try(t(as.matrix(url2dt( transferCoefs_url )[-1,2])),T)
    if(class(transferCoefs)=="try-error"){
      transferCoefs <- t(as.matrix(data.table::fread("./Data/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv")[-1,2]))
    }

    # BUILD
    BuildTransferText <- deparse1(BuildTransfer,collapse="\n")
    BuildTransferText <- gsub(BuildTransferText, pattern="function \\(\\)",replace="")
    eval(parse(text = BuildTransferText))
  }

  if("directory_LinkIt" %fin% ls(envir = globalenv()) & redownload){
    if(algorithm == "markov" & nrow(directory_LinkIt) != 264320 ){redownload <- T}
    if(algorithm == "bipartite" & nrow(directory_LinkIt) == 264320 ){redownload <- T}
  }
  if(redownload & !algorithm %in% c("ml","transfer")){
      RedownloadedIndicator <- F
      if(algorithm == "bipartite" & "directory_data_bipartite_thresh40" %in% list.files("./")){
        RedownloadedIndicator <- T
        load("./directory_data_bipartite_thresh40/LinkIt_directory_bipartite.Rdata")
        load("./directory_data_bipartite_thresh40/LinkIt_directory_bipartite_trigrams.Rdata")
      }
      if(algorithm == "markov" & "directory_data_markov" %in% list.files("./")){
        RedownloadedIndicator <- T
        load("./directory_data_markov/LinkIt_directory_markov.Rdata")
        load("./directory_data_markov/LinkIt_directory_markov_trigrams.Rdata")
      }

    if(!RedownloadedIndicator){
      # see https://techapple.net/2014/04/trick-obtain-direct-download-links-dropbox-files-dropbox-direct-link-maker-tool-cloudlinker/
      if(algorithm == "bipartite"){
        network_url <- "https://dl.dropboxusercontent.com/s/tq675xfnnxjea4d/directory_data_bipartite_thresh40.zip?dl=0"
      }
      if(algorithm == "markov"){
        network_url <- "https://dl.dropboxusercontent.com/s/ftt6ts6zrlnjqxp/directory_data_markov.zip?dl=0"
      }
      temp1 <- tempfile(pattern = "tmp14323512321423231960")
      download.file( network_url,destfile = temp1 )
      temp = unzip(temp1,junkpaths=T,exdir = "tmp14323512321423231960")
      load(temp[which(grepl(temp,pattern=sprintf("LinkIt_directory_%s_trigrams.Rdata",algorithm) ))[1]])
      load(temp[which(grepl(temp,pattern=sprintf("LinkIt_directory_%s.Rdata",algorithm) ))[1]])
      try(file.remove(temp),T)
    }
    assign("directory_trigrams", as.data.table(directory_trigrams), envir=globalenv())
    if(ToLower == T){ directory_trigrams$trigram <- tolower(directory_trigrams$trigram) }
    directory_trigrams = directory_trigrams[!duplicated(paste(directory_trigrams$trigram,
                 directory_trigrams$alias_id,collapse="_")),]
    print( sprintf("Directory size: %i aliases",nrow( directory )  ))
    assign( "directory_LinkIt", as.data.table(directory), envir=globalenv() )
    rm(directory)
  }
  #print(  sort( sapply(ls(),function(x){object.size(get(x))}))  )

  # save original names for later merging
  by_x_orig = x[[by.x]] ; by_y_orig = y[[by.y]]

  # process unique IDs
  x = cbind(1:nrow(x),x);colnames(x)[1] <- 'Xref__ID'
  y = cbind(1:nrow(y),y);colnames(y)[1] <- 'Yref__ID'
  names(by_x_orig) <- x$Xref__ID;names(by_y_orig) <- y$Yref__ID
  y$UniversalMatchCol <- x$UniversalMatchCol <- NA
  colnames_x_orig = colnames(x); colnames_y_orig = colnames(y)

  #PREPROCESSING
  x = as.data.table(x); y = as.data.table(y)
  if(!is.null(by)){by.x <- by.y <- by}
  if(ToLower == T){
    set(x,NULL,by.x,tolower(x[[by.x]]))
    set(y,NULL,by.y,tolower(y[[by.y]]))

    # all methods use lower-casedtext
    if(algorithm %in% c("bipartite","markov")){
      directory_LinkIt[["alias_name"]] <- tolower(directory_LinkIt[["alias_name"]] )
    }
  }
  if(NormalizeSpaces == T){
    set(x,NULL,by.x,
        str_replace_all(
        x[[by.x]],
        pattern="\\s+",
        replace=' '))
    set(y,NULL,by.y,
            str_replace_all(
              y[[by.y]],
              pattern="\\s+",
              replace=' '))
    if(algorithm %in% c("bipartite","markov")){
      directory_LinkIt[["alias_name"]] <- str_replace_all(directory_LinkIt[["alias_name"]],pattern="\\s+", replace = " ")
    }
  }
  if(RemovePunctuation == T){
    set(x,NULL,by.x,str_replace_all(x[[by.x]],"\\p{P}",""))
    set(y,NULL,by.y,str_replace_all(y[[by.y]],"\\p{P}",""))
    if(algorithm %in% c("bipartite","markov")){
      directory_LinkIt[["alias_name"]] <- str_replace_all(directory_LinkIt[["alias_name"]],"\\p{P}","")
    }
  }

  #drop duplicates after pre-process
  if(!algorithm %in% c("ml", "transfer")){
    keepAliases <- which(  !duplicated(directory_LinkIt$alias_name) & trimws(directory_LinkIt$alias_name) != '' )
    if(DistanceMeasure == "ml"){
      linkedIn_embeddings_t <- linkedIn_embeddings_t[,keepAliases]
    }
    directory_LinkIt = directory_LinkIt[keepAliases,]
    # mean(directory_LinkIt$alias_name != colnames(linkedIn_embeddings_t))

    #get trigrams
    directory_LinkIt_red <- directory_LinkIt[,c("alias_name","canonical_id")]
    dir_tri_index <- trigram_index(as.character(directory_LinkIt_red$alias_name),"dir.row")
    x_tri_index  <- trigram_index(x[[by.x]],"the.row")
    y_tri_index  <- trigram_index(y[[by.y]],'the.row')

    #drop components of the big corpus which don't share any trigrams with any entries in {x,y}
    tmp = unique(c(unique(as.character(x_tri_index[,trigram])),unique(as.character(y_tri_index[,trigram]))))
    dir_tri_index = dir_tri_index[trigram %fin% tmp,];rm(tmp);setkey(dir_tri_index, trigram)
  }

  #specify ID_match for the exact/fuzzy matching
  x$UniversalMatchCol <- as.character(x[[by.x]]); y$UniversalMatchCol = as.character( y[[by.y]] )

  #FAST MATCH --- DOESN'T WORK WITH NAs
  library(fastmatch)
  `%fin%` <- function(x, table) {fmatch(x, table, nomatch = 0L) > 0L}

  # define ml-based fuzzy match
  if(algorithm == "ml" | DistanceMeasure == "ml"){
    FastFuzzyMatch_ml <- function(x, by.x, embedx = NULL,
                                  y, by.y, embedy = NULL,
                                  MaxDist = NULL,
                                  AverageReference = NULL,
                                  AveMatchNumberPerAlias = NULL
                                  ){
      MIN_MATCH_PROB_ml <- MaxDist
      if(!is.null(AveMatchNumberPerAlias)){
        print("Calibrating via AveMatchNumberPerAlias...")
        cal_x_indices <- sample(1:length(x[[by.x]]), min(nrow(x),1000))
        cal_y_indices <- sample(1:length(y[[by.y]]), min(nrow(y),1000))
        MATCH_PROBS_ml <- as.data.frame( getProbMat(
          names_raw1 =  tolower(x[[by.x]])[cal_x_indices], embed1 = embedx[,cal_x_indices],
          names_raw2 =  tolower(y[[by.y]])[cal_y_indices], embed2 = embedy[,cal_y_indices],
          AcceptThreshold = NULL,
          training = F) )
        print(sprintf("Calibration resolution: %f",1/length(MATCH_PROBS_ml$matchprob)))
        #NumberToAcceptToGet_AveMatchNumberPerAlias <- max(c(length(cal_x_indices), length(cal_y_indices))) * AveMatchNumberPerAlias
        #ImpliedQuantile <- NumberToAcceptToGet_AveMatchNumberPerAlias / length(MATCH_PROBS_ml$matchprob)

        log_NPossibleMatches <- log(f2n(length(y[[by.y]]))) + log(f2n(length(x[[by.x]])))
        ReferenceNumb2 <- max(c(length(x[[by.x]]),length(y[[by.y]])))
        if(!is.null(AverageReference)){
          ReferenceNumb2 <- ifelse(AverageReference == "x",
                                   yes = length(x[[by.x]]),
                                   no = length(y[[by.y]]))
        }
        log_NObsTimesMatchesPerObs <- log(AveMatchNumberPerAlias) + log( ReferenceNumb2  )
        ImpliedQuantile <- exp(log_NObsTimesMatchesPerObs - log_NPossibleMatches)

        # negative here comes from fact that we compute the distances only, not the probabilities
        MIN_MATCH_PROB_ml <- try(mean(abs(quantile(-MATCH_PROBS_ml$matchprob,  probs = min(1,ImpliedQuantile) ))),T)
        if(class(MIN_MATCH_PROB_ml) == 'try-error'){
          MIN_MATCH_PROB_ml <- try(mean(abs(quantile(-MATCH_PROBS_ml$matchprob,  probs = min(1,ImpliedQuantile) ))),T)
        }
        MIN_MATCH_PROB_ml <- f2n(  MIN_MATCH_PROB_ml  )
        print("Calibrated accept match thres:")
        print( MIN_MATCH_PROB_ml )
      }

      #if(!is.null(embedy)){ browser() }
      z_ml_systime <- system.time(  z_ml <- as.data.frame( getProbMat(
        names_raw1 = tolower(x[[by.x]]),embed1 = embedx,
        names_raw2 = tolower(y[[by.y]]),embed2 = embedy,
        AcceptThreshold = MIN_MATCH_PROB_ml,
        training = F) )   )
      print( z_ml_systime )
      colnames(z_ml)[colnames(z_ml)=="x1"] <- by.x
      colnames(z_ml)[colnames(z_ml)=="x2"] <- by.y
      print( z_ml_systime  )
      #colnames(z_ml) <- c(by.x,by.y,"stringdist")
      #z_ml[,by.x] <- x[[by.x]][z_ml[,by.x]]
      #z_ml[,by.y] <- y[[by.y]][z_ml[,by.y]]

      print("dim(z_ml):"); print(dim(z_ml))

      z_ml$stringdist.x <- z_ml$stringdist.y <- z_ml$stringdist
      print("Joining DFs") # CHECK THE FOLLOWING
      z_ml <- merge(x = as.data.frame(x), y = z_ml, by = by.x,
                    all.x = F, all.y = T)
      z_ml <- merge(x = as.data.frame(z_ml), y = y, by = by.y,
                    all.x = T, all.y = F)

      gc()
      return(   as.data.frame( z_ml )     )
    }
  }

  # define transfer learning-based fuzzy match
  if(algorithm == "transfer" | DistanceMeasure == "transfer"){
    FastFuzzyMatch_transfer <- function(
                                  x, by.x, embedx_t,
                                  y, by.y, embedy_t,
                                  MaxDist = NULL,
                                  AverageReference = NULL,
                                  AveMatchNumberPerAlias = NULL
    ){
      MIN_MATCH_PROB_ml <- MaxDist
      if(!is.null(AveMatchNumberPerAlias)){
        print("Calibrating via AveMatchNumberPerAlias...")
        cal_x_indices <- sample(1:length(x[[by.x]]), min(nrow(x),1000))
        cal_y_indices <- sample(1:length(y[[by.y]]), min(nrow(y),1000))
        MATCH_PROBS_ml <- as.data.frame( getProbMat_transfer(
          names_raw1 =  tolower(x[[by.x]])[cal_x_indices], embed1 = embedx_t[,cal_x_indices],
          names_raw2 =  tolower(y[[by.y]])[cal_y_indices], embed2 = embedy_t[,cal_y_indices],
          AcceptThreshold = NULL,
          training = F) )
        print(sprintf("Calibration resolution: %f",1/length(MATCH_PROBS_ml$matchprob)))
        #NumberToAcceptToGet_AveMatchNumberPerAlias <- max(c(length(cal_x_indices), length(cal_y_indices))) * AveMatchNumberPerAlias
        #ImpliedQuantile <- NumberToAcceptToGet_AveMatchNumberPerAlias / length(MATCH_PROBS_ml$matchprob)

        log_NPossibleMatches <- log(f2n(length(y[[by.y]]))) + log(f2n(length(x[[by.x]])))
        ReferenceNumb2 <- max(c(length(x[[by.x]]),length(y[[by.y]])))
        if(!is.null(AverageReference)){
          ReferenceNumb2 <- ifelse(AverageReference == "x",
                                   yes = length(x[[by.x]]),
                                   no = length(y[[by.y]]))
        }
        log_NObsTimesMatchesPerObs <- log(AveMatchNumberPerAlias) + log( ReferenceNumb2  )
        ImpliedQuantile <- exp(log_NObsTimesMatchesPerObs - log_NPossibleMatches)

        # no neg sign here since we really do compute the probabilities
        MIN_MATCH_PROB_transfer <- try(mean( (quantile(MATCH_PROBS_ml$matchprob,  probs = min(1,ImpliedQuantile) ))),T)
        if(class(MIN_MATCH_PROB_ml) == 'try-error'){
          MIN_MATCH_PROB_transfer <- try(mean( (quantile(MATCH_PROBS_ml$matchprob,  probs = min(1,ImpliedQuantile) ))),T)
        }
        MIN_MATCH_PROB_transfer <- f2n(  MIN_MATCH_PROB_transfer  )
        print("Calibrated accept match thres:")
        print( MIN_MATCH_PROB_transfer )
      }

      #if(!is.null(embedy)){ browser() }
      z_transfer_systime <- system.time(  z_tr <- as.data.frame( getProbMat_transfer(
        names_raw1 = tolower(x[[by.x]]), embed1_t = embedx_t,
        names_raw2 = tolower(y[[by.y]]), embed2_t = embedy_t,
        AcceptThreshold = MIN_MATCH_PROB_transfer,
        training = F) )   )
      print( z_transfer_systime )
      colnames(z_tr)[colnames(z_tr)=="x1"] <- by.x
      colnames(z_tr)[colnames(z_tr)=="x2"] <- by.y
      #colnames(z_ml) <- c(by.x,by.y,"stringdist")
      #z_ml[,by.x] <- x[[by.x]][z_ml[,by.x]]
      #z_ml[,by.y] <- y[[by.y]][z_ml[,by.y]]

      print("dim(z_tr):"); print(dim(z_tr))

      z_tr$stringdist.x <- z_tr$stringdist.y <- z_tr$stringdist
      print("Joining DFs") # CHECK THE FOLLOWING
      z_tr <- merge(x = as.data.frame(x), y = z_tr, by = by.x,
                    all.x = F, all.y = T)
      z_tr <- merge(x = as.data.frame(z_tr), y = y, by = by.y,
                    all.x = T, all.y = F)

      gc()
      return(   as.data.frame( z_tr )     )
    }
  }

  ####################################################
  print("Searching for matches in the raw name space...")
  ####################################################
  z_fuzzy <- NULL; if(!algorithm %in% c("ml","transfer")){
    if(DistanceMeasure == "ml"){
      z_fuzzy <- try(FastFuzzyMatch_ml(x = x, by.x = by.x,
                                       y = y, by.y = by.y,
                                       MaxDist = MaxDist,
                                       AverageReference = "x",
                                       AveMatchNumberPerAlias = AveMatchNumberPerAlias) ,T)
      if(class( z_fuzzy ) == "try-error"){ browser() }
    }
    if(!DistanceMeasure %in% c("ml","transfer") ){
      z_fuzzy <- try(as.data.frame(FastFuzzyMatch(
                                              x = x,
                                              y = y,
                                              by.x = by.x,
                                              by.y = by.y,
                                              DistanceMeasure = DistanceMeasure,
                                              MaxDist = MaxDist,
                                              AveMatchNumberPerAlias = AveMatchNumberPerAlias,
                                              q = qgram)) ,T)
    }
    z_fuzzy <- try(z_fuzzy[,!colnames(z_fuzzy) %in% c("stringdist.x","stringdist.y")],T)
    if(class(z_fuzzy) == "try-error"){browser()}
    colnames(z_fuzzy)[colnames(z_fuzzy) == "stringdist"] <- "stringdist_fuzzy"
  }

  ####################################################
  # Get network matches
  ####################################################
  {
    if(!algorithm %in% c("ml","transfer")){
      # calibrate fuzzy match threshold
      FastFuzzyMatch_internal <- function(key_){
      if(key_ == "x"){ n_iters = nrow(x) };    if(key_ == "y"){ n_iters = nrow(y)}
      my_matched = ( matrix(NA,nrow = 0,ncol = 4) )
      colnames(my_matched) <- c("my_entry","alias_name","stringdist","canonical_id")
      my_matched_placeholder <- my_matched

      print("Searching for network matches...")
      key_by_ref <- eval(parse(text = sprintf("by.%s",key_)))
      if(DistanceMeasure == "ml"){
        my_matched <- try(FastFuzzyMatch_ml(
                x = eval(parse(text = sprintf("%s",key_))),
                by.x = key_by_ref,
                y = directory_LinkIt_red,
                by.y = "alias_name",
                embedy = linkedIn_embeddings_t,
                MaxDist = MaxDist_network,
                AverageReference = "x",
                AveMatchNumberPerAlias = AveMatchNumberPerAlias_network), T)
        if(class(my_matched) == "try-error"){browser()}
        colnames(my_matched)[colnames(my_matched) == key_by_ref] <- "my_entry"
        my_matched <- my_matched[,c("my_entry","alias_name","stringdist","canonical_id")]
        # head(my_matched[order(my_matched$stringdist),])
      }
      if(DistanceMeasure != "ml"){
        MaxDistThreshold_network_internal <- try(f2n(as.data.frame(FastFuzzyMatch(
          x = eval(parse(text = sprintf("%s",key_))),
          by.x = key_by_ref,
          y = directory_LinkIt_red,
          by.y = "alias_name",
          DistanceMeasure = DistanceMeasure,
          MaxDist = MaxDist_network,
          AverageReference = "x",
          AveMatchNumberPerAlias = AveMatchNumberPerAlias_network,
          q = qgram,
          ReturnMaxDistThreshold = T))) ,T)

        library("foreach"); library("doMC"); library("parallel")
        ncl <- 1; split_list <- list(1:n_iters)
        if(n_iters > 50){
          ncl = parallel::detectCores()
          print(sprintf("%s cores", ncl))
          split_list = round(seq(0.5,n_iters,length.out = ncl+1))
          split_list = as.numeric(cut(1:n_iters,breaks=split_list))
          split_list = sapply(1:ncl, function(as){ list(which(split_list ==as))})
        }
        gc()
        cl<-doMC::registerDoMC(ncl);
        loop_ <- (foreach(outer_i = 1:ncl) %dopar% {
        #loop_ <- sapply(1:ncl, function(outer_i){
          counter_ <- 0
          #my_matched_inner = matrix(NA,nrow = 0 ,ncol=4)
          #colnames(my_matched_inner) <- c("my_entry","alias_name","stringdist","canonical_id")# alias_name is match name
          my_matched_inner <- my_matched_placeholder

          for(i in split_list[[outer_i]]){
          counter_ = counter_ + 1
          if(i %% 100==0 & ReturnProgress){write.csv(data.frame("Current Split" = outer_i,
                                                                "Total Splits" = ncl,
                                                                "Current Iters in Split" = counter_,
                                                                "Total Iters in Split" = length(split_list[[outer_i]])),
                                                     file=sprintf('./PROGRESS_LINKIT_%s.csv',algorithm))}
          if(key_ == "x"){
            #get the name we want to fuzzy match against the directory_LinkIt
            my_entry = x[i,][[by.x]]

            #get the trigrams of the name i
            my_entry_trigrams = x_tri_index[the.row == i,trigram]
          }
          if(key_ == "y"){
            my_entry = y[i][[by.y]]
            my_entry_trigrams = y_tri_index[the.row==i,trigram]
          }

          #find the set of entries in directory_LinkIt_red that have some common trigram
          #https://appsilon.com/fast-data-lookups-in-r-dplyr-vs-data-table/
          dir_entries_tab = Rfast::Table(dir_tri_index[.(my_entry_trigrams),
                                                       dir.row, nomatch = 0L])
          #MinNumSharedTriGrams = ceiling(length(my_entry_trigrams)*0.05);dir_entries_tab <- dir_entries_tab[dir_entries_tab>=MinNumSharedTriGrams]
          # directory_LinkIt_red[f2n(names(dir_entries_tab)),]$alias_name

          match_ <- directory_LinkIt_red[f2n(names(dir_entries_tab)),.(
            my_entry = my_entry,
            alias_name,
            stringdist = stringdist(my_entry, alias_name, method = DistanceMeasure, q = qgram),
            canonical_id)][
              which(stringdist <= MaxDistThreshold_network_internal),
          ]
          #match_[which(match_$stringdist <= MaxDistThreshold_network_internal),]

          if(nrow(match_) > 0){
            dist_ <- tapply(match_$stringdist,match_$canonical_id,min)
            match_ = match_[!duplicated(match_$canonical_id),]
            #mean(match_$canonical_id == names(dist_[ fastmatch::fmatch(match_$canonical_id,names(dist_)) ] ))
            match_$stringdist <- dist_[ fastmatch::fmatch(match_$canonical_id,names(dist_)) ]
            my_matched_inner <- rbind(my_matched_inner,match_)
          }
          }
        colnames(my_matched_inner) <- c("my_entry", "alias_name", "stringdist", "canonical_id")
        return( my_matched_inner )
        })
        my_matched = do.call(rbind.fill,loop_)
        gc(full = T)
      }
      # deal with duplicate matches --- among duplicates, keep closest
      print("Deduplicating merge...")
      my_matched$duplication_check <- paste(my_matched[["my_entry"]], my_matched[["canonical_id"]], sep = "_")
      minDist_vec <- tapply(my_matched$stringdist,my_matched$duplication_check,min)
      #fastmatch::fmatch(c("a","b","c"),c("c","a","b", "a"))
      my_matched <- my_matched[!duplicated(my_matched$duplication_check),]
      #mean(my_matched$duplication_check == names(minDist_vec[ fastmatch::fmatch(my_matched$duplication_check,names(minDist_vec)) ]))
      my_matched[["stringdist"]] <- minDist_vec[ fastmatch::fmatch(my_matched$duplication_check,names(minDist_vec)) ]

      print("Done deduplicating...")
      print(sprintf("Final dimensions: %s",nrow(my_matched)))
      gc()
      return( my_matched )
      }
    }

    # perform match
    {
      if(!algorithm %in% c("ml","transfer")){
        xLinked <- FastFuzzyMatch_internal(key_ = 'x')
        yLinked <- FastFuzzyMatch_internal(key_ = 'y')
        print("Joining network merged DFs...")
        xLinked = merge(as.matrix(x),as.matrix(xLinked),by.x=by.x,by.y="my_entry",all=F)
        yLinked = merge(as.matrix(y),as.matrix(yLinked),by.x=by.y,by.y="my_entry",all=F)
        #f2n(xLinked$canonical_id) %in% f2n(yLinked$canonical_id)
        xLinked$canonical_id <- f2n(xLinked$canonical_id); yLinked$canonical_id <- f2n(yLinked$canonical_id)
        z_linkIt <- as.data.frame(merge(xLinked, yLinked,by="canonical_id",all=F))
        print("Done joining DFs")
      }
      if(algorithm == "ml"){
        z_linkIt <- try(FastFuzzyMatch_ml(x = x,
                                          by.x = by.x,
                                          y = y,
                                          by.y = by.y,
                                          MaxDist = MaxDist,
                                          AveMatchNumberPerAlias = AveMatchNumberPerAlias) ,T)
        if(class(z_linkIt)=="try-error"){browser()}
      }
      if(algorithm == "transfer"){
        print("Getting name representations from a LLM...")
        # rows correspond to columns for fast vectorization
        embedx_t <- t( getRepresentation_transfer( x[[by.x]]) )
        embedy_t <- t( getRepresentation_transfer( y[[by.y]]) )

        z_linkIt <- try(FastFuzzyMatch_transfer(x = x, by.x = by.x,
                                               y = y, by.y = by.y,
                                               embedx_t = embedx_t,
                                               embedy_t = embedy_t,
                                               MaxDist = MaxDist,
                                               AverageReference = "x",
                                               AveMatchNumberPerAlias = AveMatchNumberPerAlias) ,T)
        if(class(z_linkIt) == "try-error"){ browser() }
      }
      }
    }

  colnames(z_linkIt)[colnames(z_linkIt) == "canonical_id"] <- "ID_MATCH"

  # bring in fuzzy matches
  {
  z_linkIt$XYref__ID <- paste(z_linkIt$Yref__ID,
                              z_linkIt$Xref__ID,sep="__LINKED__")

  if(algorithm == "transfer"){
      z <- z_linkIt
      z$stringdist_fuzzy <- NA
  }
  if(algorithm == "ml"){
    z <- z_linkIt
    z$stringdist_fuzzy <- NA
  }
  if(!algorithm %in% c("ml","transfer")){
    z_fuzzy$XYref__ID <- paste(z_fuzzy$Yref__ID,
                               z_fuzzy$Xref__ID,sep="__LINKED__")
    z = rbind.fill(z_fuzzy,z_linkIt)
  }

  { # dead with redundant names
    print("Dealing with redundant names...")
    if(T == T){
      tmp_ <- gsub(colnames(z),pattern="\\.x",replace="")
      tmp_ <- gsub(tmp_,pattern="\\.y",replace="")
      tmp__ <- tapply(1:ncol(z),tmp_,function(ze){
        print(ze)
        value_ <- 0
        if(length(ze) == 2){
          value_ <- mean(z[,ze[1]] == z[,ze[2]],na.rm=T)
        }
        if(length(ze) == 3){
          value_ <- mean((z[,ze[1]] == z[,ze[2]]) &
                           (z[,ze[1]] == z[,ze[3]]),na.rm=T)
        }
        return( value_ )
      })
      rectify_names <- na.omit(as.character(names(tmp__[(tmp__  == 1)])))
      rectify_names <- rectify_names[rectify_names!="stringdist"]
      if(length(rectify_names) > 0){
        for(r_ in rectify_names){
          z[,r_] <- z[,(colnames(z)[tmp_ %in% r_])[1]]
          z <- z[,!(colnames(z) %in% paste(r_,".x",sep=""))]
          z <- z[,!(colnames(z) %in% paste(r_,".y",sep=""))]
        }
      }
    }
  }

  inf20 <- function(ze){ if(is.infinite(ze)){ze<-0};ze}
  na20 <- function(ze){ ze[is.na(ze)] <- 0;ze}

  #drop duplicates
  if(nrow(z)>1){
    print("Calculating distances")
    # minDist is either max(dist_x,dist_y) or dist_fuzzy
    z$stringdist.x <- f2n(z$stringdist.x)
    z$stringdist.y <- f2n(z$stringdist.y)
    z$stringdist_fuzzy <- f2n(z$stringdist_fuzzy)
    z$minDist <- NA; diff_ <- z$stringdist.x - z$stringdist.y

    # x bigger - set minDist to x
    x_bigger <- which(diff_ >= 0); z$minDist[x_bigger] <- z$stringdist.x[x_bigger]

    # y bigger - set minDist to y
    #plot(z$stringdist.x[-x_bigger],z$stringdist.y[-x_bigger]);abline(a=0,b=1)
    z$minDist[-x_bigger] <- z$stringdist.y[-x_bigger]

    # these obs are non-overlapping, which is why it works
    z$minDist <- na20(z$minDist) + na20(z$stringdist_fuzzy)

    # get match with smallest match dist
    print("Dropping duplicates...")
    minDist_vec <- tapply(z$minDist,z$XYref__ID,min)
    #z_ <- z
    z <-z[!duplicated(z$XYref__ID),]
    #mean(z$XYref__ID == names(minDist_vec[ fastmatch::fmatch(z$XYref__ID,names(minDist_vec)) ]))
    z$minDist <- minDist_vec[ fastmatch::fmatch(z$XYref__ID,names(minDist_vec)) ]

    # check to confirim code correct
    # head(names(minDist_vec)[ fastmatch::fmatch(z$XYref__ID,names(minDist_vec)) ]) == head(z$XYref__ID)
  }
  if(class(z) == "try-error"){browser()}
  z  = z[,!colnames(z) %fin% c("ID_MATCH.x", "ID_MATCH.y")]

  if(ReturnDiagnostics == F){
    z  = z[,colnames(z)[colnames(z) %fin% c(colnames(x ),colnames(y ))]]
    z = z[,!colnames(z) %fin% "UniversalMatchCol"]
  }

  #undo modifications to names for processing
  z[[by.y]] <- by_y_orig[f2n(z$Yref__ID)]
  z[[by.x]] <- by_x_orig[f2n(z$Xref__ID)]
  z  = z[,!colnames(z) %fin% c('Yref__ID', 'Xref__ID')]
  }

  print("Returning matched dataset!")
  return_ <- z
  if(ReturnDecomposition == T){ return_ = list("z"=z,"z_fuzzy"=z_fuzzy,"z_linkIt"=z_linkIt)  }
  gc()
  return(  return_ )
}

#' FastFuzzyMatch
#'
#' Performs fast fuzzy matching of strings based on the string distance measure specified in `DistanceMeasure`. Matching is parallelized using all available CPU cores to increase execution speed.
#'
#' @param x,y data frames to be merged
#'
#' @param by,by.x,by.y specifications of the columns used for merging. We follow the general syntax of `base::merge`; see `?base::merge` for more details.
#'
#' @param ... For additional options, see ``Details''.
#'
#' @return z The merged data frame.
#' @export
#'
#' @details
#' `FastFuzzyMatch` can automatically process the `by` text for each dataset. Users may specify the following options:
#'
#' - Set `DistanceMeasure` to control algorithm for computing pairwise string distances. Options include "`osa`", "`jaccard`", "`jw`". See `?stringdist::stringdist` for all options. (Default is "`jaccard`")
#'
#' - Set `MaxDist` to control the maximum allowed distance between two matched strings
#'
#' - Set `AveMatchNumberPerAlias` to control the maximum allowed distance between two matched strings. Takes priority over `MaxDist` if both specified.
#'
#' - Set `qgram` to control the character-level q-grams used in the distance measure. (Default is `2`)
#'
#' - Set `RemoveCommonWords` to TRUE to remove common words (those appearing in > 10% of aliases). (Default is `FALSE`)
#'
#' - Set `NormalizeSpaces` to TRUE to remove hanging whitespaces. (Default is `TRUE`)
#'
#' - Set `RemovePunctuation` to TRUE to remove punctuation. (Default is `TRUE`)
#'
#' - Set `ToLower` to TRUE to ignore case. (Default is `TRUE`)
#'
#' @examples
#'
#' #Create synthetic data
#' x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
#' y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
#' x <- data.frame("orgnames_x"=x_orgnames)
#' y <- data.frame("orgnames_y"=y_orgnames)
#' z <- data.frame("orgnames_x"=x_orgnames[1:2], "orgnames_y"=y_orgnames[1:2])
#' z_true <- data.frame("orgnames_x"=x_orgnames, "orgnames_y"=y_orgnames)
#'
#' # Perform merge
#' linkedOrgs_fuzzy <- FastFuzzyMatch(x = x,
#'                        y = y,
#'                        by.x = "orgnames_x",
#'                        by.y = "orgnames_y")
#'
#' @import stringdist
#' @import plyr
#' @import data.table
#'
#' @export
#' @md

FastFuzzyMatch <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, return_stringdist = T, onlyUFT = T,
                           qgram =2, DistanceMeasure = "jaccard", MaxDist = 0.20,
                           AverageReference = NULL,
                           AveMatchNumberPerAlias=NULL,openBrowser=F,ReturnProgress=T,
                           ReturnMaxDistThreshold = F){
  library(stringdist)
  f2n <- function(.){as.numeric(as.character(.))}
  if(openBrowser == T){browser()}

  #WARNING: X SHOULD ALWAYS BE THE LARGER SET
  if(is.null(by.x) & is.null(by.y)){by.x <- by.y <- by}
  if(nrow(y)>nrow(x)){
    x_old = x; y_old = y;by.y_old = by.y;by.x_old =by.x
    x <- y_old;by.x = by.y_old
    y <- x_old;by.y = by.x_old
    rm(x_old,y_old)
  }
  if(by.x == by.y){
    colnames(x)[colnames(x) == by.x] <- paste(by.x, ".x", sep = "")
    colnames(y)[colnames(y) == by.y] <- paste(by.y, ".y", sep = "")
    by.x = paste(by.x, ".x", sep = "");by.y = paste(by.y, ".y", sep = "")
  }
  y = as.data.table(y)
  x = as.data.table(x)
  x$by.x_ORIG <- x[[by.x]]
  y$by.y_ORIG <- y[[by.y]]
  x[[by.x]] <- tolower(x[[by.x]] )
  y[[by.y]] <- tolower(y[[by.y]] )
  x_tri_index = trigram_index(x[[by.x]],"the.row")
  y_tri_index = trigram_index(y[[by.y]],"the.row")
  n_iters = max(nrow(x), nrow(y))

  MIN_MATCH_DIST_fuzzy <- MaxDist
  if(!is.null(AveMatchNumberPerAlias)){
    MATCH_DIST_fuzzy <- replicate(nFuzzySamp <- 20000,
                            {  stringdist(sample(x[[by.x]],1), sample(y[[by.y]],1), method = DistanceMeasure, q = qgram) })

    log_NPossibleMatches <- log(f2n(length(y[[by.y]]))) + log(f2n(length(x[[by.x]])))
    ReferenceNumb2 <- max(c(length(x[[by.x]]),length(y[[by.y]])))
    if(!is.null(AverageReference)){
      ReferenceNumb2 <- ifelse(AverageReference == "x",
                               yes = length(x[[by.x]]),
                               no = length(y[[by.y]]))
    }
    log_NObsTimesMatchesPerObs <- log(AveMatchNumberPerAlias) + log( ReferenceNumb2  )
    ImpliedQuantile <- exp(log_NObsTimesMatchesPerObs - log_NPossibleMatches)

    #MIN_MATCH_DIST_fuzzy <- mean(extremeStat::distLquantile(MATCH_DIST_fuzzy, probs = ImpliedQuantile )[1:3,1])
    MIN_MATCH_DIST_fuzzy <- quantile(MATCH_DIST_fuzzy,
                                     probs = min(1,ImpliedQuantile))
    if(class(MIN_MATCH_DIST_fuzzy) == 'try-error' | is.na(MIN_MATCH_DIST_fuzzy)){
      MIN_MATCH_DIST_fuzzy <- quantile(MATCH_DIST_fuzzy,  probs = ImpliedQuantile)
    }
    MIN_MATCH_DIST_fuzzy <- f2n(  MIN_MATCH_DIST_fuzzy )
  }
  if(ReturnMaxDistThreshold){return( MIN_MATCH_DIST_fuzzy )}

  {
    library("foreach",quietly=T); library("doMC",quietly=T)
    ncl <- 1; split_list <- list(1:n_iters)
    if(n_iters>50){
      ncl = parallel::detectCores()
      split_list = round(seq(0.5,n_iters,length.out = ncl+1))
      split_list = as.numeric(cut(1:n_iters,breaks=split_list))
      split_list = sapply(1:ncl, function(as){ list(which(split_list ==as))})
    }
    f2n = function(.){as.numeric(as.character(.))}
    cl<-doMC::registerDoMC(ncl);
    loop_ <- foreach(outer_i = 1:ncl) %dopar% {
      counter_ <- 0
      my_matched_inner = matrix(NA,nrow = 0,ncol=3)
      colnames(my_matched_inner) <- c("my_entry",by.y,"stringdist")
      for(i in split_list[[outer_i]]){
        counter_ = counter_ + 1
        if(i %% 100==0 & ReturnProgress){write.csv(data.frame("Current Split"=outer_i,
                                                   "Total Splits"=ncl,
                                                   "Current Iters in Split"=counter_,
                                                   "Total Iters in Split"=length(split_list[[outer_i]])),
                                                   file='./PROGRESS_FUZZY.csv')}


        #get the name we want to fuzzy match against
        my_entry = x[i,][[by.x]]
        #get the trigrams of this name
        my_entry_trigrams = x_tri_index[the.row==i,trigram]

        #find the set of entries in directory_LinkIt_red that have some common trigram
        #LT_entries = unique(x_tri_index[trigram %fin% my_entry_trigrams,the.row])
        if(nrow(y)<1e5){
          LT_entries = 1:nrow(y)
        }
        if(nrow(y)>=1e5){
          MinNumSharedTriGrams = ceiling(length(my_entry_trigrams)*0.1)
          LT_entries = table(y_tri_index[trigram %fin% my_entry_trigrams,the.row])
          LT_entries = f2n(names(LT_entries[LT_entries>=MinNumSharedTriGrams]))
        }

        #calculate the nearest match accordfng to string distance
        match_ = sprintf("y[LT_entries,.(
                         my_entry=my_entry,%s,
                         stringdist = stringdist(my_entry,%s,method=DistanceMeasure,q = qgram))]",by.y,by.y)
        match_ = eval(parse(text=match_))
        if(nrow(match_)>0){
          #match_ = match_[,.(which(stringdist<=MaxDist)) ]
          match_ = as.data.frame(match_)
          match_ = match_[which(match_$stringdist <= MIN_MATCH_DIST_fuzzy ),]
          my_matched_inner = rbind(my_matched_inner,match_)
        }
      }
      return( my_matched_inner )
    }
    my_matched = do.call(rbind,loop_)
}
  colnames(my_matched)[1] <- by.x
  myMatched = merge(as.data.frame(x), as.data.frame(my_matched),
                    by.x=by.x,by.y=by.x,all.x = F,all.y=T)
  myMatched = merge(as.data.frame(y), as.data.frame(myMatched),
                    by.x=by.y,by.y=by.y,all.x = F,all.y=T)
  myMatched = as.data.frame( myMatched )

  myMatched = myMatched[!duplicated(paste(myMatched[[by.x]],
                                          myMatched[[by.y]],sep="__")),]

  # revert back to original names
  myMatched[[by.x]] <- myMatched[["by.x_ORIG"]]
  myMatched[[by.y]] <- myMatched[["by.y_ORIG"]]
  return( myMatched )
  }




#' AssessMatchPerformance
#'
#'  Computes the true/false positive and true/false negative rates of a candidate matching based on a ground-truth (preferably human-generated) matched dataset.
#'
#' @param x,y data frames to be merged
#'
#' @param by,by.x,by.y character strings specifying of the columns used for merging.
#'
#' @param z the merged data frame to be analyzed. Should contain `by`, `by.x`, and/or `by.y` as column names, depending on usage.
#'
#' @param z_true a reference data frame containing target/true matched dataset. Should contain `by`, `by.x`, and/or `by.y` as column names, depending on usage.
#'
#' @return `ResultsMatrix` A matrix containing the information on the true positive, false positive,
#' true negative, and false negative rate, in addition to the matched dataset size.  These quantities are calculated based off
#' all possible `nrow(x)*nrow(y)` candidate match pairs.
#'
#' @examples
#' # Create synthetic data
#' x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
#' y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
#' x <- data.frame("orgnames_x"=x_orgnames)
#' y <- data.frame("orgnames_y"=y_orgnames)
#' z <- data.frame("orgnames_x"=x_orgnames[1:2], "orgnames_y"=y_orgnames[1:2])
#' z_true <- data.frame("orgnames_x"=x_orgnames, "orgnames_y"=y_orgnames)
#'
#' # Obtain match performance data
#' PerformanceMatrix <- AssessMatchPerformance(x = x,
#'                                    y = y,
#'                                    z = z,
#'                                    z_true = z_true,
#'                                    by.x = "orgnames_x",
#'                                    by.y = "orgnames_y")
#' print( PerformanceMatrix )
#'
#'
#' @export
#' @md

AssessMatchPerformance = function(x, y, z, z_true, by, by.x=by, by.y=by, openBrowser=F){
  x[[by.x]] <- enc2utf8(x[[by.x]])

  y[[by.y]] <- enc2utf8(y[[by.y]])

  try(z[[by.x]] <- enc2utf8(z[[by.x]]),T)
  try(z[[by.y]] <- enc2utf8(z[[by.y]]),T)

  z_true[[by.x]] <- enc2utf8(z_true[[by.x]])
  z_true[[by.y]] <- enc2utf8(z_true[[by.y]])

  if(openBrowser==T){browser()}
  library(fastmatch)
  `%fin%` <- function(x, table) {fmatch(x, table, nomatch = 0L) > 0L}
  x <- as.matrix(x);y <- as.matrix(y);z <- as.matrix(z);z_true <- as.matrix(z_true);
  totalCombs <- length( unique(x[,by.x]) ) * length( unique(y[,by.y]) )
  ResultsMatrix =  c(matrix(0,nrow=1,ncol=5) )
  names(ResultsMatrix) <- c("TruePositives",
                            "FalsePositives",
                            "FalseNegatives",
                            "TrueNegatives",
                            "MatchedDatasetSize")

  #drop remaining duplicates
  dup_z_ <- duplicated(paste(z[,by.x],z[,by.y],sep= "___"))
  dup_zhuman_ <- duplicated(paste(z_true[,by.x],z_true[,by.y],sep= "___"))
  if(length(dup_z_) > 0 & nrow(z)>1){ z <- z[!dup_z_,] }
  if(length(dup_z_) > 0 & nrow(z_true)>1){ z_true <- z_true[!dup_zhuman_,]  }

  {
    z_vec = paste(z[,by.x],z[,by.y],sep="____LINKED____")
    z_true_vec <- paste(z_true[,by.x],z_true[,by.y],sep="____LINKED____")
    z_in_truth <- table(  z_vec %fin% z_true_vec )
    truth_in_z <- table(  z_true_vec %fin% z_vec )
    NA20 <- function(ze){ if(is.na(ze)){ze <- 0};ze}
    ResultsMatrix["TruePositives"] <- NA20(z_in_truth["TRUE"])
    ResultsMatrix["FalsePositives"] <- NA20(z_in_truth["FALSE"])
    ResultsMatrix["FalseNegatives"] <- NA20(truth_in_z["FALSE"])
    ResultsMatrix["TrueNegatives"] <- totalCombs -  ResultsMatrix["TruePositives"] - ResultsMatrix["FalsePositives"]
  }
  ResultsMatrix["MatchedDatasetSize"] <- nrow(z)
  gc()
  return( ResultsMatrix  )
}

trigram_index <- function(phrase,phrasename='phrase.no',openBrowser=F){
  # Internal function
  if(openBrowser==T){browser()}
  library(plyr)

  DT=data.table(phrase,phrase.no=1:length(phrase))
  t = DT[,.(phrase,phrase.no,phrase.length = nchar(phrase))][
    data.table(start_pos=1:100),
    .(phrase,phrase.no,start_pos),
    on="phrase.length>=start_pos",
    nomatch=0,allow.cartesian=T][
      order(phrase.no)]
  t[,end_pos := pmin(start_pos+2,nchar(phrase))]
  directory_trigrams= t[start_pos==1 | start_pos+2 == end_pos,
                        .(
                          trigram=substr(phrase,start_pos,end_pos),
                          phrase.no)]
  setkey(directory_trigrams,trigram)
  colnames(directory_trigrams) = c("trigram",phrasename)
  return(directory_trigrams)
}
