#!/usr/bin/env Rscript
#' LinkOrgs
#'
#' Implements the organizational record linkage algorithms of Libgober and Jerzak (2023+) using half-a-billion open-collaborated records.
#'
#' @param x,y data frames to be merged
#' @param by,by.x,by.y character vector(s) that specify the column names used for merging data frames `x` and `y`. The merging variables should be organizational names. See `?base::merge` for more details regarding syntax.
#' @param algorithm character; specifies which algorithm described in Libgober and Jerzak (2023+) should be used. Options are "`markov`", "`bipartite`", "`ml`", and "`transfer`". Default is "` ml`", which uses a machine-learning approach using Transformer netes and 9 million parameters to predict match probabilities using half a billion open-collaborated recoreds as training data.
#' @param ml_version character; specifies which version of the ML algorithm should be used. Options are of the form `"v1"`, `"v2"`, `"v3"`.... Highest version currently supported is `"v1"` (11M parameters).
#' @param conda_env character string; specifies a conda environment where JAX and related packages have been installed (see `?LinkOrgs::BuildBackend`). Used only when `algorithm='ml'` or `DistanceMeasure='ml'`.
#' @param conda_env_required Boolean; specifies whether conda environment is required.
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
#' @import reticulate
#' @md

LinkOrgs <- function(x,y,by=NULL, by.x = NULL,by.y=NULL,
                    algorithm = "ml",
                    conda_env = NULL, conda_env_required = F,
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
                    ml_version = "v0",
                   openBrowser = F,ReturnDecomposition = F){
  suppressPackageStartupMessages({
    library(plyr); library(dplyr);  library(data.table); library(fastmatch); library(stringdist); library(stringr)
} )
  `%fin%` <- function(x, table) {fmatch(x, table, nomatch = 0L) > 0L}

  # type checks
  DistanceMeasure <- tolower( as.character( DistanceMeasure ) )
  algorithm <- tolower( as.character( algorithm ) )
  if(algorithm == "ml"){ DistanceMeasure <- "ml" }
  if(algorithm == "transfer"){ DistanceMeasure <- "transfer" }

  redownload <- T; if(algorithm == "ml" | DistanceMeasure == "ml"){
      if(ml_version == "v0"){
        ModelURL <- "https://www.dropbox.com/scl/fi/au0zwc0kmfnzittzx9xso/AnalysisR_LinkOrgsBase_10PT8M_2024-01-27.zip?rlkey=0eex7mu51ggkfw00mqyzeraip&dl=0"
        WeightsURL <- "https://www.dropbox.com/scl/fi/shwdv8vp49yxuf3oixtwa/LinkOrgsBase_10PT8M_2024-01-27_ilast.eqx?rlkey=azr3jg3m01iawgx8wdatqcb4j&dl=0"
      }

      # process URLs
      ModelURL <- dropboxURL2downloadURL(ModelURL); WeightsURL <- dropboxURL2downloadURL(WeightsURL)

      # create temporary folder and dlownload items into it
      options(timeout = max(60*5, getOption("timeout")))
      download.file( ModelURL, destfile = (ModelZipLoc <- sprintf('%s/Model.zip', DownloadFolder <- tempdir())) )
      download.file( WeightsURL, destfile = (WeightsLoc <- sprintf('%s/ModelWeights.eqx', DownloadFolder)))

      # unzip model
      ModelLoc <- gsub(ModelZipLoc, pattern = "\\.zip", replace = "")
      unzip(ModelZipLoc, junkpaths = T, exdir = ModelLoc)

      # build model
      print2("Re-building ML model...")
      backend <- "METAL"; trainModel <- F; AnalysisName <- "LinkOrgs"
      source(sprintf('%s/LinkOrgs_Helpers.R',ModelLoc), local = T)
      source(sprintf('%s/JaxTransformer_Imports.R',ModelLoc), local = T)
      source(sprintf('%s/JaxTransformer_BuildML.R',ModelLoc), local = T)
      source(sprintf('%s/JaxTransformer_TrainDefine.R',ModelLoc), local = T)

      # obtain trained weights
      print2("Applying trained weights...")
      ModelList <- eq$tree_deserialise_leaves( WeightsLoc, list(ModelList, StateList, opt_state) )
      StateList <- ModelList[[2]]; ModelList <- ModelList[[1]]

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
    if("try-error" %in% class(transferCoefs)){
      transferCoefs <- t(as.matrix(data.table::fread("./Data/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv")[-1,2]))
    }

    # build transfer learning platform
    BuildTransferText <- gsub(deparse1(BuildTransfer,collapse="\n"), pattern="function \\(\\)",replace="")
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
      download.file( network_url, destfile = temp1 )
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
    assign( "directory_LinkIt", as.data.table(directory), envir = globalenv())
    rm( directory )
  }
  # print(  sort( sapply(ls(),function(x){object.size(get(x))}))  )

  # save original names for later merging
  by_x_orig = x[[by.x]]; by_y_orig = y[[by.y]]

  # process unique IDs
  x = cbind(1:nrow(x),x);colnames(x)[1] <- 'Xref__ID'
  y = cbind(1:nrow(y),y);colnames(y)[1] <- 'Yref__ID'
  names(by_x_orig) <- x$Xref__ID;names(by_y_orig) <- y$Yref__ID
  y$UniversalMatchCol <- x$UniversalMatchCol <- NA
  colnames_x_orig = colnames(x); colnames_y_orig = colnames(y)

  # preprocessing
  x = as.data.table(x); y = as.data.table(y)
  if(!is.null(by)){ by.x <- by.y <- by }
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
        pattern="\\s+", replace=' '))
    set(y,NULL,by.y,
            str_replace_all(
              y[[by.y]],
              pattern="\\s+", replace=' '))
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
    if(DistanceMeasure == "ml"){ linkedIn_embeddings_t <- linkedIn_embeddings_t[,keepAliases] }
    directory_LinkIt = directory_LinkIt[keepAliases,]

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

  # define ml-based fuzzy match
  print2("Searching for matches in the raw name space...")

  browser()
  z_fuzzy <- NULL; if(!algorithm %in% c("ml","transfer")){
    if(DistanceMeasure == "ml"){
      z_fuzzy <- try(pFuzzyMatch_euclidean(x = x, by.x = by.x, embedx = embedx,
                                    y = y, by.y = by.y, embedy = embedy,
                                    MaxDist = MaxDist,
                                    AveMatchNumberPerAlias = AveMatchNumberPerAlias), T)
    }
    if(!DistanceMeasure %in% c("ml","transfer") ){
      z_fuzzy <- try(as.data.frame(pFuzzyMatch( x = x, by.x = by.x,
                                                y = y, by.y = by.y,
                                                DistanceMeasure = DistanceMeasure,
                                                MaxDist = MaxDist,
                                                AveMatchNumberPerAlias = AveMatchNumberPerAlias,
                                                q = qgram)) ,T)
    }
    z_fuzzy <- try(z_fuzzy[,!colnames(z_fuzzy) %in% c("stringdist.x","stringdist.y")],T)
    colnames(z_fuzzy)[colnames(z_fuzzy) == "stringdist"] <- "stringdist_fuzzy"
  }

  # Get network matches
  {
    if(!algorithm %in% c("ml","transfer")){
      pFuzzyMatch_internal <- function(key_){
      if(key_ == "x"){ n_iters = nrow(x) };    if(key_ == "y"){ n_iters = nrow(y)}
      my_matched <- matrix(NA,nrow = 0, ncol = 4)
      colnames(my_matched) <- c("my_entry","alias_name","stringdist","canonical_id")
      my_matched_placeholder <- my_matched

      print2("Searching for network matches...")
      key_by_ref <- eval(parse(text = sprintf("by.%s",key_)))
      if(DistanceMeasure == "ml"){
        my_matched <- try(pFuzzyMatch_euclidean(
                x = eval(parse(text = sprintf("%s",key_))),
                by.x = key_by_ref,
                y = directory_LinkIt_red,
                by.y = "alias_name",
                embedy = linkedIn_embeddings_t,
                MaxDist = MaxDist_network,
                AveMatchNumberPerAlias = AveMatchNumberPerAlias_network), T)
        colnames(my_matched)[colnames(my_matched) == key_by_ref] <- "my_entry"
        my_matched <- my_matched[,c("my_entry","alias_name","stringdist","canonical_id")]
      }
      if(DistanceMeasure != "ml"){
        MaxDistThreshold_network_internal <- try(f2n(as.data.frame(pFuzzyMatch(
          x = eval(parse(text = sprintf("%s",key_))),
          by.x = key_by_ref,
          y = directory_LinkIt_red,
          by.y = "alias_name",
          DistanceMeasure = DistanceMeasure,
          MaxDist = MaxDist_network,
          AveMatchNumberPerAlias = AveMatchNumberPerAlias_network,
          q = qgram,
          ReturnMaxDistThreshold = T))) ,T)

        library("foreach"); library("doMC"); library("parallel")
        ncl <- 1; split_list <- list(1:n_iters)
        if(n_iters > 50){
          print2(sprintf("Using %s detected cores...", ncl = parallel::detectCores()))
          split_list = round(seq(0.5,n_iters,length.out = ncl+1))
          split_list = as.numeric(cut(1:n_iters, breaks=split_list))
          split_list = sapply(1:ncl, function(as){ list(which(split_list ==as))})
        }
        loop_ <- (foreach(outer_i = 1:(cl <- doMC::registerDoMC(ncl))) %dopar% {
          counter_ <- 0; my_matched_inner <- my_matched_placeholder
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
            dir_entries_tab = Rfast::Table(dir_tri_index[.(my_entry_trigrams), dir.row, nomatch = 0L])
            match_ <- directory_LinkIt_red[f2n(names(dir_entries_tab)),.(
              my_entry = my_entry,
              alias_name,
              stringdist = stringdist(my_entry, alias_name, method = DistanceMeasure, q = qgram),
              canonical_id)][ which(stringdist <= MaxDistThreshold_network_internal), ]

            if(nrow(match_) > 0){
              dist_ <- tapply(match_$stringdist,match_$canonical_id,min)
              match_ = match_[!duplicated(match_$canonical_id),]
              match_$stringdist <- dist_[ fastmatch::fmatch(match_$canonical_id,names(dist_)) ]
              my_matched_inner <- rbind(my_matched_inner,match_)
            }
          }
        colnames(my_matched_inner) <- c("my_entry", "alias_name", "stringdist", "canonical_id")
        return( my_matched_inner )
        })
        my_matched = do.call(rbind.fill,loop_)
      }

      print2("De-duplicating merge --- among duplicates to canonical_id, keeping closest match...")
      my_matched$duplication_check <- paste(my_matched[["my_entry"]], my_matched[["canonical_id"]], sep = "_")
      minDist_vec <- tapply(my_matched$stringdist,my_matched$duplication_check,min)
      my_matched <- my_matched[!duplicated(my_matched$duplication_check),]
      my_matched[["stringdist"]] <- minDist_vec[ fastmatch::fmatch(my_matched$duplication_check,names(minDist_vec)) ]

      print2(sprintf("Done deduplicating; final dimensions: %s...", nrow(my_matched)))
      gc(); return( my_matched )
      }
    }

    # perform match
    {
      if(!algorithm %in% c("ml","transfer")){
        xLinked <- pFuzzyMatch_internal(key_ = 'x')
        yLinked <- pFuzzyMatch_internal(key_ = 'y')

        print2("Joining network merged DFs...")
        xLinked = merge(as.matrix(x), as.matrix(xLinked), by.x=by.x, by.y="my_entry", all=F)
        yLinked = merge(as.matrix(y), as.matrix(yLinked), by.x=by.y, by.y="my_entry", all=F)
        xLinked$canonical_id <- f2n(xLinked$canonical_id); yLinked$canonical_id <- f2n(yLinked$canonical_id)
        z_linkIt <- as.data.frame(merge(xLinked, yLinked, by="canonical_id", all=F))
      }
      if(algorithm == "transfer" | algorithm == "ml"){
        if(algorithm == "ml"){
          embedx <- GetAliasRep_BigBatch( tolower(x[[by.x]]), nBatch_BigBatch = 50)
          embedy <- GetAliasRep_BigBatch( tolower(y[[by.y]]), nBatch_BigBatch = 50)
        }
        if(algorithm == "transfer"){
          print2("Matching via name representations from a LLM...")
          embedx <-  getRepresentation_transfer( x[[by.x]] )
          embedy <-  getRepresentation_transfer( y[[by.y]] )
        }
        z_linkIt <- try(pFuzzyMatch_euclidean(x = x, by.x = by.x,
                                               y = y, by.y = by.y,
                                               embedx = embedx,
                                               embedy = embedy,
                                               MaxDist = MaxDist,
                                               AveMatchNumberPerAlias = AveMatchNumberPerAlias) ,T)
      }
      }
  }

  colnames(z_linkIt)[colnames(z_linkIt) == "canonical_id"] <- "ID_MATCH"

  # bring in fuzzy matches
  {
  z_linkIt$XYref__ID <- paste(z_linkIt$Yref__ID,
                              z_linkIt$Xref__ID,sep="__LINKED__")

  if(algorithm == "transfer"){ z <- z_linkIt; z$stringdist_fuzzy <- NA }
  if(algorithm == "ml"){ z <- z_linkIt; z$stringdist_fuzzy <- NA }
  if(!algorithm %in% c("ml","transfer")){
    z_fuzzy$XYref__ID <- paste(z_fuzzy$Yref__ID,
                               z_fuzzy$Xref__ID,sep="__LINKED__")
    z = rbind.fill(z_fuzzy, z_linkIt)
  }

  print2("Checking for redundant name matches...")
  {
      tmp_ <- gsub( gsub(colnames(z),pattern="\\.x",replace=""), pattern="\\.y",replace="")
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

  #drop duplicates
  if(nrow(z)>1){
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
    print2("Dropping duplicates...")
    minDist_vec <- tapply(z$minDist, z$XYref__ID,min)
    z <-z[!duplicated(z$XYref__ID),]
    z$minDist <- minDist_vec[ fastmatch::fmatch(z$XYref__ID,names(minDist_vec)) ]
  }
  z  = z[,!colnames(z) %fin% c("ID_MATCH.x", "ID_MATCH.y")]

  if( ReturnDiagnostics == F ){
    z  = z[,colnames(z)[colnames(z) %fin% c(colnames(x ),colnames(y ))]]
    z = z[,!colnames(z) %fin% "UniversalMatchCol"]
  }

  #undo modifications to names for processing
  z[[by.y]] <- by_y_orig[f2n(z$Yref__ID)]
  z[[by.x]] <- by_x_orig[f2n(z$Xref__ID)]
  z  = z[,!colnames(z) %fin% c('Yref__ID', 'Xref__ID')]
  }

  print2("Returning matched dataset!")
  if(ReturnDecomposition == T){ z = list("z"=z,
                                         "z_fuzzy"=z_fuzzy,
                                         "z_linkIt"=z_linkIt)  }
  gc(); return(  z )
}
