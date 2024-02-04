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
  z_Network <- linkedIn_embeddings <- embedy <- embedy <- NULL
  DistanceMeasure <- tolower( as.character( DistanceMeasure ) )
  algorithm <- tolower( as.character( algorithm ) )
  DownloadFolder <- paste0(find.package("LinkOrgs"),"/data")
  if(algorithm == "ml"){ DistanceMeasure <- "ml" }
  if(algorithm == "transfer"){ DistanceMeasure <- "transfer" }
  if(algorithm == "ml" | DistanceMeasure == "ml"){
      # find.package("LinkOrgs")
      ModelLoc <- gsub(ModelZipLoc <- sprintf('%s/Model_%s.zip', DownloadFolder, ml_version ),
                       pattern = "\\.zip", replace = "")
      WeightsLoc <- sprintf('%s/ModelWeights_%s.eqx', DownloadFolder, ml_version)
      CharIndicatorsLoc <- sprintf('%s/CharIndicatorsLoc.csv', DownloadFolder)

      if( !file.exists(sprintf('%s/Model_%s.zip', DownloadFolder, ml_version )) |
          !file.exists(sprintf('%s/ModelWeights_%s.eqx', DownloadFolder, ml_version )) ){
        if(ml_version == "v0"){
          ModelURL <- "https://www.dropbox.com/scl/fi/e6a80y1ehi1b0i4lxd9ut/AnalysisR_LinkOrgsBase_30PT4M_2024-01-31.zip?rlkey=fjgxv95tq19mj9lh8qjh8wggu&dl=0"
          WeightsURL <- "https://www.dropbox.com/scl/fi/ovq0bejg5ym6f5z65r7ra/LinkOrgsBase_30PT4M_2024-01-31_ilast.eqx?rlkey=ai79dz7wntyfhdtf19qpag6p1&dl=0"
        }

        # process URLs
        ModelURL <- dropboxURL2downloadURL(ModelURL); WeightsURL <- dropboxURL2downloadURL(WeightsURL)

        # change timeout for long download of large-scale weight matrices
        options(timeout = max(60*5, getOption("timeout")))

        # download weights
        download.file( WeightsURL, destfile = WeightsLoc )

        # download and unzip model
        download.file( ModelURL, destfile = ModelZipLoc )
        unzip(ModelZipLoc, junkpaths = T, exdir = ModelLoc)

        # download characters & save
        charIndicators <- LinkOrgs::url2dt("https://www.dropbox.com/scl/fi/1jh8nrwsucfzj2gy9rydy/charIndicators.csv.zip?rlkey=wkhqk9x3550l364xbvnvnkoem&dl=0")
        data.table::fwrite(charIndicators, file = CharIndicatorsLoc)
      }

      # build model
      print2("Re-building ML model...")
      backend <- "METAL"; trainModel <- F; AnalysisName <- "LinkOrgs"
      charIndicators <- as.matrix(data.table::fread(file = CharIndicatorsLoc))
      source(sprintf('%s/LinkOrgs_Helpers.R', ModelLoc), local = T)
      source(sprintf('%s/JaxTransformer_Imports.R', ModelLoc), local = T)
      source(sprintf('%s/JaxTransformer_BuildML.R', ModelLoc), local = T)
      source(sprintf('%s/JaxTransformer_TrainDefine.R', ModelLoc), local = T)

      # obtain trained weights
      print2("Applying trained weights...")
      ModelList <- eq$tree_deserialise_leaves( WeightsLoc, list(ModelList, StateList, opt_state) )
      StateList <- ModelList[[2]]; ModelList <- ModelList[[1]]

      print2(sprintf("Matching via name representations from match-calibrated Transformer models [%s]...",ml_version))
      embedx <- GetAliasRep_BigBatch( tolower(x[[by.x]]), nBatch_BigBatch = 50)
      embedy <- GetAliasRep_BigBatch( tolower(y[[by.y]]), nBatch_BigBatch = 50)
      if( algorithm == "ml" | DistanceMeasure == "ml" ){ pFuzzyMatchNetworkFxn_touse <- pFuzzyMatchRawFxn_touse <- pFuzzyMatch_euclidean }

      if(!algorithm %in% c("ml", "transfer") & DistanceMeasure %in% c("ml","transfer") ){
        if(algorithm == "markov"){ EmbeddingsURL <- "https://www.dropbox.com/s/yo7t5vzkrzf91m6/Directory_LinkIt_markov_Embeddings.csv.zip?dl=0" }
        if(algorithm == "bipartite"){ EmbeddingsURL <- "https://www.dropbox.com/s/iqf9ids77dckopf/Directory_LinkIt_bipartite_Embeddings.csv.zip?dl=0" }

        if(!file.exists(sprintf("%s/Directory_LinkIt_%s_Embeddings.csv", DownloadFolder, algorithm))){
          linkedIn_embeddings <- as.matrix(url2dt( EmbeddingsURL ))
          data.table::fwrite(linkedIn_embeddings, file = sprintf("%s/Directory_LinkIt_%s_Embeddings.csv", DownloadFolder, algorithm))
        }
        linkedIn_embeddings <- as.matrix(fread(sprintf("%s/Directory_LinkIt_%s_Embeddings.csv", DownloadFolder, algorithm)))
     }
  }
  if(algorithm == "transfer" | DistanceMeasure == "transfer"){
    if( !file.exists(sprintf("%s/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv", DownloadFolder)) ){
      transferCoefs_url <- "https://www.dropbox.com/s/b2lvc4illml68w5/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv.zip?dl=0"
      transferCoefs <- try(t(as.matrix(url2dt( transferCoefs_url )[-1,2])),T)
      data.table::fwrite(transferCoefs,
              file = sprintf("%s/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv", DownloadFolder) )
    }

    # load in transfer learning coefficients
    transferCoefs <- data.table::fread(
      sprintf("%s/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv", DownloadFolder))

    # build transfer learning platform
    BuildTransferText <- gsub(deparse1(BuildTransfer,collapse="\n"), pattern="function \\(\\)",replace="")
    eval(parse(text = BuildTransferText))

    print2("Matching via name representations from a LLM...")
    embedx <-  getRepresentation_transfer( x[[by.x]] )
    embedy <-  getRepresentation_transfer( y[[by.y]] )
    if( algorithm == "transfer" | DistanceMeasure == "transfer" ){ pFuzzyMatchNetworkFxn_touse <- pFuzzyMatchRawFxn_touse <- pFuzzyMatch_euclidean }
  }

  if(algorithm %in% c("bipartite","network")){
      # see https://techapple.net/2014/04/trick-obtain-direct-download-links-dropbox-files-dropbox-direct-link-maker-tool-cloudlinker/
      if(algorithm == "bipartite"){ NetworkURL <- "https://dl.dropboxusercontent.com/s/tq675xfnnxjea4d/directory_data_bipartite_thresh40.zip?dl=0" }
      if(algorithm == "markov"){ NetworkURL <- "https://dl.dropboxusercontent.com/s/ftt6ts6zrlnjqxp/directory_data_markov.zip?dl=0" }
      if(!file.exists(sprintf("%s/LinkIt_directory_%s.Rdata", DownloadFolder, algorithm) ) ){
          DirectoryLoc <- gsub(DirectoryZipLoc <- sprintf('%s/DirectoryType%s.zip',
                                DownloadFolder, algorithm ), pattern = "\\.zip", replace = "")
          download.file( NetworkURL, destfile = DirectoryZipLoc )
          unzip(DirectoryZipLoc, junkpaths = T, exdir = DirectoryLoc)
      }
      load(sprintf("%s/LinkIt_directory_%s_trigrams.Rdata", DirectoryLoc, algorithm))
      load(sprintf("%s/LinkIt_directory_%s.Rdata", DirectoryLoc, algorithm) )

      assign("directory_trigrams", as.data.table(directory_trigrams), envir=globalenv())
      if(ToLower == T){ directory_trigrams$trigram <- tolower(directory_trigrams$trigram) }
      directory_trigrams = directory_trigrams[!duplicated(paste(directory_trigrams$trigram,
                                  directory_trigrams$alias_id,collapse="_")),]
      print( sprintf("Directory size: %i aliases",nrow( directory )  ))
      assign( "directory_LinkIt", as.data.table(directory), envir = globalenv())
      if( !DistanceMeasure %in% c("ml", "transfer" )){ pFuzzyMatchNetworkFxn_touse <- pFuzzyMatchRawFxn_touse <- pFuzzyMatch_discrete }
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
    for(it_ in c("x","y")){
      eval(parse(text = sprintf('set(%s,NULL,by.%s,tolower(%s[[by.%s]]))', it_, it_, it_, it_)))
    }

    # all methods use lower-casedtext
    if(algorithm %in% c("bipartite","markov")){
      directory_LinkIt[["alias_name"]] <- tolower(directory_LinkIt[["alias_name"]] )
    }
  }
  if(NormalizeSpaces == T){
    for(it_ in c("x","y")){
      eval(parse(text = sprintf('set(%s,NULL,by.%s, str_replace_all( %s[[by.%s]], pattern="\\\\s+", replace=" "))',
                    it_, it_, it_, it_)))
    }
    if(algorithm %in% c("bipartite","markov")){
      directory_LinkIt[["alias_name"]] <- str_replace_all(directory_LinkIt[["alias_name"]],pattern="\\s+", replace = " ")
    }
  }
  if(RemovePunctuation == T){
    for(it_ in c("x","y")){
      eval(parse(text = sprintf('set(%s,NULL,by.%s, str_replace_all(%s[[by.%s]],"\\\\p{P}",""))', it_, it_,it_, it_)))
    }
    if(algorithm %in% c("bipartite","markov")){
      directory_LinkIt[["alias_name"]] <- str_replace_all(directory_LinkIt[["alias_name"]],"\\p{P}","")
    }
  }

  #drop duplicates after pre-process
  if(!algorithm %in% c("ml", "transfer")){
    keepAliases <- which(  !duplicated(directory_LinkIt$alias_name) & trimws(directory_LinkIt$alias_name) != '' )
    if(DistanceMeasure == "ml"){ linkedIn_embeddings <- linkedIn_embeddings[keepAliases,] }
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

  # fuzzy match
  browser()
  z_RawNames <- try(as.data.frame(pFuzzyMatchRawFxn_touse(
                                  x = x, by.x = by.x, embedx = embedx,
                                  y = y, by.y = by.y, embedy = embedy,
                                  DistanceMeasure = DistanceMeasure,
                                  MaxDist = MaxDist,
                                  AveMatchNumberPerAlias = AveMatchNumberPerAlias,
                                  q = qgram)) ,T)

  # network match
  if(!algorithm %in% c("ml","transfer")){
    pFuzzyMatch_internal <- function(key_){
      n_iters <- ifelse(key_ == "x", yes = nrow(x), no = nrow(y) )
      my_matched_placeholder <- my_matched <- matrix(NA,nrow = 0, ncol = 4, dimnames = list(NULL,c("my_entry","alias_name","stringdist","canonical_id")))

      print2("Searching for network matches...")
      key_by_ref <- eval(parse(text = sprintf("by.%s",key_)))
      xory_by_ref <- eval(parse(text = sprintf("%s",key_)))
      MatchedIntoNetwork <- try(pFuzzyMatchNetworkFxn_touse(
                  x = xory_by_ref, by.x = key_by_ref, embedx = ifelse(key_ == 'x', yes = list(embedx), no = list(embedy))[[1]],
                  y = directory_LinkIt_red, by.y = "alias_name", embedy = linkedIn_embeddings,
                  MaxDist = MaxDist_network,
                  AveMatchNumberPerAlias = AveMatchNumberPerAlias_network), T)
      colnames(MatchedIntoNetwork)[colnames(MatchedIntoNetwork) == key_by_ref] <- "my_entry"
      MatchedIntoNetwork <- MatchedIntoNetwork[,c("my_entry","alias_name","stringdist","canonical_id")]

      print2("De-duplicating merge among duplicates to canonical_id [keeping closest match]...")
      MatchedIntoNetwork$duplication_check <- paste(MatchedIntoNetwork[["my_entry"]], MatchedIntoNetwork[["canonical_id"]], sep = "_")
      minDist_vec <- tapply(MatchedIntoNetwork$stringdist,MatchedIntoNetwork$duplication_check,min)
      MatchedIntoNetwork <- MatchedIntoNetwork[!duplicated(MatchedIntoNetwork$duplication_check),]
      MatchedIntoNetwork[["stringdist"]] <- minDist_vec[ fastmatch::fmatch(MatchedIntoNetwork$duplication_check,names(minDist_vec)) ]

      print2(sprintf("Done deduplicating; final dimensions: %s...", nrow(MatchedIntoNetwork)))
      gc(); return( MatchedIntoNetwork )
    }

    # perform match
    x2Network <- pFuzzyMatch_internal(key_ = 'x')
    y2Network <- pFuzzyMatch_internal(key_ = 'y')

    print2("Joining network merged DFs...")
    x2Network = merge(as.matrix(x), as.matrix(x2Network), by.x = by.x, by.y = "my_entry", all = F)
    y2Network = merge(as.matrix(y), as.matrix(y2Network), by.x = by.y, by.y = "my_entry", all = F)
    x2Network$canonical_id <- f2n(x2Network$canonical_id); y2Network$canonical_id <- f2n(y2Network$canonical_id)
    z_Network <- as.data.frame(merge(x2Network, y2Network, by="canonical_id", all=F))
    colnames(z_Network)[colnames(z_Network) == "canonical_id"] <- "ID_MATCH"
  }

  # bring in fuzzy matches
  {
  z_RawNames$XYref__ID <- paste( z_RawNames[[paste0(by.x,".x")]],
                                 z_RawNames[[paste0(by.y,".y")]],
                                 sep="__LINKED__" )
  z = rbind.fill(z_RawNames, z_Network)
  if(!ReturnDecomposition){ suppressWarnings( rm(z_RawNames, z_Network) ) }

  #drop duplicates
  if( nrow(z)>1 ){
    print2("Dropping duplicates...")

    if(!"stringdist.x" %in% colnames(z)){
      z$stringdist.x <- z$stringdist.y <- z$stringdist
    }
    z$stringdist.x <- f2n(z$stringdist.x)
    z$stringdist.y <- f2n(z$stringdist.y)

    # x bigger - set minDist to x
    x_bigger <- which( z$stringdist.x - z$stringdist.y >= 0);
    z$minDist <- NA; z$minDist[x_bigger] <- z$stringdist.x[x_bigger]

    # y bigger - set minDist to y
    z$minDist[-x_bigger] <- z$stringdist.y[-x_bigger]

    # these obs are non-overlapping, which is why it works
    z$minDist <- na20(z$minDist) + na20(z$stringdist.x)

    # get match with smallest match dist
    minDist_vec <- tapply(z$minDist, z$XYref__ID, min)
    z <-z[!duplicated(z$XYref__ID),]
    z$minDist <- minDist_vec[ fastmatch::fmatch(z$XYref__ID,names(minDist_vec)) ]
  }

  # Checking for redundant name matches
  {
    names_clean <- gsub( gsub(names_raw <- colnames(z),pattern="\\.x",replace=""),
                         pattern="\\.y",replace="")
    FracMatchAmongSharedCols <- tapply(1:ncol(z), names_clean, function(col_){
      if(length(col_) == 1){
        value_ <- NA
      }
      if(length(col_) == 2){
        value_ <- mean(z[,col_[1]] == z[,col_[2]], na.rm=T)
      }
      if(length(col_) == 3){
        value_ <- mean((z[,col_[1]] == z[,col_[2]]) & (z[,col_[1]] == z[,col_[3]]), na.rm=T)
      }
      return( value_ )
    })
    DropOneCopy <- na.omit(as.character(names(FracMatchAmongSharedCols[which(FracMatchAmongSharedCols  == 1)])))
    DropFlags <- na.omit(as.character(names(FracMatchAmongSharedCols[which( is.na(FracMatchAmongSharedCols) )])))
    KeepBothCopies <- na.omit(as.character(names(FracMatchAmongSharedCols[which(FracMatchAmongSharedCols  < 1)])))

    # drop flags if no collisions
    colnames(z)[colnames(z) %in% names_raw[names_clean %in% DropFlags] ] <- names_clean[names_clean %in% DropFlags]

    # drop flags if colliding with same output
    colnames(z)[colnames(z) %in% names_raw[names_clean %in% DropOneCopy] ] <- names_clean[names_clean %in% DropOneCopy]
    z <- z[,!duplicated(colnames(z))]
  }

  if( ReturnDiagnostics == F ){
    z  = z[,colnames(z)[colnames(z) %fin% c(colnames(x ),colnames(y ),
                            "stringdist", "stringdist.x", "stringdist.y")]]
    z = z[,!colnames(z) %fin% "UniversalMatchCol"]
  }

  #undo modifications to names for processing
  z[[by.y]] <- by_y_orig[f2n(z$Yref__ID)]
  z[[by.x]] <- by_x_orig[f2n(z$Xref__ID)]
  z  = z[,!colnames(z) %fin% c("ID_MATCH.x", "ID_MATCH.y",
                               'Yref__ID', 'Xref__ID')]
  }

  print2("Returning matched dataset!")
  if(ReturnDecomposition == T){ z = list("z"=z,
                                         "z_RawNames"=z_RawNames,
                                         "z_Network"=z_Networks)  }
  gc(); return(  z )
}
