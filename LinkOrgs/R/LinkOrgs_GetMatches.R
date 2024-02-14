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
                    ml_version = "v1",
                   openBrowser = F,ReturnDecomposition = F){
  suppressPackageStartupMessages({
    library(plyr); library(dplyr);  library(data.table); library(fastmatch); library(stringdist); library(stringr)
} )
  `%fin%` <- function(x, table) {fmatch(x, table, nomatch = 0L) > 0L}

  # change timeout for long download of large-scale data objects
  options(timeout = max(60*10, getOption("timeout")))

  # type checks
  z_Network <- linkedIn_embeddings <- embedx <- embedy <- NULL
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

      if( !file.exists( ModelZipLoc ) | !file.exists( WeightsLoc) ){
        if(ml_version == "v0"){
          ModelURL <- "https://www.dropbox.com/scl/fi/1uz9pmw466kfnwwdrinpz/Archive.zip?rlkey=ia7d0nu8syixav8qlnug8gwpt&dl=0"
          WeightsURL <- "https://www.dropbox.com/scl/fi/7w0fc4vdw372a4jkkwpfp/ModelWeights_v0.eqx?rlkey=5rjppey7i4ymllne5gitxt80x&dl=0"
        }
        if(ml_version == "v1"){
          ModelURL <- "https://www.dropbox.com/scl/fi/4evf6d6t2804hpx4i4al3/AnalysisR_LinkOrgsBase_29PT9M_2024-02-12.zip?rlkey=08tet5hd1v178j9r6mvv447f8&dl=0"
          WeightsURL <- "https://www.dropbox.com/scl/fi/b07rk4gjnqmows5rwsgkd/LinkOrgsBase_29PT9M_2024-02-12_ilast.eqx?rlkey=t16hqkv2wwpivd28blak5opcz&dl=0"
        }

        # process URLs
        ModelURL <- dropboxURL2downloadURL(ModelURL); WeightsURL <- dropboxURL2downloadURL(WeightsURL)

        # download weights
        download.file( WeightsURL, destfile = WeightsLoc )

        # download and unzip model
        download.file( ModelURL, destfile = ModelZipLoc )
        unzip(ModelZipLoc, exdir = ModelLoc)

        # download characters & save
        charIndicators <- LinkOrgs::url2dt("https://www.dropbox.com/scl/fi/1jh8nrwsucfzj2gy9rydy/charIndicators.csv.zip?rlkey=wkhqk9x3550l364xbvnvnkoem&dl=0")
        data.table::fwrite(charIndicators, file = CharIndicatorsLoc)
      }

      # build model
      print2("Re-building ML model...")
      backend <- "METAL"; trainModel <- F; AnalysisName <- "LinkOrgs"
      charIndicators <- as.matrix(data.table::fread(file = CharIndicatorsLoc))
      source( sprintf('%s/Analysis/LinkOrgs_Helpers.R', ModelLoc), local = T )
      source( sprintf('%s/Analysis/JaxTransformer_Imports.R', ModelLoc), local = T )
      source( sprintf('%s/Analysis/JaxTransformer_BuildML.R', ModelLoc), local = T )
      source( sprintf('%s/Analysis/JaxTransformer_TrainDefine.R', ModelLoc), local = T )

      # obtain trained weights
      print2("Applying trained weights...")
      ModelList <- eq$tree_deserialise_leaves( WeightsLoc, list(ModelList, StateList, opt_state) )
      StateList <- ModelList[[2]]; ModelList <- ModelList[[1]]

      print2(sprintf("Matching via name representations from ML models [%s]...", ml_version))
      embedx <- NA2ColMean(
                    GetAliasRep_BigBatch(gsub(tolower(x[[by.x]]), pattern = "\\s+", replace =" "), # unnormalized spaces result in NA (so we normalize)
                                     nBatch_BigBatch = 50))
      embedy <- NA2ColMean(
                    GetAliasRep_BigBatch(gsub(tolower(y[[by.y]]), pattern = "\\s+", replace =" "),
                                     nBatch_BigBatch = 50))
      if( algorithm == "ml" | DistanceMeasure == "ml" ){ pFuzzyMatchNetworkFxn_touse <- pFuzzyMatchRawFxn_touse <- pFuzzyMatch_euclidean }
  }
  if(algorithm == "transfer" | DistanceMeasure == "transfer"){
    TransferModelLoc <- sprintf("%s/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv", DownloadFolder)
    if( !file.exists(TransferModelLoc) ){
      transferCoefs_url <- "https://www.dropbox.com/s/b2lvc4illml68w5/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv.zip?dl=0"
      transferCoefs <- try(t(as.matrix(url2dt( transferCoefs_url )[-1,2])),T)
      data.table::fwrite(transferCoefs, file = TransferModelLoc )
    }

    # load in transfer learning coefficients
    transferCoefs <- data.table::fread( TransferModelLoc )

    # build transfer learning platform
    BuildTransferText <- gsub(deparse1(BuildTransfer,collapse="\n"), pattern="function \\(\\)",replace="")
    eval(parse(text = BuildTransferText))

    print2("Matching via name representations from a LLM...")
    embedx <- getRepresentation_transfer( x[[by.x]] ); gc(); py_gc$collect()
    embedy <- getRepresentation_transfer( y[[by.y]] ); gc(); py_gc$collect()
    if( algorithm == "transfer" | DistanceMeasure == "transfer" ){ pFuzzyMatchNetworkFxn_touse <- pFuzzyMatchRawFxn_touse <- pFuzzyMatch_euclidean }
  }

  if(algorithm %in% c("markov", "bipartite")){
      if(DistanceMeasure %in% c("ml")){
        if(ml_version == "v0"){
          if(algorithm == "bipartite"){ EmbeddingsURL <- "https://www.dropbox.com/scl/fi/bnp5yxy7pgr6lqk5hd54n/Directory_LinkIt_bipartite_Embeddings_v0.csv.gz?rlkey=bvdzkkg544ujogzy82eyceezn&dl=0" }
          if(algorithm == "markov"){ EmbeddingsURL <- "https://www.dropbox.com/scl/fi/i8f5n93sxqw7jfyg6u8h5/Directory_LinkIt_markov_Embeddings_v0.csv.gz?rlkey=qxslvzxz0kn4n57mvoodadxif&dl=0" }
        }
        if(ml_version == "v1"){
          if(algorithm == "bipartite"){ EmbeddingsURL <- "https://www.dropbox.com/scl/fi/20j96htp1qr6hnvj721qc/Directory_LinkIt_bipartite_Embeddings_v1.csv.gz?rlkey=i9gekn7rmuhidvu6pysanta9b&dl=0" }
          if(algorithm == "markov"){ EmbeddingsURL <- "https://www.dropbox.com/scl/fi/zkl7x6yfr19nszlyak900/Directory_LinkIt_markov_Embeddings_v1.csv.gz?rlkey=506dspvnakihl9szp3kb02u6m&dl=0" }
        }

        EmbedddingsLoc <- sprintf("%s/Directory_LinkIt_%s_Embeddings_%s.csv.gz",
                                  DownloadFolder, algorithm, ml_version)
        if(!file.exists(EmbedddingsLoc)){
          download.file(LinkOrgs::dropboxURL2downloadURL(EmbeddingsURL), destfile = EmbedddingsLoc)
        }
        linkedIn_embeddings <- NA2ColMean(as.matrix(fread(EmbedddingsLoc))[,-1]); gc(); py_gc$collect()
        # print(  sort( sapply(ls(),function(x){object.size(get(x))}))  )
  } }

  if(algorithm %in% c("bipartite", "markov")){
      # see https://techapple.net/2014/04/trick-obtain-direct-download-links-dropbox-files-dropbox-direct-link-maker-tool-cloudlinker/
      if(algorithm == "bipartite"){ NetworkURL <- "https://dl.dropboxusercontent.com/s/tq675xfnnxjea4d/directory_data_bipartite_thresh40.zip?dl=0" }
      if(algorithm == "markov"){ NetworkURL <- "https://dl.dropboxusercontent.com/s/ftt6ts6zrlnjqxp/directory_data_markov.zip?dl=0" }
      DirectoryLoc <- gsub(DirectoryZipLoc <- sprintf('%s/Directory_%s.zip',
                                DownloadFolder, algorithm ), pattern = "\\.zip", replace = "")
      if(!dir.exists(sprintf("%s/Directory_%s/", DownloadFolder, algorithm) ) ){
          unzip(download.file( NetworkURL, destfile = DirectoryZipLoc ), exdir = DirectoryLoc)
      }
      load(sprintf("%s/%s/LinkIt_directory_%s_trigrams.Rdata",
                   DirectoryLoc,
                   ifelse(algorithm == "bipartite", yes = "directory_data_bipartite_thresh40", no = "directory_data_markov"),
                   algorithm))
      load(sprintf("%s/%s/LinkIt_directory_%s.Rdata",
                   DirectoryLoc,
                   ifelse(algorithm == "bipartite", yes = "directory_data_bipartite_thresh40", no = "directory_data_markov"),
                   algorithm) )

      if(ToLower == T){ directory_trigrams$trigram <- tolower(directory_trigrams$trigram) }
      directory_trigrams <- directory_trigrams[!duplicated(paste(directory_trigrams$trigram,
                                  directory_trigrams$alias_id,collapse="_")),]
      print2( sprintf("Directory size: %i aliases",nrow( directory )  )); gc()

      if( !algorithm %in% c("ml", "transfer" ) ){
      if( !DistanceMeasure %in% c("ml", "transfer" ) ){
        pFuzzyMatchNetworkFxn_touse <- pFuzzyMatchRawFxn_touse <- pFuzzyMatch_discrete
      } }
  }

  # check object sizes
  # print(  sort( sapply(ls(),function(x){object.size(get(x))}))  )

  # save original names for later merging
  by_x_orig = x[[by.x]]; by_y_orig = y[[by.y]]

  # process unique IDs
  if(!is.null(by)){ by.x <- by.y <- by }
  x = cbind(1:nrow(x),x);colnames(x)[1] <- 'Xref__ID'
  y = cbind(1:nrow(y),y);colnames(y)[1] <- 'Yref__ID'
  names(by_x_orig) <- x$Xref__ID;names(by_y_orig) <- y$Yref__ID
  y$UniversalMatchCol <- x$UniversalMatchCol <- NA
  colnames_x_orig = colnames(x); colnames_y_orig = colnames(y)

  # preprocessing
  x = as.data.table(x); y = as.data.table(y)
  if(ToLower == T){
    for(it_ in c("x","y")){
      eval(parse(text = sprintf('set(%s,NULL,by.%s,tolower(%s[[by.%s]]))', it_, it_, it_, it_)))
    }

    # all methods use lower-casedtext
    if(algorithm %in% c("bipartite","markov")){
      directory[["alias_name"]] <- tolower(directory[["alias_name"]] )
    }
  }
  if(NormalizeSpaces == T){
    for(it_ in c("x","y")){
      eval(parse(text = sprintf('set(%s,NULL,by.%s, str_replace_all( %s[[by.%s]], pattern="\\\\s+", replace=" "))', it_, it_, it_, it_)))
    }
    if(algorithm %in% c("bipartite","markov")){
      directory[["alias_name"]] <- str_replace_all(directory[["alias_name"]],pattern="\\s+", replace = " ")
    }
  }
  if(RemovePunctuation == T){
    for(it_ in c("x","y")){
      eval(parse(text = sprintf('set(%s,NULL,by.%s, str_replace_all(%s[[by.%s]],"\\\\p{P}",""))', it_, it_,it_, it_)))
    }
    if(algorithm %in% c("bipartite","markov")){
      directory[["alias_name"]] <- str_replace_all(directory[["alias_name"]],"\\p{P}","")
    }
  }

  #drop duplicates after pre-process
  if(!algorithm %in% c("ml", "transfer")){
    keepAliases <- which(  !duplicated(directory$alias_name) & trimws(directory$alias_name) != '' )
    if(DistanceMeasure == "ml"){ linkedIn_embeddings <- linkedIn_embeddings[keepAliases,] }
    directory = directory[keepAliases,]

    #get trigrams
    directory_red <- directory[,c("alias_name","canonical_id")]
    dir_tri_index <- trigram_index(as.character(directory_red$alias_name),"dir.row")
    x_tri_index  <- trigram_index(x[[by.x]],"the.row")
    y_tri_index  <- trigram_index(y[[by.y]],'the.row')

    #drop components of the big corpus which don't share any trigrams with any entries in {x,y}
    tmp = unique(c(unique(as.character(x_tri_index[,trigram])),unique(as.character(y_tri_index[,trigram]))))
    dir_tri_index = dir_tri_index[trigram %fin% tmp,];rm(tmp);setkey(dir_tri_index, trigram)
  }

  #specify ID_match for the exact/fuzzy matching
  x$UniversalMatchCol <- as.character(x[[by.x]]); y$UniversalMatchCol = as.character( y[[by.y]] )

  print2("Searching for matches in the raw name space...")
  z_RawNames <- as.data.frame(
                          DeconflictNames(
                                pFuzzyMatchRawFxn_touse(
                                  x = x, by.x = by.x, embedx = embedx,
                                  y = y, by.y = by.y, embedy = embedy,
                                  DistanceMeasure = DistanceMeasure,
                                  MaxDist = MaxDist,
                                  AveMatchNumberPerAlias = AveMatchNumberPerAlias,
                                  q = qgram)))

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
                  y = directory_red, by.y = "alias_name", embedy = linkedIn_embeddings,
                  MaxDist = MaxDist_network,
                  AveMatchNumberPerAlias = AveMatchNumberPerAlias_network), T)
      #MatchedIntoNetwork <- MatchedIntoNetwork[,c(paste0(key_by_ref,".x"),"alias_name.y","stringdist","canonical_id.y")]
      MatchedIntoNetwork <- DeconflictNames(MatchedIntoNetwork)

      print2("De-duplicating merge among duplicates to canonical_id [keeping closest match]...")
      MatchedIntoNetwork$duplication_check <- paste(MatchedIntoNetwork[[key_by_ref]],
                                                    MatchedIntoNetwork[["canonical_id"]], sep = "_")
      minDist_vec <- tapply(MatchedIntoNetwork$stringdist, MatchedIntoNetwork$duplication_check, min)
      MatchedIntoNetwork <- MatchedIntoNetwork[!duplicated(MatchedIntoNetwork$duplication_check),]
      MatchedIntoNetwork$stringdist <- minDist_vec[ fastmatch::fmatch(MatchedIntoNetwork$duplication_check,names(minDist_vec)) ]

      print2(sprintf("Done deduplicating; final dimensions: %s...", nrow(MatchedIntoNetwork)))
      gc(); return( MatchedIntoNetwork )
    }

    # perform match
    x2Network <- pFuzzyMatch_internal(key_ = 'x')
    y2Network <- pFuzzyMatch_internal(key_ = 'y')

    # merge into network
    x2Network <- DeconflictNames( merge(x = as.matrix(x), by.x = by.x,
                       y = as.matrix(x2Network), by.y = by.x, all = F) )
    colnames(x2Network)[colnames(x2Network) %in% 'stringdist'] <- "stringdist.x2network"

    y2Network <- DeconflictNames( merge(x = as.matrix(y), by.x = by.y,
                      y = as.matrix(y2Network), by.y = by.y, all = F) )
    colnames(y2Network)[colnames(y2Network) %in% 'stringdist'] <- "stringdist.y2network"

    x2Network$canonical_id <- f2n(x2Network$canonical_id);
    y2Network$canonical_id <- f2n(y2Network$canonical_id)

    # merge network into network
    z_Network <- as.data.frame(merge(x2Network, y2Network, by="canonical_id", all=F))
    colnames(z_Network)[colnames(z_Network) == "canonical_id"] <- "ID_MATCH"
  }

  # bring in fuzzy matches
  {
  z_RawNames$XYref__ID <- paste( z_RawNames$Xref__ID, z_RawNames$Yref__ID,
                                 sep="__LINKED__" )
  z_Network$XYref__ID <- paste( f2n(z_Network$Xref__ID), f2n(z_Network$Yref__ID),
                                 sep="__LINKED__" )
  z = rbind.fill(as.data.frame( z_RawNames ), as.data.frame( z_Network ) )
  if(!ReturnDecomposition){ suppressWarnings( rm(z_RawNames, z_Network) ) }

  #drop duplicates
  if( nrow(z)>1 ){
    print2("Dropping duplicates...")

    if(!"stringdist.y2network" %in% colnames(z)){
      z$stringdist.y2network <- z$stringdist.x2network <- z$stringdist
    }
    z$stringdist <- f2n( z$stringdist )
    z$stringdist.y2network <- f2n( z$stringdist.y2network )
    z$stringdist.x2network <- f2n( z$stringdist.x2network )

    # x bigger - set maxDist2Network to x
    z$maxDist2Network <- apply(cbind(z[,c("stringdist.y2network","stringdist.x2network")]),1,max)

    # NAs cast to 0, add to get full profile of min distances
    z$minDist <- na20(z$maxDist2Network) + na20(z$stringdist)

    # take min{  max{network}, string }
    minDist_vec <- tapply(z$minDist, z$XYref__ID, min)
    z <- z[!duplicated(z$XYref__ID),]
    z$minDist <- minDist_vec[ z$XYref__ID[fastmatch::fmatch(z$XYref__ID,names(minDist_vec))] ]
  }

  # Checking for redundant name matches
  z <- DeconflictNames(z)

  if( ReturnDiagnostics == F ){
    z  = z[,colnames(z)[colnames(z) %fin% c(colnames(x ),colnames(y ),
                          grep(colnames(z),pattern="stringdist",value=T), "minDist")]]
    z = z[,!colnames(z) %fin% "UniversalMatchCol"]
  }

  #undo modifications to names for processing
  z[[by.y]] <- by_y_orig[f2n(z$Yref__ID)]
  z[[by.x]] <- by_x_orig[f2n(z$Xref__ID)]
  z  = z[,!colnames(z) %fin% c("ID_MATCH.x", "ID_MATCH.y",
                               'Yref__ID', 'Xref__ID')]
  if(algorithm %in% c("ml", "transfer")){
    z <- z[,!colnames(z) %in% c("stringdist2network",
                                "stringdist.y2network", "stringdist.x2network")]
  }
  }

  print2("Returning matched dataset!")
  if(ReturnDecomposition == T){ z = list("z"=z,
                                         "z_RawNames"=z_RawNames,
                                         "z_Network"=z_Networks)  }
  gc(); return(  z )
}
