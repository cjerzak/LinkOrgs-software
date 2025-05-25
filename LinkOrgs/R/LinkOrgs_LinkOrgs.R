#!/usr/bin/env Rscript
#' LinkOrgs
#'
#' Implements the organizational record linkage algorithms of Libgober and Jerzak (2023+) using half-a-billion open-collaborated records.
#'
#' @param x,y data frames to be merged
#' @param by,by.x,by.y character vector(s) that specify the column names used for merging data frames `x` and `y`. The merging variables should be organizational names. See `?base::merge` for more details regarding syntax.
#' @param algorithm character; specifies which algorithm described in Libgober and Jerzak (2023+) should be used. Options are "`markov`", "`bipartite`", "`ml`", and "`transfer`". Default is "` ml`", which uses a machine-learning approach using Transformer netes and 9 million parameters to predict match probabilities using half a billion open-collaborated recoreds as training data.
#' @param ml_version character; specifies which version of the ML algorithm should be used. Options are of the form `"v0"` and `"v1"`. Highest version currently supported is `"v1"` (11M parameters).
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
#' @import doParallel
#' @import reticulate
#' @md

LinkOrgs <- function(x, y, by = NULL, by.x = NULL,by.y = NULL,
                    embedx = NULL, embedy = NULL, embedDistMetric = NULL, 
                    algorithm = "ml",
                    conda_env = "CondaEnv_LinkOrgs", conda_env_required = T,
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
                    RelThresNetwork = 1.5,
                    ml_version = "v1",
                    openBrowser = F,
                    ReturnDecomposition = F,
                    python_executable, 
                    nCores = NULL, 
                    deezyLoc = NULL){
  suppressPackageStartupMessages({
    library(plyr); library(dplyr);  library(data.table); library(fastmatch); library(stringdist); library(stringr)
} )
  `%fin%` <- function(x, table) {fmatch(x, table, nomatch = 0L) > 0L}
  if(DistanceMeasure != "ml"){ 
      suppressPackageStartupMessages({
        library("foreach",quietly=T); library("doParallel",quietly=T); 
      })
    if(is.null(nCores)){ 
      nCores <- ifelse(nrow(x)*nrow(y) > 500, 
                       yes = max(c(1L,parallel::detectCores() - 2L)), 
                       no = 1L)
    }
    doParallel::registerDoParallel(  cl <- parallel::makeCluster(  nCores  ) )
  }

  # change timeout for long download of large-scale data objects
  options(timeout = max(60*20, getOption("timeout")))

  # type checks
  if(!is.null(embedx)){ 
    pFuzzyMatchFxn_touse <- pFuzzyMatch_euclidean
    z_Network <- linkedIn_embeddings <- NULL
  }
  if(is.null(embedx)){ 
    z_Network <- linkedIn_embeddings <- embedx <- embedy <- NULL
    DistanceMeasure <- tolower( as.character( DistanceMeasure ) )
    algorithm <- tolower( as.character( algorithm ) )
    DownloadFolder <- paste0(find.package("LinkOrgs"),"/data")
    if(algorithm == "ml"){ DistanceMeasure <- "ml" }
    if(algorithm == "ml" | DistanceMeasure == "ml"){
        if(is.null(ml_version)){ ml_version <- "v4" }
        ModelLoc <- gsub(ModelZipLoc <- sprintf('%s/Model_%s.zip', DownloadFolder, ml_version ),
                         pattern = "\\.zip", replace = "")
        WeightsLoc <- sprintf('%s/ModelWeights_%s.eqx', DownloadFolder, ml_version)
        CharIndicatorsLoc <- sprintf('%s/CharIndicatorsLoc.csv', DownloadFolder)
  
        if( !file.exists( ModelZipLoc ) | !file.exists( WeightsLoc) ){
          if(ml_version == "v0"){
            #ModelURL <- "https://www.dropbox.com/scl/fi/1uz9pmw466kfnwwdrinpz/Archive.zip?rlkey=ia7d0nu8syixav8qlnug8gwpt&dl=0"
            #WeightsURL <- "https://www.dropbox.com/scl/fi/7w0fc4vdw372a4jkkwpfp/ModelWeights_v0.eqx?rlkey=5rjppey7i4ymllne5gitxt80x&dl=0"
            ModelURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/Archive_v0.zip"
            WeightsURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/ModelWeights_v0.eqx"
          }
          if(ml_version == "v1"){
            #ModelURL <- "https://www.dropbox.com/scl/fi/irzduuojtq13opz3hqpan/Analysis.zip?rlkey=i9nye1p3tkew00g50wpdwrx6x&dl=0"
            #WeightsURL <- "https://www.dropbox.com/scl/fi/5klzz36zjyhto4eef3vdi/LinkOrgsBase_29PT9M_2024-02-13_ilast.eqx?rlkey=icirb63feja9nv3kq6t5yow8p&dl=0"
            ModelURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/Analysis_v1.zip"
            WeightsURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/LinkOrgsBase_29PT9M_2024-02-13_ilast.eqx"
          }
          if(ml_version == "v2"){ # 2^8 word, 2^8.75 for alias, 10k iters
            #ModelURL <- "https://www.dropbox.com/scl/fi/3rc7u27k1mszc6j9qehqq/AnalysisR_LinkOrgsBase_22PT9M_2024-02-25.zip?rlkey=qp2qclwpcypb43vq43h3wujzs&dl=0"
            #WeightsURL <- "https://www.dropbox.com/scl/fi/467ccsay1cos3baqiyy0k/LinkOrgsBase_22PT9M_2024-02-25_ilast.eqx?rlkey=5nyc0hovvmfj06srmc43mw2w6&dl=0"
            ModelURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/AnalysisR_LinkOrgsBase_22PT9M_2024-02-25.zip"
            WeightsURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/LinkOrgsBase_22PT9M_2024-02-25_ilast.eqx"
          }
          if(ml_version == "v3"){ # 2^8 word, 2^9 for alias, 7k iters
            #ModelURL <- "https://www.dropbox.com/scl/fi/0lzgl5nyqyuebi94n13gy/AnalysisR_LinkOrgsBase_31PT3M_2024-02-26.zip?rlkey=svcuc8z02fg0hh2tn00s1jn9m&dl=0"
            #WeightsURL <- "https://www.dropbox.com/scl/fi/br8qs5w6nhl1ujgsot4wk/LinkOrgsBase_31PT3M_2024-02-26_ilast.eqx?rlkey=cdc8ao6zfkte4tk0dl0z5pnaw&dl=0"
            ModelURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/AnalysisR_LinkOrgsBase_31PT3M_2024-02-26.zip"
            WeightsURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/LinkOrgsBase_31PT3M_2024-02-26_ilast.eqx"
          }
          if(ml_version == "v4"){ # 2^8 word, 2^9 for alias, 14k iters
            #ModelURL <- "https://www.dropbox.com/scl/fi/2mi3v3e88cao9dzw2mns6/AnalysisR_LinkOrgsBase_17PT3M_2024-02-29.zip?rlkey=85d1xgm24t0u8j3l4wl321i4f&dl=0"
            #WeightsURL <- "https://www.dropbox.com/scl/fi/zr4bziggj3nugrpovkxrm/LinkOrgsBase_17PT3M_2024-02-29_ilast.eqx?rlkey=b6f7i8dhuro62hlszm365vofi&dl=0"
            ModelURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/AnalysisR_LinkOrgsBase_17PT3M_2024-02-29.zip"
            WeightsURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/LinkOrgsBase_17PT3M_2024-02-29_ilast.eqx"
          }
  
          # process URLs if using dropbox URLs
          #ModelURL <- dropboxURL2downloadURL(ModelURL);WeightsURL <- dropboxURL2downloadURL(WeightsURL)
  
          # download weights
          download.file( WeightsURL, destfile = WeightsLoc )
  
          # download and unzip model
          download.file( ModelURL, destfile = ModelZipLoc )
          unzip(ModelZipLoc, exdir = ModelLoc)
  
          # download characters & save
          #charIndicators <- LinkOrgs::url2dt("https://www.dropbox.com/scl/fi/1jh8nrwsucfzj2gy9rydy/charIndicators.csv.zip?rlkey=wkhqk9x3550l364xbvnvnkoem&dl=0")
          charIndicators <- LinkOrgs::url2dt("https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/charIndicators.csv.zip")
          data.table::fwrite(charIndicators, file = CharIndicatorsLoc)
        }
  
        # build model
        print2("Re-building ML model...")
        trainModel <- F; AnalysisName <- "LinkOrgs"
        charIndicators <- as.matrix(data.table::fread(file = CharIndicatorsLoc))
        
        print2("Loading LinkOrgs_Helpers.R...")
        source( sprintf('%s/Analysis/LinkOrgs_Helpers.R', ModelLoc), local = T )
        
        print2("Loading JaxTransformer_Imports.R...")
        source( sprintf('%s/Analysis/JaxTransformer_Imports.R', ModelLoc), local = T )
        
        print2("Loading JaxTransformer_BuildML.R...")
        source( sprintf('%s/Analysis/JaxTransformer_BuildML.R', ModelLoc), local = T )
        
        print2("Loading JaxTransformer_TrainDefine.R...")
        source( sprintf('%s/Analysis/JaxTransformer_TrainDefine.R', ModelLoc), local = T )
        print(sprintf( "Default device backend: %s", jax$default_backend())) 
        
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
        pFuzzyMatchFxn_touse <- pFuzzyMatch_euclidean
        jax <<- jax; jnp <<- jnp; np <<- np
    }
    if(algorithm == "deezymatch"){
      library( reticulate )
      reticulate::use_condaenv("py39deezy")
      DeezyMatch <- import("DeezyMatch")
      pd <- import("pandas")
      torch <- import("torch")
      np <- import("numpy")
      jax <- import("jax")
      jnp <- import("jax.numpy")
      pickle <- import("pickle")
  
      orig_wd <- getwd()
      setwd( deezyLoc )
      for(val_ in c("x","y")){
        DatasetPath <- c( "./dataset/dataset-candidates_LINKORGS.txt")
        input_ <- eval(parse(text =
                  sprintf('enc2utf8(gsub(tolower(%s[[by.%s]]),
                          pattern = "\\\\s+", replace =" "))', # [ir_]
                          val_, val_)))
        tmp <- strsplit(input_,split="")
        input_ <- sapply(1:length(input_), function(s_){
            # append to input with no letters to avoid problems in DeezyMatch
            if(!any(letters %in% tmp[[s_]])){
              input_[s_] <- paste0(input_[s_], " - inc")
            }
            input_[s_]
        } )
        writeLines(input_, con = DatasetPath)
        file.remove( sprintf("./candidates/test2/embeddings/%s",
                            list.files("./candidates/test2/embeddings")) )
        DeezyMatch$inference(input_file_path="./inputs/input_dfm.yaml",
                           dataset_path = DatasetPath,
                           pretrained_model_path="./models/test001/test001.model",
                           pretrained_vocab_path="./models/test001/test001.vocab",
                           inference_mode="vect",
                           scenario="./candidates/test2")
        nBatches <- length(list.files("./candidates/test2/embeddings/")) / 3
        embed <- sapply(0L:(nBatches-1L),function(b_){
          list(cbind(
                np$array( torch$load(sprintf("./candidates/test2/embeddings/rnn_indxs_%s",b_)) ),
                np$array( torch$load(sprintf("./candidates/test2/embeddings/rnn_fwd_%s",b_)) ),
                np$array( torch$load(sprintf("./candidates/test2/embeddings/rnn_bwd_%s",b_)) ) ))
          })
        embed <- (embed <- do.call(rbind,embed))[embed[,1]+1,-1]
  
        if(val_ == "x"){ embedx <- embed }
        if(val_ == "y"){ embedy <- embed }; rm(embed)
        file.remove( sprintf("./candidates/test2/embeddings/%s",
                             list.files("./candidates/test2/embeddings")) )
      }
  
      setwd( orig_wd )
      pFuzzyMatchFxn_touse <- pFuzzyMatch_euclidean
      jax <<- jax; jnp <<- jnp; np <<- np
      if(nrow(embedx) != nrow(x)){browser(); stop("DeezyMatch output mismatch!")}
      if(nrow(embedy) != nrow(y)){browser(); stop("DeezyMatch output mismatch!")}
    }
    if(algorithm == "lookup"){
      # load lookup data - new way 
      {
        # process URLs
        #lookupURL <- dropboxURL2downloadURL("https://www.dropbox.com/scl/fi/ct2qvgrr8jjh6959olrcg/linkedIn_rawData.Rdata?rlkey=v58cqpcccuksie8utobis4eov&dl=0")
        lookupURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/linkedIn_rawData.Rdata"
        
        # download & unzip 
        download.file( lookupURL, destfile = (lookupDest <- sprintf("%s/linkedIn_rawData.Rdata",
                                                     paste0(find.package("LinkOrgs"),"/data") ) ))
        load( lookupDest )
      }
      
      directory_red <- my_data; rm(my_data)
      directory_red$raw_names <- directory_red[,1]
      directory_red[,1] <- tolower(directory_red[,1])
      colnames(directory_red)[1:3] <- c("alias_name","canonical_id","weights") #colnames(directory)[1:3] <- c("from","to","weights")
  
      # define matching metric
      pFuzzyMatchFxn_touse <- pFuzzyMatch_discrete; DistanceMeasure <- "jaccard"
  
      # descriptive stats -  to remove in future version 
      if(T == F){
        library(dplyr)
  
        # Step 1: Combine into a data frame
        df <- as.data.frame(data.frame("x" = directory[,1], "y"=directory[,2]))
        #x <- c("a", "b", "c", "d", "e"); y <- c(1, 1, 2, 2, 2)
        #df <- as.data.frame(data.frame("x" = x, "y"=y))
  
        # Step 2: Process the data
        result <- df %>%
          distinct() %>% # Remove exact duplicates
          group_by(y) %>%
          mutate(unique_x = n_distinct(x)) %>% # Count unique x values per group
          filter(unique_x > 1) %>% # Keep only groups with more than one unique x
          ungroup() %>%
          count('y') # Count occurrences
  
        # To count the number of pairs where x are not equal but have the same y
        sum(result$freq) - nrow(result)
        total_pairs <- nrow(result)
      }
    }
    if(algorithm == "transfer" | DistanceMeasure == "transfer"){
      TransferModelLoc <- sprintf("%s/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv", DownloadFolder)
      if( !file.exists(TransferModelLoc) ){
        #transferCoefs_url <- "https://www.dropbox.com/s/b2lvc4illml68w5/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv.zip?dl=0"
        transferCoefs_url <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/TransferLCoefs_tokenizer_parallelism_FALSE_model_bert-base-multilingual-uncased_layers_-1_device_cpu_logging_level_error_FullTRUE.csv.zip"
        transferCoefs <- try(t(as.matrix(url2dt( transferCoefs_url )[-1,2])),T)
        data.table::fwrite(transferCoefs, file = TransferModelLoc )
      }
  
      # load in transfer learning coefficients
      transferCoefs <- data.table::fread( TransferModelLoc )
  
      # build transfer learning platform
      library(text); try(textrpp_initialize(),T)
      BuildTransferText <- gsub(deparse1(BuildTransfer,collapse="\n"), pattern="function \\(\\)",replace="")
      eval(parse(text = BuildTransferText))
  
      print2("Matching via name representations from a LLM...")
      py_gc <- reticulate::import("gc")
      embedx <- getRepresentation_transfer( x[[by.x]] ); gc(); py_gc$collect()
      embedy <- getRepresentation_transfer( y[[by.y]] ); gc(); py_gc$collect()
      if( algorithm == "transfer" | DistanceMeasure == "transfer" ){ pFuzzyMatchFxn_touse <- pFuzzyMatch_euclidean }
    }
  
    if(algorithm %in% c("bipartite", "markov")){
        # see https://techapple.net/2014/04/trick-obtain-direct-download-links-dropbox-files-dropbox-direct-link-maker-tool-cloudlinker/
        if(algorithm == "bipartite"){ 
          #NetworkURL <- "https://dl.dropboxusercontent.com/s/tq675xfnnxjea4d/directory_data_bipartite_thresh40.zip?dl=0" 
          NetworkURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/directory_data_bipartite_thresh40.zip"
        }
        if(algorithm == "markov"){ 
          #NetworkURL <- "https://dl.dropboxusercontent.com/s/ftt6ts6zrlnjqxp/directory_data_markov.zip?dl=0" 
          NetworkURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/directory_data_markov.zip"
        }
        DirectoryLoc <- gsub(DirectoryZipLoc <- sprintf('%s/Directory_%s.zip',
                                  DownloadFolder, algorithm ), pattern = "\\.zip", replace = "")
        if(!dir.exists(sprintf("%s/Directory_%s/", DownloadFolder, algorithm) ) ){
            download.file( NetworkURL, destfile = DirectoryZipLoc )
            unzip(DirectoryZipLoc, exdir = DirectoryLoc)
        }
        load(sprintf("%s/%s/LinkIt_directory_%s_trigrams.Rdata",
                     DirectoryLoc,
                     ifelse(algorithm == "bipartite", yes = "directory_data_bipartite_thresh40", no = "directory_data_markov"),
                     algorithm))
        load(sprintf("%s/%s/LinkIt_directory_%s.Rdata",
                     DirectoryLoc,
                     ifelse(algorithm == "bipartite", yes = "directory_data_bipartite_thresh40", no = "directory_data_markov"),
                     algorithm) )
  
        directory <- data.table::as.data.table(directory)
        directory_trigrams <- data.table::as.data.table(directory_trigrams)
        if(ToLower == T){ directory_trigrams$trigram <- tolower(directory_trigrams$trigram) }
        directory_trigrams <- directory_trigrams[!duplicated(paste(directory_trigrams$trigram,
                                    directory_trigrams$alias_id,collapse="_")),]
        print2( sprintf("Directory size: %i aliases",nrow( directory )  )); gc()
  
        if( !algorithm %in% c("ml", "transfer", "deezymatch" ) ){
        if( !DistanceMeasure %in% c("ml", "transfer", "deezymatch" ) ){
          pFuzzyMatchFxn_touse <- pFuzzyMatch_discrete
        } }
  
        if(DistanceMeasure %in% c("ml")){
          if(ml_version == "v0"){
            if(algorithm == "bipartite"){ 
              #EmbeddingsURL <- "https://www.dropbox.com/scl/fi/bnp5yxy7pgr6lqk5hd54n/Directory_LinkIt_bipartite_Embeddings_v0.csv.gz?rlkey=bvdzkkg544ujogzy82eyceezn&dl=0" 
              EmbeddingsURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/Directory_LinkIt_bipartite_Embeddings_v0.csv.gz"
            }
            if(algorithm == "markov"){ 
              #EmbeddingsURL <- "https://www.dropbox.com/scl/fi/i8f5n93sxqw7jfyg6u8h5/Directory_LinkIt_markov_Embeddings_v0.csv.gz?rlkey=qxslvzxz0kn4n57mvoodadxif&dl=0" 
              EmbeddingsURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/Directory_LinkIt_markov_Embeddings_v0.csv.gz"
            }
          }
          if(ml_version == "v1"){
            if(algorithm == "bipartite"){ 
              #EmbeddingsURL <- "https://www.dropbox.com/scl/fi/20j96htp1qr6hnvj721qc/Directory_LinkIt_bipartite_Embeddings_v1.csv.gz?rlkey=i9gekn7rmuhidvu6pysanta9b&dl=0" 
              EmbeddingsURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/Directory_LinkIt_bipartite_Embeddings_v1.csv.gz"
            }
            if(algorithm == "markov"){ 
              #EmbeddingsURL <- "https://www.dropbox.com/scl/fi/zkl7x6yfr19nszlyak900/Directory_LinkIt_markov_Embeddings_v1.csv.gz?rlkey=506dspvnakihl9szp3kb02u6m&dl=0" 
              EmbeddingsURL <- "https://huggingface.co/datasets/cjerzak/LinkOrgs_PackageSupport/resolve/main/Directory_LinkIt_markov_Embeddings_v1.csv.gz"
            }
          }
          
          # Convert link if using Dropbox 
          # EmbeddingsURL <- LinkOrgs::dropboxURL2downloadURL(EmbeddingsURL)
  
          EmbedddingsLoc <- sprintf("%s/Directory_LinkIt_%s_Embeddings_%s.csv.gz",
                                    DownloadFolder, algorithm, ml_version)
          if(!file.exists(EmbedddingsLoc)){
            download.file(EmbeddingsURL, destfile = EmbedddingsLoc)
          }
          linkedIn_embeddings <- NA2ColMean(as.matrix(data.table::fread(EmbedddingsLoc, showProgress = T))[,-1]); gc(); py_gc$collect()
          # print(  sort( sapply(ls(),function(x){object.size(get(x))}))  )
        }
    }
    if(algorithm == "fuzzy"){ pFuzzyMatchFxn_touse <- pFuzzyMatch_discrete }
  }
  # check object sizes
  # print(  sort( sapply(ls(),function(x){object.size(get(x))}))  )

  # save original names for later merging
  by_x_orig = x[[by.x]]; by_y_orig = y[[by.y]]

  # process unique IDs
  if(!is.null(by)){ by.x <- by.y <- by }

  x = cbind(sapply(x[[by.x]],digest::digest), x); colnames(x)[1] <- 'Xref__ID'
  y = cbind(sapply(y[[by.y]],digest::digest),y); colnames(y)[1] <- 'Yref__ID'
  names(by_x_orig) <- x$Xref__ID;names(by_y_orig) <- y$Yref__ID
  y$UniversalMatchCol <- x$UniversalMatchCol <- NA
  colnames_x_orig <- colnames(x); colnames_y_orig <- colnames(y)

  print2("Pre-processing strings...")
  x <- as.data.table(x); y <- as.data.table(y)
  if(ToLower == T){
    for(it_ in c("x","y")){
      eval(parse(text = sprintf('set(%s,NULL,by.%s,tolower(%s[[by.%s]]))', it_, it_, it_, it_)))
    }

    # methods use lower-cased text
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
  if(algorithm %in% c("markov","bipartite")){
    keepAliases <- which(  !duplicated(directory$alias_name) & trimws(directory$alias_name) != '' )
    if(DistanceMeasure == "ml"){
      linkedIn_embeddings <- linkedIn_embeddings[keepAliases,]
    }
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
  z_RawNames <- as.data.frame( pFuzzyMatchFxn_touse(
                                  x = x, by.x = by.x, embedx = embedx,
                                  y = y, by.y = by.y, embedy = embedy,
                                  DistanceMeasure = DistanceMeasure, q = qgram, nCores = nCores, 
                                  MaxDist = MaxDist, AveMatchNumberPerAlias = AveMatchNumberPerAlias))
  # View(z_RawNames[order(z_RawNames$stringdist),c(by.x,by.y,"stringdist")])

  if(algorithm %in% c("markov","bipartite","lookup")){
    print2("Searching for LinkedIn network matches")
    pFuzzyMatch_internal <- function(key_){
      print2("Searching for network matches...")
      key_by_ref <- eval(parse(text = sprintf("by.%s",key_)))
      xory_by_ref <- eval(parse(text = sprintf("%s",key_)))

      if(algorithm != "lookup"){
        MatchedIntoNetwork <- pFuzzyMatchFxn_touse(
                    x = xory_by_ref, by.x = key_by_ref, embedx = ifelse(key_ == 'x', yes = list(embedx), no = list(embedy))[[1]],
                    y = directory_red, by.y = "alias_name", embedy = linkedIn_embeddings,
                    DistanceMeasure = DistanceMeasure, q = qgram, nCores = nCores, 
                    MaxDist = MaxDist_network, AveMatchNumberPerAlias = AveMatchNumberPerAlias_network)
        MatchedIntoNetwork <- DeconflictNames(MatchedIntoNetwork)
      }
      if(algorithm == "lookup"){
        MatchedIntoNetwork <- merge(xory_by_ref, by.x = key_by_ref, all.x = F,
              directory_red, by.y = "alias_name", all.y = F)
        MatchedIntoNetwork$stringdist <- runif(nrow(MatchedIntoNetwork),0,0.0000001) # add to avoid problems with Deduplication checks
      }

      print2("De-duplicating merge among duplicates to canonical_id [keeping closest match]...")
      MatchedIntoNetwork$duplication_check <- paste(MatchedIntoNetwork[[key_by_ref]],
                                                    MatchedIntoNetwork[["canonical_id"]], sep = "_")
      minDist_vec <- tapply(MatchedIntoNetwork$stringdist, MatchedIntoNetwork$duplication_check, min)
      MatchedIntoNetwork$stringdist <- minDist_vec[ MatchedIntoNetwork$duplication_check  ]

      # drop duplicates after calculating stringdist like so
      MatchedIntoNetwork <- MatchedIntoNetwork[!duplicated(MatchedIntoNetwork$duplication_check),]

      print2(sprintf("Done deduplicating; final dimensions: %s...", nrow(MatchedIntoNetwork)))
      gc(); return( MatchedIntoNetwork )
    }

    # perform analysis via network
    x2Network <- pFuzzyMatch_internal(key_ = 'x')
    y2Network <- pFuzzyMatch_internal(key_ = 'y')

    # merge into network - some redundancies may emerge due to multiple observations per name
    x2Network <- DeconflictNames( merge(x = as.matrix(x), by.x = by.x,
                       y = as.matrix(x2Network), by.y = by.x, all = F) )
    colnames(x2Network)[colnames(x2Network) %in% 'stringdist'] <- "stringdist.x2network"
    x2Network$canonical_id <- as.character(x2Network$canonical_id);

    y2Network <- DeconflictNames( merge(x = as.matrix(y), by.x = by.y,
                      y = as.matrix(y2Network), by.y = by.y, all = F) )
    colnames(y2Network)[colnames(y2Network) %in% 'stringdist'] <- "stringdist.y2network"
    y2Network$canonical_id <- as.character(y2Network$canonical_id)

    # merge network into network
    z_Network <- DeconflictNames( as.data.frame(merge(x2Network, y2Network, by="canonical_id", all=F)) )
    colnames(z_Network)[colnames(z_Network) == "canonical_id"] <- "ID_MATCH"

    # bring back distances to 0 (after random jitter to avoid problems with deconflict names for lookup)
    #if(algorithm == "lookup"){ z_Network$stringdist.x2network <- z_Network$stringdist.y2network <- 0. }
  }

  print2("Combining with fuzzy matches...")
  {
  z_RawNames$XYref__ID <- paste( z_RawNames$Xref__ID, z_RawNames$Yref__ID, sep="__LINKED__" )
  if(is.null(z_Network)){ z <- as.data.frame( z_RawNames ) }
  if(!is.null(z_Network)){
    z_Network$XYref__ID <- paste( z_Network$Xref__ID, z_Network$Yref__ID, sep="__LINKED__" )
    z = rbind.fill(as.data.frame( z_RawNames ), as.data.frame( z_Network ) )
  }
    
  print2("Dropping duplicates..."); if( nrow(z)>1 ){
    if(is.null(z_Network)){ z$minDist <- z$stringdist }
    if(!is.null(z_Network)){
      z$stringdist <- f2n( z$stringdist )
      z$stringdist.y2network <- RelThresNetwork * f2n( z$stringdist.y2network )
      z$stringdist.x2network <- RelThresNetwork * f2n( z$stringdist.x2network )

      # x bigger - set maxDist2Network to x
      z$maxDist2Network <- apply(z[,c("stringdist.y2network","stringdist.x2network")], 1, max)

      # NAs cast to 0, add to get full profile of min distances
      z$minDist <- na20(z$maxDist2Network) + na20(z$stringdist)

      # take min{  max{network}, string } and save this as the min distance
      minDist_vec <- tapply(z$minDist, z$XYref__ID, min)
      z$minDist <- minDist_vec[z$XYref__ID];
    }

    if(T == F){ # checks
      View(z[order(f2n(z_RawNames$stringdist)),c(by.x,by.y,"stringdist")])
      View(z_Network[order(f2n(z_Network$stringdist.x2network)),c(by.x,by.y,"stringdist.x2network","stringdist.y2network")])
      View(z[order(f2n(z$minDist)),c(by.x,by.y,"stringdist.x2network","stringdist.y2network","maxDist2Network","stringdist","minDist")])
      View(z[order(f2n(z$minDist)),c(by.x,by.y,"minDist")])
      View(z[!duplicated(z$XYref__ID),][order(z[!duplicated(z$XYref__ID),]$minDist),c(by.x,by.y,"minDist")])
    }

    # drop duplicates now that minDist is established
    z <- z[!duplicated(z$XYref__ID),]
    if(!ReturnDecomposition){ suppressWarnings( rm(z_RawNames, z_Network) ) }
  }

  print2("Checking for redundant name matches...")
  z <- DeconflictNames(z)

  #undo modifications to names for processing
  z[[by.y]] <- by_y_orig[z$Yref__ID]
  z[[by.x]] <- by_x_orig[z$Xref__ID]
  z  = z[,!colnames(z) %fin% c("ID_MATCH.x", "ID_MATCH.y",
                               'Yref__ID', 'Xref__ID')]
  if(!(algorithm %in% c("markov", "bipartite"))){
    z <- z[,!colnames(z) %in% c("stringdist2network",
                                "stringdist.y2network", "stringdist.x2network")]
  }
  }

  if( ReturnDiagnostics == F ){
    z  = z[,colnames(z)[colnames(z) %fin% c(colnames(x ),colnames(y ),
                                            grep(colnames(z),pattern="stringdist",value=T), "minDist")]]
    z = z[,!colnames(z) %fin% "UniversalMatchCol"]
  }

  if(DistanceMeasure != "ml"){ doParallel::stopImplicitCluster() }
  # if(any(is.na( z[[by.x]] ))){browser()}; if(any(is.na( z[[by.y]] ))){browser()}

  print2("Returning matched dataset!")
  if(ReturnDecomposition == T){ z = list("z" = z,
                                         "z_RawNames" = z_RawNames,
                                         "z_Network" = z_Networks)  }
  gc(); return(  z )
}
