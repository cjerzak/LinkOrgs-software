#!/usr/bin/env Rscript
#' LinkOrgs
#'
#' Implements the organizational record linkage algorithms of Libgober and Jerzak (2023+)
#' using half-a-billion open-collaborated records.
#'
#' @param x,y Data frames to be merged.
#' @param by,by.x,by.y Character vector(s) that specify the column names used for merging
#'   data frames `x` and `y`. The merging variables should be organizational names.
#'   See `?base::merge` for more details regarding syntax.
#' @param embedx,embedy Optional pre-computed embedding matrices. If provided, these will
#'   be used instead of computing embeddings from names. Rows correspond to observations
#'   and columns to embedding dimensions.
#' @param embedDistMetric Optional custom distance metric function for embedding-based matching.
#' @param algorithm Character; specifies which algorithm should be used. Options are
#'   `"fuzzy"`, `"ml"`, `"bipartite"`, `"markov"`, and `"transfer"`. Default is `"ml"`,
#'   which uses a machine-learning approach with Transformer networks and up to 11 million
#'   parameters to predict match probabilities using half a billion open-collaborated
#'   records as training data.
#' @param conda_env Character string; specifies a conda environment where JAX and related
#'   packages have been installed (see `?LinkOrgs::BuildBackend`). Used only when
#'   `algorithm='ml'` or `DistanceMeasure='ml'`. Default is `"LinkOrgs_env"`.
#' @param conda_env_required Logical; specifies whether conda environment is required.
#'   Default is `TRUE`.
#' @param ReturnDiagnostics Logical; if `TRUE`, various match-level diagnostics are
#'   returned in the merged data frame. Default is `FALSE`.
#' @param ReturnProgress Logical; if `TRUE`, progress messages are printed during
#'   execution. Default is `TRUE`.
#' @param ToLower Logical; if `TRUE`, converts names to lowercase before matching.
#'   Default is `TRUE`.
#' @param NormalizeSpaces Logical; if `TRUE`, removes extra whitespace from names.
#'   Default is `TRUE`.
#' @param RemovePunctuation Logical; if `TRUE`, removes punctuation from names.
#'   Default is `TRUE`.
#' @param MaxDist Numeric; maximum allowed distance between two matched strings.
#'   If `AveMatchNumberPerAlias` is specified, it takes priority over this parameter.
#' @param MaxDist_network Numeric; maximum allowed distance for network-based matching
#'   when using `algorithm = "bipartite"` or `"markov"`.
#' @param AveMatchNumberPerAlias Numeric; target average number of matches per alias.
#'   Used to automatically calibrate `MaxDist`. Takes priority over `MaxDist` if both
#'   are specified. Default is `10`.
#' @param AveMatchNumberPerAlias_network Numeric; target average number of matches per
#'   alias for network-based candidate selection. Default is `2`.
#' @param DistanceMeasure Character; algorithm for computing pairwise string distances.
#'   Options include `"osa"`, `"jaccard"`, `"jw"`, or `"ml"` for embedding-based distance.
#'   See `?stringdist::stringdist` for all string distance options. Default is `"jaccard"`.
#' @param qgram Integer; the q-gram size used in string distance measures. Default is `2`.
#' @param RelThresNetwork Numeric; relative threshold multiplier for network distances.
#'   Default is `1.5`.
#' @param ml_version Character; specifies which version of the ML algorithm to use.
#'   Options are `"v0"` (9M parameters) through `"v4"`. Default is `"v1"` (11M parameters).
#' @param openBrowser Logical; if `TRUE`, opens browser for debugging. Default is `FALSE`.
#' @param ExportEmbeddingsOnly Logical; if `TRUE` with `algorithm='ml'` (or
#'   `DistanceMeasure='ml'`), returns only ML embeddings for x and/or y without
#'   matching, for offline linkage. Default is `FALSE`.
#' @param ReturnDecomposition Logical; if `TRUE`, returns a list containing the merged
#'   data frame along with intermediate results. Default is `FALSE`.
#' @param python_executable Path to Python executable. Usually not needed if
#'   `conda_env` is specified.
#' @param nCores Integer; number of CPU cores to use for parallel processing.
#'   Default is `NULL` (auto-detect based on data size).
#' @param deezyLoc Path to DeezyMatch installation (for `algorithm = "deezymatch"`).
#'
#' @return If `ExportEmbeddingsOnly = TRUE`, returns a list with `embedx` and/or `embedy`
#'   data frames containing the input names and their embeddings. If
#'   `ReturnDecomposition = TRUE`, returns a list with `z` (merged data), `z_RawNames`
#'   (raw name matches), and `z_Network` (network matches). Otherwise, returns the
#'   merged data frame `z`.
#'
#' @details `LinkOrgs` automatically processes the name text for each dataset
#'   (specified by `by` or `by.x` and `by.y`). Text preprocessing includes:
#'
#' - **Case normalization**: Set `ToLower = FALSE` to preserve case sensitivity.
#' - **Space normalization**: Set `NormalizeSpaces = FALSE` to preserve whitespace.
#' - **Punctuation removal**: Set `RemovePunctuation = FALSE` to preserve punctuation.
#'
#' To use combined machine learning and network methods, set `algorithm` to
#' `"bipartite"` or `"markov"`, and `DistanceMeasure` to `"ml"`.
#'
#' @examples
#' # Create synthetic data
#' x_orgnames <- c("apple", "oracle", "enron inc.", "mcdonalds corporation")
#' y_orgnames <- c("apple corp", "oracle inc", "enron", "mcdonalds co")
#' x <- data.frame("orgnames_x" = x_orgnames)
#' y <- data.frame("orgnames_y" = y_orgnames)
#'
#' # Perform merge with fuzzy matching
#' linkedOrgs <- LinkOrgs(x = x,
#'                        y = y,
#'                        by.x = "orgnames_x",
#'                        by.y = "orgnames_y",
#'                        algorithm = "fuzzy",
#'                        MaxDist = 0.6)
#'
#' print(linkedOrgs)
#'
#' @importFrom data.table ":="
#' @importFrom stats runif
#' @importFrom utils download.file unzip View
#' @importFrom dplyr %>% distinct group_by mutate n_distinct filter ungroup
#' @import fastmatch
#' @import doParallel
#' @import reticulate
#' @export
#' @md

LinkOrgs <- function(x = NULL, y = NULL, by = NULL, by.x = NULL,by.y = NULL,
                    embedx = NULL, embedy = NULL, embedDistMetric = NULL, 
                    algorithm = "ml",
                    conda_env = "LinkOrgs_env", conda_env_required = T,
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
                    ExportEmbeddingsOnly = FALSE,
                    ReturnDecomposition = FALSE,
                    python_executable, 
                    nCores = NULL, 
                    deezyLoc = NULL){
  # Packages loaded via NAMESPACE imports
  # Allow users to pass only `by`
  if (!is.null(by)) {
    if (is.null(by.x)){ by.x <- by };  if (is.null(by.y)){ by.y <- by }
  }

  # Input validation
  if (!is.data.frame(x) && !is.null(x)) {
    stop("'x' must be a data frame")
  }
  if (!is.data.frame(y) && !is.null(y)) {
    stop("'y' must be a data frame")
  }
  if (!is.null(by.x) && !is.null(x) && !(by.x %in% colnames(x))) {
    stop(sprintf("Column '%s' not found in 'x'. Available columns: %s",
                 by.x, paste(colnames(x), collapse = ", ")))
  }
  if (!is.null(by.y) && !is.null(y) && !(by.y %in% colnames(y))) {
    stop(sprintf("Column '%s' not found in 'y'. Available columns: %s",
                 by.y, paste(colnames(y), collapse = ", ")))
  }
  valid_algorithms <- c("ml", "fuzzy", "bipartite", "markov", "transfer")
  if (!tolower(algorithm) %in% valid_algorithms) {
    stop(sprintf("Invalid algorithm '%s'. Must be one of: %s",
                 algorithm, paste(valid_algorithms, collapse = ", ")))
  }

  # define fast match
  `%fin%` <- function(x, table) {fmatch(x, table, nomatch = 0L) > 0L}

  # Initialize cluster variable for cleanup tracking
  cl <- NULL

  if(DistanceMeasure != "ml"){
    if(is.null(nCores)){
      nCores <- ifelse(nrow(x)*nrow(y) > 500,
                       yes = max(c(1L,parallel::detectCores() - 2L)),
                       no = 1L)
    }
    # Respect CRAN check limits (usually 2 cores max during R CMD check)
    chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
    if (nzchar(chk) && chk == "true") {
      nCores <- min(nCores, 2L)
    }
    cl <- parallel::makeCluster(nCores)
    doParallel::registerDoParallel(cl)
    # Ensure cluster is stopped on exit (error, early return, or normal completion)
    on.exit(if(!is.null(cl)) try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
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
        ModelLoc <- gsub(pattern = "\\.zip",
                         replacement = "",
                         x = (ModelZipLoc <- sprintf('%s/Model_%s.zip',
                                                     DownloadFolder,
                                                     ml_version )))
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
        
        # Compute embeddings only for the datasets that are present
        embedx <- NULL
        embedy <- NULL
        if (!is.null(x) && !is.null(by.x)) {
          embedx <- NA2ColMean(
            GetAliasRep_BigBatch(
              gsub(pattern = "\\s+", replacement = " ", x = tolower(x[[by.x]])),
              nBatch_BigBatch = 50
            ) )
        }
        if (!is.null(y) && !is.null(by.y)) {
          embedy <- NA2ColMean(
            GetAliasRep_BigBatch(
              gsub(pattern = "\\s+", replacement = " ", x = tolower(y[[by.y]])),
              nBatch_BigBatch = 50
            ) )
        }
        
        pFuzzyMatchFxn_touse <- pFuzzyMatch_euclidean
        jax <<- jax; jnp <<- jnp; np <<- np
        
        # NEW: if the user only wants embeddings, return them now
        if (isTRUE(ExportEmbeddingsOnly)) {
          out <- list()
          if (!is.null(embedx)) {
            ex <- data.frame(x[[by.x]], embedx, check.names = FALSE)
            colnames(ex)[1] <- by.x
            out$embedx <- ex     # rows aligned to x input
          }
          if (!is.null(embedy)) {
            ey <- data.frame(y[[by.y]], embedy, check.names = FALSE)
            colnames(ey)[1] <- by.y
            out$embedy <- ey     # rows aligned to y input
          }
          return(out)
        }
        
    }
    if(algorithm == "deezymatch"){
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
                  sprintf('enc2utf8(gsub(pattern = "\\\\s+",
                                  replacement = " ",
                                  x = tolower(%s[[by.%s]])))', # [ir_]
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
      if(nrow(embedx) != nrow(x)){stop("DeezyMatch output mismatch!")}
      if(nrow(embedy) != nrow(y)){stop("DeezyMatch output mismatch!")}
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
      if (!requireNamespace("text", quietly = TRUE)) {
        stop("Package 'text' is required for transfer learning. Please install it.")
      }
      try(text::textrpp_initialize(), TRUE)
      BuildTransferText <- gsub(pattern = "function \\(\\)",
                               replacement = "",
                               x = deparse1(BuildTransfer, collapse = "\n"))
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
        DirectoryLoc <- gsub(pattern = "\\.zip",
                              replacement = "",
                              x = (DirectoryZipLoc <- sprintf('%s/Directory_%s.zip',
                                                            DownloadFolder,
                                                            algorithm )))
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
          linkedIn_embeddings <- NA2ColMean(as.matrix(data.table::fread(EmbedddingsLoc, showProgress = T))[,-1]); gc()
          # print(  sort( sapply(ls(),function(x){object.size(get(x))}))  )
        }
    }
    if(algorithm == "fuzzy"){ pFuzzyMatchFxn_touse <- pFuzzyMatch_discrete }
  }
  # check object sizes
  # print(  sort( sapply(ls(),function(x){object.size(get(x))}))  )

  # save original names for later merging
  by_x_orig <- x[[by.x]]; by_y_orig <- y[[by.y]]

  # process unique IDs
  if(!is.null(by)){ by.x <- by.y <- by }

  x <- cbind(sapply(x[[by.x]], digest::digest), x); colnames(x)[1] <- 'Xref__ID'
  y <- cbind(sapply(y[[by.y]], digest::digest), y); colnames(y)[1] <- 'Yref__ID'
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
      eval(parse(text = sprintf('set(%s,NULL,by.%s, stringr::str_replace_all( %s[[by.%s]], pattern="\\\\s+", replacement=" "))', it_, it_, it_, it_)))
    }
    if(algorithm %in% c("bipartite","markov")){
      directory[["alias_name"]] <- stringr::str_replace_all(directory[["alias_name"]], pattern="\\s+", replacement = " ")
    }
  }
  if(RemovePunctuation == T){
    for(it_ in c("x","y")){
      eval(parse(text = sprintf('set(%s,NULL,by.%s, stringr::str_replace_all(%s[[by.%s]],"\\\\p{P}",""))', it_, it_,it_, it_)))
    }
    if(algorithm %in% c("bipartite","markov")){
      directory[["alias_name"]] <- stringr::str_replace_all(directory[["alias_name"]],"\\p{P}","")
    }
  }

  #drop duplicates after pre-process
  if(algorithm %in% c("markov","bipartite")){
    keepAliases <- which(  !duplicated(directory$alias_name) & trimws(directory$alias_name) != '' )
    if(DistanceMeasure == "ml"){
      linkedIn_embeddings <- linkedIn_embeddings[keepAliases,]
    }
    directory <- directory[keepAliases, ]

    #get trigrams
    directory_red <- directory[,c("alias_name","canonical_id")]
    dir_tri_index <- trigram_index(as.character(directory_red$alias_name),"dir.row")
    x_tri_index  <- trigram_index(x[[by.x]],"the.row")
    y_tri_index  <- trigram_index(y[[by.y]],'the.row')

    #drop components of the big corpus which don't share any trigrams with any entries in {x,y}
    tmp <- unique(c(unique(as.character(x_tri_index[, trigram])), unique(as.character(y_tri_index[, trigram]))))
    dir_tri_index <- dir_tri_index[trigram %fin% tmp, ]; rm(tmp); setkey(dir_tri_index, trigram)
  }

  #specify ID_match for the exact/fuzzy matching
  x$UniversalMatchCol <- as.character(x[[by.x]]); y$UniversalMatchCol <- as.character(y[[by.y]])

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

    # Check for empty x2Network before operating on it
    if (!is.null(x2Network) && nrow(x2Network) > 0) {
      colnames(x2Network)[colnames(x2Network) %in% 'stringdist'] <- "stringdist.x2network"
      x2Network$canonical_id <- as.character(x2Network$canonical_id)
    }

    y2Network <- DeconflictNames( merge(x = as.matrix(y), by.x = by.y,
                      y = as.matrix(y2Network), by.y = by.y, all = F) )

    # Check for empty y2Network before operating on it
    if (!is.null(y2Network) && nrow(y2Network) > 0) {
      colnames(y2Network)[colnames(y2Network) %in% 'stringdist'] <- "stringdist.y2network"
      y2Network$canonical_id <- as.character(y2Network$canonical_id)
    }

    # merge network into network (only if both x2Network and y2Network have data)
    if (!is.null(x2Network) && nrow(x2Network) > 0 &&
        !is.null(y2Network) && nrow(y2Network) > 0) {
      z_Network <- DeconflictNames( as.data.frame(merge(x2Network, y2Network, by="canonical_id", all=F)) )

      # Handle empty merge result - set to NULL so downstream NULL checks work correctly
      if (is.null(z_Network) || nrow(z_Network) == 0) {
        z_Network <- NULL
      } else {
        colnames(z_Network)[colnames(z_Network) == "canonical_id"] <- "ID_MATCH"
      }
    } else {
      z_Network <- NULL
    }

    # bring back distances to 0 (after random jitter to avoid problems with deconflict names for lookup)
    #if(algorithm == "lookup"){ z_Network$stringdist.x2network <- z_Network$stringdist.y2network <- 0. }
  }

  print2("Combining with fuzzy matches...")
  {
  # Check if z_RawNames has data before operating on it
  has_raw_matches <- !is.null(z_RawNames) && nrow(z_RawNames) > 0
  has_network_matches <- !is.null(z_Network) && nrow(z_Network) > 0

  if (has_raw_matches) {
    z_RawNames$XYref__ID <- paste( z_RawNames$Xref__ID, z_RawNames$Yref__ID, sep="__LINKED__" )
  }

  if (!has_raw_matches && !has_network_matches) {
    # No matches found from either method - return empty data frame
    print2("Warning: No matches found")
    z <- data.frame()
  } else if (!has_network_matches) {
    z <- as.data.frame( z_RawNames )
  } else {
    z_Network$XYref__ID <- paste( z_Network$Xref__ID, z_Network$Yref__ID, sep="__LINKED__" )
    if (has_raw_matches) {
      z <- rbind.fill(as.data.frame(z_RawNames), as.data.frame(z_Network))
    } else {
      z <- as.data.frame(z_Network)
    }
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
  z <- z[, !colnames(z) %fin% c("ID_MATCH.x", "ID_MATCH.y",
                                'Yref__ID', 'Xref__ID')]
  if(!(algorithm %in% c("markov", "bipartite"))){
    z <- z[,!colnames(z) %in% c("stringdist2network",
                                "stringdist.y2network", "stringdist.x2network")]
  }
  }

  if( ReturnDiagnostics == F ){
    z <- z[, colnames(z)[colnames(z) %fin% c(colnames(x), colnames(y),
                                             grep(colnames(z), pattern = "stringdist", value = T), "minDist")]]
    z <- z[, !colnames(z) %fin% "UniversalMatchCol"]
  }

  # Cluster cleanup handled by on.exit() registered at cluster creation
  # if(any(is.na( z[[by.x]] ))){browser()}; if(any(is.na( z[[by.y]] ))){browser()}

  print2("Returning matched dataset!")
  if(ReturnDecomposition == T){ z <- list("z" = z,
                                          "z_RawNames" = z_RawNames,
                                          "z_Network" = z_Network) }
  gc(); return(  z )
}
