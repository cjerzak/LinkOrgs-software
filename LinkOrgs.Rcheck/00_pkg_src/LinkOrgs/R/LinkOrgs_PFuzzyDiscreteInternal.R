#' Compute Discrete String Distances (Internal)
#'
#' Computes pairwise string distances between organization names in two data frames
#' using discrete distance measures (e.g., Jaccard, OSA, Jaro-Winkler). Uses trigram
#' indexing for efficient candidate filtering and parallel processing for speed.
#'
#' @param x,y Data frames containing organization names to be matched.
#' @param by Character string; column name for matching when both data frames use the
#'   same column name. Overridden by `by.x` and `by.y` if specified.
#' @param by.x Character string; column name in `x` containing organization names.
#' @param by.y Character string; column name in `y` containing organization names.
#' @param embedDistMetric Not used in discrete matching (included for API consistency).
#' @param return_stringdist Logical; if `TRUE`, returns string distances. Default is `TRUE`.
#' @param onlyUFT Logical; if `TRUE`, processes only UTF-8 strings. Default is `TRUE`.
#' @param qgram Integer; the q-gram size for string distance calculation. Default is `2`.
#' @param DistanceMeasure Character; algorithm for computing pairwise string distances.
#'   Options include `"jaccard"`, `"osa"`, `"jw"`. See `?stringdist::stringdist` for
#'   all options. Default is `"jaccard"`.
#' @param MaxDist Numeric; maximum allowed distance between matched strings. Pairs with
#'   distances greater than this threshold are excluded. Default is `0.20`.
#' @param ReturnProgress Logical; if `TRUE`, progress information is available (currently
#'   disabled). Default is `TRUE`.
#' @param nCores Integer; number of CPU cores for parallel processing. Default is `NULL`
#'   (uses single core).
#' @param ReturnMaxDistThreshold Logical; if `TRUE`, returns the distance threshold used.
#'   Default is `FALSE`.
#'
#' @return A data frame with three columns:
#' \describe{
#'   \item{ix}{Integer; row index in `x` of the matched record.}
#'   \item{iy}{Integer; row index in `y` of the matched record.}
#'   \item{stringdist}{Numeric; the string distance between the matched pair.}
#' }
#' Returns an empty data frame if no matches are found below `MaxDist`.
#'
#' @details This function uses a two-stage approach for efficient matching:
#'
#' 1. **Trigram indexing**: Builds an index of character trigrams for each name,
#'    then filters candidate pairs to those sharing at least 5% of trigrams.
#'
#' 2. **Distance computation**: Computes exact string distances only for filtered
#'    candidates, returning pairs with distances at or below `MaxDist`.
#'
#' The function automatically swaps `x` and `y` if `y` has fewer rows than `x`
#' for more efficient parallelization.
#'
#' @examples
#' # Create synthetic data
#' x_orgnames <- c("apple", "oracle", "enron inc.", "mcdonalds corporation")
#' y_orgnames <- c("apple corp", "oracle inc", "enron", "mcdonalds co")
#' x <- data.frame("orgnames_x" = x_orgnames)
#' y <- data.frame("orgnames_y" = y_orgnames)
#'
#' # Compute distances
#' distances <- pDistMatch_discrete(x = x,
#'                                  y = y,
#'                                  by.x = "orgnames_x",
#'                                  by.y = "orgnames_y",
#'                                  MaxDist = 0.5)
#'
#' @seealso [pFuzzyMatch_discrete()] for the higher-level wrapper that returns
#'   merged data, [stringdist::stringdist()] for available distance measures.
#' @import stringdist
#' @importFrom plyr rbind.fill
#' @import data.table
#' @importFrom foreach foreach %dopar%
#' @export
#' @md

pDistMatch_discrete <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, embedDistMetric = NULL,
                           return_stringdist = T, onlyUFT = T,
                           qgram =2, DistanceMeasure = "jaccard", MaxDist = 0.20,
                           ReturnProgress=T, nCores = NULL,
                           ReturnMaxDistThreshold = F){
  if(is.null(by.x) & is.null(by.y)){ by.x <- by.y <- by }


  # Handle empty data frames early
  if(nrow(x) == 0 || nrow(y) == 0){
    return(data.frame(ix = integer(0), iy = integer(0), stringdist = numeric(0)))
  }

  # Set nCores to 1 if NULL
  if(is.null(nCores)){ nCores <- 1L }

  if(swappedXY <- (nrow(y) < nrow(x))){
    # condition: y should be larger than x for efficient parallelization
    # if not, swap
    x_old <- x; y_old <- y;
    by.y_old <- by.y;
    by.x_old <- by.x
    x <- y_old; by.x <- by.y_old; rm(y_old, by.y_old)
    y <- x_old; by.y <- by.x_old; rm(x_old, by.x_old)
  }
  if(by.x == by.y){
    colnames(x)[colnames(x) == by.x] <- paste(by.x, ".x", sep = "")
    colnames(y)[colnames(y) == by.y] <- paste(by.y, ".y", sep = "")
    by.x <- paste(by.x, ".x", sep = ""); by.y <- paste(by.y, ".y", sep = "")
  }
  x <- as.data.table(x); x$by.x_ORIG <- x[[by.x]]
  y <- as.data.table(y); y$by.y_ORIG <- y[[by.y]]
  x_tri_index <- trigram_index(x[[by.x]], "the.row")
  y_tri_index <- trigram_index(y[[by.y]], "the.row")

  # Handle case where trigram indices are empty
  if(nrow(x_tri_index) == 0 || nrow(y_tri_index) == 0){
    return(data.frame(ix = integer(0), iy = integer(0), stringdist = numeric(0)))
  }

  # test
  # trigram_index(c("hi","hiaer", "hi","hi"),"the.row")

  # start pdist calc
  {
    n_iters <- nrow(x)
    # For small datasets, use single core
    if(n_iters <= 50){
      nCores <- 1L
      split_list_x <- list(1:n_iters)
    } else {
      split_list_x <- round(seq(0.5, n_iters, length.out = nCores + 1))
      split_list_x <- as.numeric(cut(1:n_iters, breaks = split_list_x))
      split_list_x <- sapply(1:nCores, function(as){ list(which(split_list_x == as))})
    }

    # Define the worker function
    worker_fn <- function(outer_ix) {
      counter_ <- 0
      my_matched_inner <- matrix(NA, nrow = 0, ncol = 3)
      for(ix in split_list_x[[outer_ix]]){
        counter_ <- counter_ + 1

        # get the trigrams of the name, x[ix,][[by.x]]
        # find the set of entries in directory_LinkIt_red that have x% in common trigram
        MinNumSharedTriGrams <- ceiling( length(
                 my_entry_trigrams <- x_tri_index[the.row==ix, trigram] )*0.05 )
        LT_entries <- table( y_tri_index[trigram %fin% my_entry_trigrams, the.row] )
        if( length(LT_entries) > 0 ){
          LT_entries <- f2n(names(LT_entries[LT_entries >= MinNumSharedTriGrams]))
          dist_vec_red <- stringdist(x[ix,][[by.x]], # match my entry
                                     y[LT_entries,][[by.y]],
                                     method = DistanceMeasure, q = qgram)
          iy <- LT_entries[ belowThres_ <- which(dist_vec_red <= MaxDist) ]
        if(length(iy) > 0){
          my_matched_inner <- rbind(my_matched_inner,
                                    cbind(ix, matrix( c(iy, dist_vec_red[belowThres_]),
                                                      nrow = length(iy), ncol = 2L, byrow = F) ) )
        }
        }
      }
      return( my_matched_inner )
    }

    # Respect CRAN check limits (usually 2 cores max during R CMD check)
    chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
    if (nzchar(chk) && chk == "true") {
      nCores <- min(nCores, 2L)
    }

    # Use sequential or parallel execution based on nCores
    if(nCores == 1L){
      # Sequential execution - no cluster needed
      loop_ <- lapply(1:nCores, worker_fn)
    } else {
      # Parallel execution - register cluster
      cl <- parallel::makeCluster(nCores)
      doParallel::registerDoParallel(cl)
      on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

      Export <- c("split_list_x", "DistanceMeasure", "qgram", "nCores", "ReturnProgress",
                  "x_tri_index", "y_tri_index", "MaxDist",
                  "x", "by.x", "y", "by.y", "worker_fn")
      NoExport <- c(ls(), ls(parent.env(environment())), ls(globalenv()))
      NoExport <- NoExport[!NoExport %in% Export]
      loop_ <- foreach::foreach(outer_ix = 1:nCores,
                       .export = Export,
                       .noexport = NoExport
                       ) %dopar% {
        worker_fn(outer_ix)
      }
    }
    myMatched <- do.call(rbind, loop_)
    # Handle empty results
    if(is.null(myMatched) || nrow(myMatched) == 0){
      return(data.frame(ix = integer(0), iy = integer(0), stringdist = numeric(0)))
    }
    myMatched <- as.data.frame(myMatched)
    colnames(myMatched) <- c("ix","iy","stringdist")
  }
  if(swappedXY){
    myMatched <- cbind("ix" = myMatched[,"iy"],
                       "iy" = myMatched[,"ix"],
                       "stringdist" = myMatched[,"stringdist"])
  }
  return( myMatched )
  }

