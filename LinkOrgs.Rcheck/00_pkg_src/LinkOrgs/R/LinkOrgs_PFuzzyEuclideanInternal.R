#' Compute Euclidean Distances Between Embeddings (Internal)
#'
#' Computes pairwise Euclidean distances between embedding vectors from two sets of
#' observations. This function is used internally for ML-based matching where
#' organization names have been converted to numeric embeddings.
#'
#' @param embedx Numeric matrix; embeddings for the first set of observations. Rows
#'   correspond to observations and columns to embedding dimensions.
#' @param embedy Numeric matrix; embeddings for the second set of observations. Rows
#'   correspond to observations and columns to embedding dimensions.
#' @param MaxDist Numeric; maximum allowed Euclidean distance. Pairs with distances
#'   greater than this threshold are excluded. If `NULL`, all pairs are returned.
#' @param embedDistMetric Optional function; custom distance metric. If `NULL`,
#'   Euclidean distance is computed. The function should take two arguments
#'   (expanded x vector and transposed y matrix) and return a distance vector.
#' @param ReturnProgress Logical; if `TRUE`, progress information is available
#'   (currently disabled). Default is `TRUE`.
#'
#' @return A data frame with three columns:
#' \describe{
#'   \item{ix}{Integer; row index in `embedx` of the matched record.}
#'   \item{iy}{Integer; row index in `embedy` of the matched record.}
#'   \item{stringdist}{Numeric; the Euclidean distance between the matched pair's
#'     embeddings (named `stringdist` for consistency with discrete matching).}
#' }
#' Returns an empty data frame if no matches are found below `MaxDist`.
#'
#' @details This function computes Euclidean distances between all pairs of
#' embedding vectors. For efficiency:
#'
#' - Automatically swaps `embedx` and `embedy` if `embedy` has fewer rows for
#'   better vectorization.
#' - Uses JAX for GPU acceleration when available (detected automatically).
#' - Rounds embeddings to reduce precision overhead when embedding precision differs.
#'
#' The function is typically called by [pFuzzyMatch_euclidean()] rather than
#' directly by users.
#'
#' @examples
#' \dontrun{
#' # Create synthetic embeddings
#' embedx <- matrix(rnorm(4 * 256), nrow = 4)
#' embedy <- matrix(rnorm(4 * 256), nrow = 4)
#'
#' # Compute distances
#' distances <- pDistMatch_euclidean(embedx = embedx,
#'                                   embedy = embedy,
#'                                   MaxDist = 5.0)
#' }
#'
#' @seealso [pFuzzyMatch_euclidean()] for the higher-level wrapper that returns
#'   merged data, [pDistMatch_discrete()] for string-distance-based matching.
#' @importFrom stats median
#' @importFrom plyr rbind.fill
#' @import doParallel
#' @import data.table
#' @export
#' @md
pDistMatch_euclidean <- function(embedx, embedy, MaxDist = NULL, embedDistMetric=NULL, ReturnProgress = T){
  # parallelized distance matching

  # Handle empty inputs early
  if(nrow(embedx) == 0 || nrow(embedy) == 0){
    return(data.frame(ix = integer(0), iy = integer(0), stringdist = numeric(0)))
  }

  # broadcast across larger matrix for fast vectorization
  if(swappedXY <- (nrow(embedy) < nrow(embedx))){
    print2("Swapping for efficient use of vectorization...")
    # reason for condition: y should be larger than x for efficient vectorization potential
    embedy_old <- embedy
    embedx_old <- embedx
    embedy <- embedx_old
    embedx <- embedy_old
    rm(embedx_old, embedy_old); gc()
  }

  # cast to matrix
  print2("Casting to matrix")
  embedx <- as.matrix(embedx); embedy <- t(as.matrix(embedy))

  # parallelized dist calc
  # library(foreach);cl <- doParallel::registerDoParallel(ncl<- max(c(1L,(parallel::detectCores() - 2L))));
  # print(  sort( sapply(ls(),function(x){object.size(get(x))}))  )

  # perform the op
  NoExport <- c(ls(), ls(parent.env(environment())), ls(globalenv()))
  NoExport <- NoExport[!NoExport %in% (Export <- c("embedx", "embedy", "MaxDist","ncl","ReturnProgress"))]

  CountDecimalPlaces <- function(xx) {
    xx <- sapply(c(xx), function(x){
    # Convert the number to a string
    str_num <- as.character(x)

    # Find the position of the decimal point
    decimal_pos <- regexpr("\\.", str_num)

    # Calculate the number of characters after the decimal point
    # If there is no decimal point, return 0
    if (decimal_pos < 0) { return(0) } else { return(nchar(substr(str_num, decimal_pos + 1, nchar(str_num)))) }
    })
    return( median(xx) )
  }

  UseDigits <- c(median(CountDecimalPlaces(embedx[1:min(c(10,nrow(embedx))), 1:10])),
                  median(CountDecimalPlaces(embedy[1:10,1:min(c(10,ncol(embedy)))])))
  if(which.min(UseDigits) == 1){ embedy <- round(embedy, min(UseDigits)) }
  if(which.min(UseDigits) == 2){ embedx <- round(embedx, min(UseDigits)) }

  print2(sprintf("Starting parallel Euclidean distance calculations with dim(x) = [%s, %s] and dim(y) = [%s, %s]...",
                dim(embedx)[1], dim(embedx)[2], dim(embedy)[2], dim(embedy)[1]))

  if("jax" %in% ls()){
    embedy <- jnp$array(embedy, dtype = jnp$float16)
    ColDists_jit <- jax$jit(function(an_x){
      if(is.null(embedDistMetric)){  # return euclidean 
        val_ <- jnp$sqrt( 0.0001 + jnp$mean( (jnp$expand_dims(an_x,1L) - embedy)^2, 0L) )
      }   
      if(!is.null(embedDistMetric)){  # compute custom value 
        val_ <- embedDistMetric(jnp$expand_dims(an_x,1L), embedy)
      }
      return( val_ )
    })
  }

  #gc(); distMat <- (foreach(ix = 1:nrow(embedx), .export = Export, .noexport = NoExport) %dopar% { # useful for small embedding dims
  gc(); distMat <- lapply(1:nrow(embedx), function(ix){ # useful for big embedding dims
    # Commented out: hardcoded file path in progress tracking (Bug #7)
    # if(ix %% 25 == 0 & ReturnProgress | ix < 10){ gc(); write.csv(data.frame("Current ix" = max(ix),
    #                                                        "Total ix" = nrow(embedx)),
    #                                             file = './PROGRESS_pDistMatch_euclidean.csv')}

    # calculate distances
    if(!"jax" %in% ls()){
       dist_vec <- colSums( (embedx[ix,] - embedy)^2 )^0.5  # embed2 is already transposed for row broadcasting
    }
    if("jax" %in% ls()){
      dist_vec <- np$array( ColDists_jit( jnp$array(embedx[ix,], jnp$float16) ) )
    }

    # select iy matches to ix
    iy <- ifelse(is.null(MaxDist),
                 yes = list( belowThres_ <- (1:length( dist_vec ))),
                 no = list( belowThres_ <- which(dist_vec <= MaxDist)))[[1]]

    match_ <- NULL; if(length(iy) > 0){
      match_ <- cbind(ix, matrix( c(iy, dist_vec[belowThres_]),
                                  nrow = length(iy),
                                  ncol = 2L, byrow = F))
    }
    return( list( match_ ))
  })
  print2("Done with Euclidean distance calculations...")
  gc(); distMat <- do.call(rbind, unlist(distMat, recursive=F))

  # Handle empty results
  if(is.null(distMat) || length(distMat) == 0){
    return(data.frame(ix = integer(0), iy = integer(0), stringdist = numeric(0)))
  }

  distMat <- as.data.frame(distMat)
  colnames(distMat) <- c("ix","iy","stringdist")

  print2("Wrapping up call to pDistMatch_euclidean()...")
  if(swappedXY){
    distMat <- data.frame("ix" = distMat[,"iy"],
                          "iy" = distMat[,"ix"],
                          "stringdist" = distMat[,"stringdist"])
  }

  return( distMat )
}
