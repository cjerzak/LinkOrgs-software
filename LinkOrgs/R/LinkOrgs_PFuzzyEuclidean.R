#' pDistMatch_euclidean
#'
#' Performs parallelized distance computation strings based on the string distance measure specified in `DistanceMeasure`. Matching is parallelized using all available CPU cores to increase execution speed.
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
#' `pDistMatch_euclidean` can automatically process the `by` text for each dataset. Users may specify the following options:
#'
#' - Set `DistanceMeasure` to control algorithm for computing pairwise string distances. Options include "`osa`", "`jaccard`", "`jw`". See `?stringdist::stringdist` for all options. (Default is "`jaccard`")
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
#' linkedOrgs_fuzzy <- pFuzzyMatch(x = x,
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
#'
pDistMatch_euclidean <- function(embedx, embedy, MaxDist = NULL){
  # parallelized distance matching

  # broadcast across larger matrix for fast vectorization
  if(swappedXY <- (nrow(y) < nrow(x))){
    # reason for condition: y should be larger than x for efficient vectorization potential
    embedy_old <- embedy
    embedx_old <- embedx
    embedy <- embedx_old
    embedx <- embedy_old
    rm(embedx_old, embedy_old)
  }

  # cast to matrix
  embedx <- as.matrix(embedx); embedy <- t(as.matrix(embedy))

  # parallelized dist calc
  library(foreach);cl <- doMC::registerDoMC(ncl<-parallel::detectCores());
  distMat <- foreach(ix = 1:nrow(embedx)) %dopar% {
    # calculate distances
    dist_vec <- colSums( (embedx[ix,] - embedy)^2 )^0.5  # embed2 is already transposed for row broadcasting

    # select iy matches to ix
    iy <- ifelse(is.null(MaxDist),
                 yes = list(1:length( dist_vec )),
                 no = list(which(dist_vec <= MaxDist)))[[1]]

    match_ <- NULL; if(length(iy) > 0){
      match_ <- data.frame( "ix" = ix,
                            "iy" = iy,
                            "stringdist" = dist_vec[match_indices] )
    }
    return( list( match_ ))
  }
  distMat <- as.data.frame( do.call(rbind, unlist(distMat,recursive=F) ))

  if(swappedXY){
    distMat <- data.frame("ix" = distMat$iy,
                          "iy" = distMat$ix,
                          "stringdist" = distMat$stringdist)
  }

  return( distMat )
}
