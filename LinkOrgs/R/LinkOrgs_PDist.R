#' pDistMatch
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
#' `pDistMatch` can automatically process the `by` text for each dataset. Users may specify the following options:
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
pDistMatch <- function(embed1, embed2, AcceptThreshold = NULL){
  # parallelized distance matching

  # cast to matrix
  embed1 <- as.matrix(embed1); embed2 <- t(as.matrix(embed2))

  # parallelized dist calc
  library(foreach);cl <- doMC::registerDoMC(ncl<-parallel::detectCores());
  distMat <- foreach(ier = 1:nrow(embed1)) %dopar% {
    dist_vec <- colSums( (embed1[ier,] - embed2)^2 )^0.5  # embed2 is transposed for row broadcasting

    match_ <- NULL
    match_indices <- ifelse(is.null(AcceptThreshold),
                            yes = list(1:length( dist_vec )),
                            no = list(which(dist_vec <= AcceptThreshold)))[[1]]

    if(length(match_indices) > 0){
      match_ <- data.frame( "ix" = ier,
                            "iy" = match_indices,
                            "dist" = dist_vec[match_indices] )
    }
    return( list( match_ ))
  }
  distMat <- unlist(distMat,recursive=F)
  distMat <- as.data.frame( do.call(rbind,distMat) )
}
