#' GetCalibratedDistThres
#'
#' Performs parallelized fuzzy matching of strings based on the string distance measure specified in `DistanceMeasure`. Matching is parallelized using all available CPU cores to increase execution speed.
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
#' `pFuzzyMatch` can automatically process the `by` text for each dataset. Users may specify the following options:
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

GetCalibratedDistThres <- function(x, y, AveMatchNumberPerAlias = 5L,
                                   mode = "euclidean"){
  print2("Calibrating via AveMatchNumberPerAlias...")

  # first, calculate all pairwise distances between x and y for a random subsample
  cal_x_indices <- sample(1:nrow(x), min(nrow(x),3000), replace = F)
  cal_y_indices <- sample(1:nrow(y), min(nrow(y),3000), replace = F)
  DistMat <- ifelse(mode == "euclidean", yes = pDistMatch_euclidean,
                    no = pDistMatch_euclidean)(x[cal_x_indices,],
                                               y[cal_y_indices,])

  # then,calculate the implied quantile needed to generate a specific # of matches,
  # for the full inner-joined dataset
  log_NPossibleMatches <- log(nrow(y)) + log(nrow(x))
  NObs_GeometricMean <- (nrow(x)*nrow(y))^0.5
  log_AveMatchesPerObs_times_NObs <- log(AveMatchNumberPerAlias) + log( NObs_GeometricMean  )

  # log(AveMatchesPerObs*nObs / NPossibleMatches)
  ImpliedQuantile <- exp(log_AveMatchesPerObs_times_NObs - log_NPossibleMatches)

  #get implied distance thres from random sample of distances
  IMPLIED_MATCH_DIST_THRES <- try(mean(abs(quantile(DistMat$dist,  probs = min(1,ImpliedQuantile) ))),T)
  print2(sprintf("Calibrated accept match dist threshold: %.6f",IMPLIED_MATCH_DIST_THRES))
  return( IMPLIED_MATCH_DIST_THRES )
}