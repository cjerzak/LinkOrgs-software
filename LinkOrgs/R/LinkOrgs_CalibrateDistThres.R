#' GetCalibratedDistThres
#'
#' Calibrates a distance threshold based on a target average number of matches per alias.
#' Samples pairwise distances from a subset of observations to determine the threshold
#' that would yield approximately the desired number of matches.
#'
#' @param x Input data. For `mode = "euclidean"`: an embedding matrix where rows are observations
#'   and columns are embedding dimensions. For `mode = "discrete"`: a data frame containing
#'   the column specified by `by.x`.
#'
#' @param y Input data. For `mode = "euclidean"`: an embedding matrix where rows are observations
#'   and columns are embedding dimensions. For `mode = "discrete"`: a data frame containing
#'   the column specified by `by.y`.
#'
#' @param by.x Column name in `x` to use for matching. Only used when `mode = "discrete"`.
#'
#' @param by.y Column name in `y` to use for matching. Only used when `mode = "discrete"`.
#'
#' @param AveMatchNumberPerAlias Target average number of matches per observation.
#'   Used to calibrate the distance threshold. Default is 5.
#'
#' @param qgram The q-gram size for string distance calculation. Only used when
#'   `mode = "discrete"`. Default is 2.
#'
#' @param DistanceMeasure The string distance measure to use. Only used when
#'   `mode = "discrete"`. Options include "jaccard", "osa", "jw". See
#'   `?stringdist::stringdist` for all options. Default is "jaccard".
#'
#' @param nCores Number of CPU cores for parallel computation. Only used when
#'   `mode = "discrete"`. Default is NULL (auto-detect).
#'
#' @param mode Character string specifying the distance computation mode.
#'   Must be either "euclidean" (for embedding-based matching) or "discrete"
#'   (for string-based matching). Default is "euclidean".
#'
#' @return A numeric value representing the calibrated distance threshold.
#'
#' @export
#' @md

GetCalibratedDistThres <- function(x = NULL, y = NULL,
                                   by.x = NULL, by.y = NULL,
                                   AveMatchNumberPerAlias = 5L,
                                   qgram = 2L, DistanceMeasure = "jaccard", nCores = NULL, 
                                   mode = "euclidean"){
  print2("Calibrating via AveMatchNumberPerAlias...")
  
  # first, calculate all pairwise distances between x and y for a random subsample
  cal_x_indices <- sample(1:nrow(x), min(nrow(x),1000), replace = F)
  cal_y_indices <- sample(1:nrow(y), min(nrow(y),1000), replace = F)

  if(mode == "euclidean"){
    DistMat <- pDistMatch_euclidean(embedx = x[cal_x_indices,],
                                    embedy = y[cal_y_indices,])
  }

  if(mode == "discrete"){
    DistMat <- pDistMatch_discrete(x = x[cal_x_indices, , drop = FALSE], by.x = by.x,
                                   y = y[cal_y_indices, , drop = FALSE], by.y = by.y,
                                   qgram = qgram, DistanceMeasure = DistanceMeasure,
                                   MaxDist = Inf,
                                   nCores = 1L)
  }

  # Handle case where DistMat is NULL or has no rows
  if(is.null(DistMat) || !is.data.frame(DistMat)){
    DistMat <- data.frame(ix = integer(0), iy = integer(0), stringdist = numeric(0))
  }

  IMPLIED_MATCH_DIST_THRES <- Inf; if(nrow(DistMat) > 0){
    # then,calculate the implied quantile needed to generate a specific # of matches,
    # for the full inner-joined dataset
    log_NPossibleMatches <- log(nrow(y)) + log(nrow(x))
    log_NObs_GeometricMean <-  0.5* (log(nrow(x))+log(nrow(y))) # performs: log((nx*ny)^0.5)
    log_AveMatchesPerObs_times_NObs <- log(AveMatchNumberPerAlias) + log_NObs_GeometricMean

    # log(AveMatchesPerObs*nObs / NPossibleMatches)
    ImpliedQuantile <- exp(log_AveMatchesPerObs_times_NObs - log_NPossibleMatches)

    #get implied distance thres from random sample of distances
    IMPLIED_MATCH_DIST_THRES <- mean(abs(quantile(DistMat[,"stringdist"], probs = min(1,ImpliedQuantile) )))
    if(IMPLIED_MATCH_DIST_THRES < 1e-6){IMPLIED_MATCH_DIST_THRES <- 1e-6}
  }
  print2( sprintf("Calibrated accept match dist threshold: %.6f", IMPLIED_MATCH_DIST_THRES) )

  return( IMPLIED_MATCH_DIST_THRES )
}
