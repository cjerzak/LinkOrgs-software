#!/usr/bin/env Rscript
#' Fuzzy Match with Discrete String Distances
#'
#' Performs fuzzy matching between two data frames using string distance measures
#' (e.g., Jaccard, OSA, Jaro-Winkler). This is a wrapper around [pDistMatch_discrete()]
#' that returns the merged data frame with matched records.
#'
#' @param x,y Data frames to be merged.
#' @param by.x Character string; column name in `x` containing organization names.
#' @param by.y Character string; column name in `y` containing organization names.
#' @param embedx,embedy Optional embedding matrices (not used in discrete matching,
#'   included for API consistency).
#' @param embedDistMetric Optional custom distance metric (not used in discrete matching).
#' @param MaxDist Numeric; maximum allowed distance between matched strings. Pairs with
#'   distances greater than this threshold are excluded. If `AveMatchNumberPerAlias` is
#'   specified, it takes priority.
#' @param qgram Integer; the q-gram size for string distance calculation. Default is `2`.
#' @param DistanceMeasure Character; algorithm for computing pairwise string distances.
#'   Options include `"jaccard"`, `"osa"`, `"jw"`. See `?stringdist::stringdist` for
#'   all options. Default is `"jaccard"`.
#' @param AveMatchNumberPerAlias Numeric; target average number of matches per alias.
#'   If specified, automatically calibrates `MaxDist` using [GetCalibratedDistThres()].
#' @param nCores Integer; number of CPU cores for parallel processing. Default is `NULL`
#'   (uses single core).
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame containing matched records from `x` and `y`, with columns from
#'   both data frames (suffixed with `.x` and `.y` respectively) and a `stringdist`
#'   column indicating the distance between each matched pair.
#'
#' @details This function uses trigram indexing to efficiently filter candidate matches
#'   before computing exact string distances. This approach significantly speeds up
#'   matching for large datasets.
#'
#' @examples
#' # Create synthetic data
#' x_orgnames <- c("apple", "oracle", "enron inc.", "mcdonalds corporation")
#' y_orgnames <- c("apple corp", "oracle inc", "enron", "mcdonalds co")
#' x <- data.frame("orgnames_x" = x_orgnames)
#' y <- data.frame("orgnames_y" = y_orgnames)
#'
#' # Perform fuzzy matching
#' matched <- pFuzzyMatch_discrete(x = x,
#'                                 y = y,
#'                                 by.x = "orgnames_x",
#'                                 by.y = "orgnames_y",
#'                                 MaxDist = 0.5)
#'
#' @seealso [pDistMatch_discrete()] for the underlying distance computation,
#'   [GetCalibratedDistThres()] for automatic threshold calibration,
#'   [pFuzzyMatch_euclidean()] for embedding-based matching.
#' @export
#' @md

pFuzzyMatch_discrete <- function(
    x = NULL, by.x = NULL, embedx = NULL,
    y = NULL, by.y = NULL, embedy = NULL, embedDistMetric = NULL, 
    MaxDist = NULL,
    qgram = 2, DistanceMeasure = "jaccard",
    AveMatchNumberPerAlias = NULL,
    nCores = NULL,
    ...
){
  if(!is.null(AveMatchNumberPerAlias)){
    MaxDist <- GetCalibratedDistThres(x = x, by.x = by.x,
                                      y = y, by.y = by.y,
                                      AveMatchNumberPerAlias = AveMatchNumberPerAlias,
                                      qgram = qgram,
                                      DistanceMeasure = DistanceMeasure,
                                      nCores = nCores,
                                      mode = "discrete" )
  }
  z_indices <- pDistMatch_discrete(x, by.x = by.x,
                                   y, by.y = by.y,
                                   qgram = qgram,
                                   DistanceMeasure = DistanceMeasure,
                                   MaxDist = MaxDist, 
                                   nCores = nCores)
  colnames(y) <- paste(colnames(y), '.y', sep = "")
  colnames(x) <- paste(colnames(x), '.x', sep = "")
  z <- as.data.frame( cbind( x[z_indices[,"ix"],], y[z_indices[,"iy"],] ) )
  z$stringdist <- z_indices[,"stringdist"]
  gc(); return( DeconflictNames( z  ) )
}
