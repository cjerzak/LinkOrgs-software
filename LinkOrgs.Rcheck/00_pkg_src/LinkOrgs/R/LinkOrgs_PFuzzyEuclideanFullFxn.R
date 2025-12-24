#!/usr/bin/env Rscript
#' Fuzzy Match with Euclidean Distance on Embeddings
#'
#' Performs fuzzy matching between two data frames using Euclidean distance on
#' pre-computed embeddings. This is a wrapper around [pDistMatch_euclidean()]
#' that returns the merged data frame with matched records.
#'
#' @param x,y Data frames to be merged.
#' @param by.x Character string; column name in `x` containing organization names.
#' @param by.y Character string; column name in `y` containing organization names.
#' @param embedx,embedy Numeric matrices containing embeddings for records in `x` and `y`
#'   respectively. Rows correspond to observations and columns to embedding dimensions.
#'   These are typically produced by the ML backend (see [LinkOrgs()] with `algorithm = "ml"`).
#' @param embedDistMetric Optional custom distance metric function for computing distances
#'   between embeddings. If `NULL`, Euclidean distance is used.
#' @param MaxDist Numeric; maximum allowed Euclidean distance between matched embeddings.
#'   Pairs with distances greater than this threshold are excluded. If
#'   `AveMatchNumberPerAlias` is specified, it takes priority.
#' @param AveMatchNumberPerAlias Numeric; target average number of matches per alias.
#'   If specified, automatically calibrates `MaxDist` using [GetCalibratedDistThres()].
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame containing matched records from `x` and `y`, with columns from
#'   both data frames (suffixed with `.x` and `.y` respectively) and a `stringdist`
#'   column indicating the Euclidean distance between each matched pair's embeddings.
#'
#' @details This function is typically used internally by [LinkOrgs()] when
#'   `algorithm = "ml"` or `DistanceMeasure = "ml"`. It computes Euclidean distances
#'   between embedding vectors rather than string distances.
#'
#' @examples
#' \dontrun{
#' # Create synthetic data with embeddings
#' x <- data.frame("orgnames_x" = c("apple", "oracle"))
#' y <- data.frame("orgnames_y" = c("apple corp", "oracle inc"))
#'
#' # Assume embedx and embedy are pre-computed embedding matrices
#' # (typically produced by LinkOrgs ML backend)
#' embedx <- matrix(rnorm(2 * 256), nrow = 2)
#' embedy <- matrix(rnorm(2 * 256), nrow = 2)
#'
#' matched <- pFuzzyMatch_euclidean(x = x,
#'                                  y = y,
#'                                  by.x = "orgnames_x",
#'                                  by.y = "orgnames_y",
#'                                  embedx = embedx,
#'                                  embedy = embedy,
#'                                  MaxDist = 5.0)
#' }
#'
#' @seealso [pDistMatch_euclidean()] for the underlying distance computation,
#'   [GetCalibratedDistThres()] for automatic threshold calibration,
#'   [pFuzzyMatch_discrete()] for string-distance-based matching.
#' @export
#' @md

pFuzzyMatch_euclidean <- function(
    x = NULL, by.x = NULL, embedx = NULL,
    y = NULL, by.y = NULL, embedy = NULL,
    embedDistMetric = NULL, 
    MaxDist = NULL,
    AveMatchNumberPerAlias = NULL,
    ...
){
  if(!is.null(AveMatchNumberPerAlias)){
    MaxDist <- GetCalibratedDistThres(x = embedy,
                                      y = embedx,
                                      AveMatchNumberPerAlias = AveMatchNumberPerAlias,
                                      mode = "euclidean")
  }

  z_indices <- pDistMatch_euclidean(embedx, embedy,
                                    MaxDist = MaxDist)
  colnames(y) <- paste(colnames(y), '.y', sep = "")
  colnames(x) <- paste(colnames(x), '.x', sep = "")
  z <- as.data.frame( cbind( x[z_indices[,"ix"],], y[z_indices[,"iy"],] ) )
  z$stringdist <- z_indices[,"stringdist"]
  gc(); return( DeconflictNames( z ) )
}
