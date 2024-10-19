#!/usr/bin/env Rscript
#' pFuzzyMatch_discrete
#'
#' Implements
#'
#' @param x,y data frames to be merged
#'
#' @return ...
#' @export
#'
#' @details ...
#'
#' @examples
#'
#' #Create synthetic data
#' x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
#' y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
#' x <- data.frame("orgnames_x"=x_orgnames)
#' y <- data.frame("orgnames_y"=y_orgnames)
#'
#' @export
#'
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
