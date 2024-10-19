#!/usr/bin/env Rscript
#' pFuzzyMatch_euclidean
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
#'#' #Create synthetic data
#' x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
#' y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
#' x <- data.frame("orgnames_x"=x_orgnames)
#' y <- data.frame("orgnames_y"=y_orgnames)
#'
#' @export
#'
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
                                      AveMatchNumberPerAlias = AveMatchNumberPerAlias)
  }

  z_indices <- pDistMatch_euclidean(embedx, embedy,
                                    MaxDist = MaxDist)
  colnames(y) <- paste(colnames(y), '.y', sep = "")
  colnames(x) <- paste(colnames(x), '.x', sep = "")
  z <- cbind( x[z_indices$ix,], y[z_indices$iy,] )
  z <- as.data.frame( cbind( x[z_indices[,"ix"],], y[z_indices[,"iy"],] ) )
  z$stringdist <- z_indices[,"stringdist"]
  gc(); return( DeconflictNames( z ) )
}
