pFuzzyMatch_discrete <- function(
    x, by.x, embedx = NULL,
    y, by.y, embedy = NULL,
    MaxDist = NULL,
    qgram =2, DistanceMeasure = "jaccard",
    AveMatchNumberPerAlias = NULL,
    ...
){
  if(!is.null(AveMatchNumberPerAlias)){
    MaxDist <- GetCalibratedDistThres(x = x, by.x = by.x,
                                      y = y, by.y = by.y,
                                      AveMatchNumberPerAlias = AveMatchNumberPerAlias,
                                      qgram = qgram,
                                      DistanceMeasure = DistanceMeasure,
                                      mode = "discrete" )
  }

  z_indices <- pDistMatch_discrete(x, by.x = by.x,
                                      y, by.y = by.y,
                                      qgram = qgram,
                                      DistanceMeasure = DistanceMeasure,
                                      MaxDist = MaxDist)
  colnames(y) <- paste(colnames(y), '.y', sep = "")
  colnames(x) <- paste(colnames(x), '.x', sep = "")
  z <- cbind( x[z_indices$ix,], y[z_indices$iy,] )
  z$stringdist <- z_indices$stringdist
  gc(); return( as.data.frame( z ) )
}
