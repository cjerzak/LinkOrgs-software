pFuzzyMatch_euclidean <- function(
    x, by.x, embedx = NULL,
    y, by.y, embedy = NULL,
    MaxDist = NULL,
    AveMatchNumberPerAlias = NULL
){
  if(!is.null(AveMatchNumberPerAlias)){
    MaxDist <- GetCalibratedDistThres(x = embedy,
                                      y = embedx,
                                      AveMatchNumberPerAlias = AveMatchNumberPerAlias)
  }

  z_ml_indices <- pDistMatch(embedx, embedy, AcceptThreshold = MaxDist)
  colnames(y) <- paste(colnames(y), '.y', sep = "")
  colnames(x) <- paste(colnames(x), '.x', sep = "")
  z_ml <- cbind( x[z_ml_indices$ix,],
                 y[z_ml_indices$iy,] )
  z_ml$stringdist.x <- z_ml$stringdist.y <- z_ml_indices$dist

  gc(); return( as.data.frame( z_ml ) )
}
