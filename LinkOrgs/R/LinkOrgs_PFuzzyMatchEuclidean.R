pFuzzyMatch_euclidean <- function(
    x, by.x, embedx = NULL,
    y, by.y, embedy = NULL,
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
  z$stringdist <- z_indices$stringdist
  gc(); return( as.data.frame( z ) )
}
