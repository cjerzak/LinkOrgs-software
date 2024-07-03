zoomerjoin_euclidean_join <- function(
  x = NULL, embedx = NULL,
  y = NULL, embedy = NULL,
  MaxDist = NULL,
  AveMatchNumberPerAlias = NULL,
  ...
) {

  if(!is.null(AveMatchNumberPerAlias)){
    warning("Setting AveMatchNumberPerAlias may result in long runtimes when using zoomerjoin")

    threshold <- GetCalibratedDistThres(x = embedy,
                                      y = embedx,
                                      AveMatchNumberPerAlias = AveMatchNumberPerAlias)

  } else {
      threshold <- MaxDist
  }

    # Find zoomerjoin hyperparameters that xyz in time
 hyperparameters <- euclidean_hyper_search(3*threshold, threshold, .01, .995)

 n_bands <- hyperparameters$n_bands
 band_width <- hyperparameters$band_width
 r <- hyperparameters$r

  match_table <- zoomerjoin:::rust_p_norm_join(
    a_mat = as.matrix(embedx),
    b_mat = as.matrix(embedy),
    radius = threshold,
    band_width = band_width,
    n_bands = n_bands,
    r = r,
    progress = FALSE,
    seed = round(runif(1, 0, 2^32))
  )

  colnames(y) <- paste(colnames(y), ".y", sep = "")
  colnames(x) <- paste(colnames(x), ".x", sep = "")

  z <- cbind(x[match_table[,1],], y[match_table[,2],])
  return(z)
}
