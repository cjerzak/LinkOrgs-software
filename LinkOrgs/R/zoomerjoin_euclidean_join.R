zoomerjoin_euclidean_join <- function(
  x = NULL, embedx = NULL,
  y = NULL, embedy = NULL,
  MaxDist = NULL,
  AveMatchNumberPerAlias = NULL,
  n_bands = 40L,
  band_width = 5L,
  r = 5L,
  ...
) {

  stopifnot(
    "AveMatchNumberPerAlias not supported for zoomerjoin" =
      !is.null(AveMatchNumberPerAlias)
  )

  match_table <- zoomerjoin::rust_p_norm_join(
    a_mat = as.matrix(embedx),
    b_mat = as.matrix(embedy),
    radius = MaxDist,
    band_width = band_width,
    n_bands = n_bands,
    r = r,
    progress = FALSE,
    seed = round(runif(1, 0, 2^32))
  )

  colnames(y) <- paste(colnames(y), ".y", sep = "")
  colnames(x) <- paste(colnames(x), ".x", sep = "")

  z <- cbind(x[match_table[,1],], y[match_table[,2],])
  return(DeconflictNames(z))
}
