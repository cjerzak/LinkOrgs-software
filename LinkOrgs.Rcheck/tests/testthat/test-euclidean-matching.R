# =============================================================================
# Euclidean Matching Tests
# =============================================================================

test_that("Identical embeddings have distance ~0", {
  set.seed(123)
  n_dim <- 10
  embed_identical <- matrix(rnorm(n_dim), nrow = 1)
  df_x <- as.data.frame(embed_identical)
  df_y <- as.data.frame(embed_identical)

  result <- pDistMatch_euclidean(
    embedx = df_x, embedy = df_y,
    MaxDist = 1
  )

  expect_true(nrow(result) > 0)
  expect_lt(abs(result$stringdist[1]), 1e-10)
})

test_that("Distance is sqrt(2) for orthogonal unit vectors", {
  embed_x <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
  embed_y <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
  expected_dist <- sqrt(2)

  result <- pDistMatch_euclidean(
    embedx = as.data.frame(embed_x),
    embedy = as.data.frame(embed_y),
    MaxDist = 2
  )

  expect_true(nrow(result) > 0)
  expect_lt(abs(result$stringdist[1] - expected_dist), 0.001)
})

test_that("Tighter threshold returns fewer or equal matches", {
  set.seed(123)
  n_dim <- 10
  embed_x <- matrix(rnorm(n_dim * 5), nrow = 5)
  embed_y <- matrix(rnorm(n_dim * 5), nrow = 5)

  result_tight <- pDistMatch_euclidean(
    embedx = as.data.frame(embed_x),
    embedy = as.data.frame(embed_y),
    MaxDist = 0.001
  )

  result_loose <- pDistMatch_euclidean(
    embedx = as.data.frame(embed_x),
    embedy = as.data.frame(embed_y),
    MaxDist = 1000
  )

  expect_lte(nrow(result_tight), nrow(result_loose))
})

test_that("Large dataset (100 rows) returns matches", {
  set.seed(456)
  n_dim <- 10
  large_embed_x <- as.data.frame(matrix(rnorm(n_dim * 100), nrow = 100))
  large_embed_y <- as.data.frame(matrix(rnorm(n_dim * 100), nrow = 100))

  result <- pDistMatch_euclidean(
    embedx = large_embed_x,
    embedy = large_embed_y,
    MaxDist = 10
  )

  expect_gt(nrow(result), 0)
})
