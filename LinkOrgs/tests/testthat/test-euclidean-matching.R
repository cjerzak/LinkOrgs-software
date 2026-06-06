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

test_that("Custom Euclidean distance metric is honored", {
  custom_metric <- function(an_x, candidate_y) {
    rep(0, ncol(candidate_y))
  }
  embed_x <- matrix(0, nrow = 1, ncol = 2)
  embed_y <- matrix(100, nrow = 1, ncol = 2)

  result <- pDistMatch_euclidean(
    embedx = embed_x,
    embedy = embed_y,
    MaxDist = 1,
    embedDistMetric = custom_metric,
    ReturnProgress = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$stringdist, 0)
})

test_that("pFuzzyMatch_euclidean passes custom distance metric through", {
  custom_metric <- function(an_x, candidate_y) {
    rep(0, ncol(candidate_y))
  }
  x <- data.frame(name = "left")
  y <- data.frame(name = "right")
  embed_x <- matrix(0, nrow = 1, ncol = 2)
  embed_y <- matrix(100, nrow = 1, ncol = 2)

  result <- pFuzzyMatch_euclidean(
    x = x,
    y = y,
    by.x = "name",
    by.y = "name",
    embedx = embed_x,
    embedy = embed_y,
    MaxDist = 1,
    embedDistMetric = custom_metric,
    ReturnProgress = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$stringdist, 0)
})

test_that("Euclidean matching supports fewer than ten embedding dimensions", {
  embed_x <- matrix(c(0, 0), nrow = 1)
  embed_y <- matrix(c(0, 0), nrow = 1)

  result <- pDistMatch_euclidean(
    embedx = embed_x,
    embedy = embed_y,
    MaxDist = 1,
    ReturnProgress = FALSE
  )

  expect_equal(nrow(result), 1)
})
