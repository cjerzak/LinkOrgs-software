# =============================================================================
# Distance Calibration Tests
# =============================================================================

test_that("Basic euclidean calibration returns positive threshold", {
  set.seed(789)
  n_dim <- 20
  n_rows <- 100
  embed_x <- as.data.frame(matrix(rnorm(n_dim * n_rows), nrow = n_rows))
  embed_y <- as.data.frame(matrix(rnorm(n_dim * n_rows), nrow = n_rows))

  result <- GetCalibratedDistThres(
    x = embed_x, y = embed_y,
    AveMatchNumberPerAlias = 5,
    mode = "euclidean"
  )

  expect_true(is.numeric(result))
  expect_gt(result, 0)
})

test_that("Basic discrete calibration returns non-negative threshold", {
  df_x <- data.frame(name = paste0("company_", 1:50))
  df_y <- data.frame(name = paste0("company_", 1:50))

  result <- GetCalibratedDistThres(
    x = df_x, y = df_y,
    by.x = "name", by.y = "name",
    AveMatchNumberPerAlias = 5,
    mode = "discrete",
    DistanceMeasure = "jaccard"
  )

  expect_true(is.numeric(result))
  expect_gte(result, 0)
})

test_that("Larger AveMatchNumberPerAlias gives larger threshold", {
  set.seed(789)
  n_dim <- 20
  n_rows <- 100
  embed_x <- as.data.frame(matrix(rnorm(n_dim * n_rows), nrow = n_rows))
  embed_y <- as.data.frame(matrix(rnorm(n_dim * n_rows), nrow = n_rows))

  result_small <- GetCalibratedDistThres(
    x = embed_x, y = embed_y,
    AveMatchNumberPerAlias = 1,
    mode = "euclidean"
  )

  result_large <- GetCalibratedDistThres(
    x = embed_x, y = embed_y,
    AveMatchNumberPerAlias = 20,
    mode = "euclidean"
  )

  expect_lte(result_small, result_large)
})

test_that("Minimum threshold clamp works", {
  set.seed(789)
  n_dim <- 20
  n_rows <- 100
  embed_x <- as.data.frame(matrix(rnorm(n_dim * n_rows), nrow = n_rows))
  embed_y <- as.data.frame(matrix(rnorm(n_dim * n_rows), nrow = n_rows))

  result <- GetCalibratedDistThres(
    x = embed_x, y = embed_y,
    AveMatchNumberPerAlias = 0.001,
    mode = "euclidean"
  )

  expect_gte(result, 1e-6)
})

test_that("Small sample returns valid threshold", {
  set.seed(789)
  n_dim <- 20
  small_embed_x <- as.data.frame(matrix(rnorm(n_dim * 5), nrow = 5))
  small_embed_y <- as.data.frame(matrix(rnorm(n_dim * 5), nrow = 5))

  result <- GetCalibratedDistThres(
    x = small_embed_x, y = small_embed_y,
    AveMatchNumberPerAlias = 2,
    mode = "euclidean"
  )

  expect_true(is.numeric(result))
})
