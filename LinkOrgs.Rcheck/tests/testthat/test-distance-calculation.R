# =============================================================================
# Distance Calculation Tests
# Tests for validating distance calculations used in matching
# =============================================================================

test_that("GetCalibratedDistThres with pDistMatch_euclidean returns expected match count", {
  set.seed(42)
  n_dim <- 49
  n_rows <- 100

  x <- as.data.frame(matrix(round(rnorm(n_rows * n_dim), 5), ncol = n_dim))
  y <- as.data.frame(matrix(round(rnorm(n_rows * n_dim), 5), ncol = n_dim))

  AveMatchNumberPerAlias <- 5L

  CalibratedThres <- GetCalibratedDistThres(
    x, y,
    AveMatchNumberPerAlias = AveMatchNumberPerAlias,
    mode = "euclidean"
  )

  expect_true(is.numeric(CalibratedThres))
  expect_gt(CalibratedThres, 0)

  joined_xy_exact <- pDistMatch_euclidean(x, y, MaxDist = CalibratedThres)

  expect_s3_class(joined_xy_exact, "data.frame")
  expect_true("stringdist" %in% names(joined_xy_exact))
  expect_true(all(c("ix", "iy") %in% names(joined_xy_exact)))

  # Expected matches should be approximately nrow(x) * AveMatchNumberPerAlias
  expected_matches <- nrow(x) * AveMatchNumberPerAlias
  # Allow for some variance (50-150% of expected)
  expect_gte(nrow(joined_xy_exact), expected_matches * 0.5)
  expect_lte(nrow(joined_xy_exact), expected_matches * 1.5)
})

test_that("pDistMatch_euclidean distances match base R dist() calculation", {
  set.seed(123)
  n_dim <- 10
  n_rows <- 5

  x <- as.data.frame(matrix(rnorm(n_rows * n_dim), ncol = n_dim))
  y <- as.data.frame(matrix(rnorm(n_rows * n_dim), ncol = n_dim))

  result <- pDistMatch_euclidean(x, y, MaxDist = Inf)

  # Calculate expected distances using base R
  expected_dist_matrix <- as.matrix(dist(rbind(x, y)))[1:nrow(x), (1 + nrow(x)):(nrow(x) + nrow(y))]

  # Verify all distances are present
  expect_equal(nrow(result), nrow(x) * nrow(y))

  # Sample a few pairs and verify distances match
  for (i in 1:min(5, nrow(result))) {
    x_idx <- result$ix[i]
    y_idx <- result$iy[i]
    pkg_dist <- result$stringdist[i]
    expected_dist <- expected_dist_matrix[x_idx, y_idx]
    expect_lt(abs(pkg_dist - expected_dist), 1e-6)
  }
})

test_that("Distance threshold filtering works correctly", {
  set.seed(456)
  n_dim <- 10
  n_rows <- 20

  x <- as.data.frame(matrix(rnorm(n_rows * n_dim), ncol = n_dim))
  y <- as.data.frame(matrix(rnorm(n_rows * n_dim), ncol = n_dim))

  # Get calibrated threshold for specific match count
  threshold <- GetCalibratedDistThres(
    x, y,
    AveMatchNumberPerAlias = 3,
    mode = "euclidean"
  )

  result <- pDistMatch_euclidean(x, y, MaxDist = threshold)

  # Verify all returned distances are at or below threshold
  expect_true(all(result$stringdist <= threshold))
})
