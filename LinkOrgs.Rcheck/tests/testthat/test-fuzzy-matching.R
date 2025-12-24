# =============================================================================
# Fuzzy Matching (Discrete) Tests
# =============================================================================

test_that("Exact match has distance 0", {
  df_exact_x <- data.frame(name = "test company")
  df_exact_y <- data.frame(name = "test company")

  result <- pFuzzyMatch_discrete(
    x = df_exact_x, y = df_exact_y,
    by.x = "name", by.y = "name",
    MaxDist = 0.5
  )

  expect_true(nrow(result) > 0)
  expect_equal(result$stringdist[1], 0)
})

test_that("Near match returns small distance", {
  df_near_x <- data.frame(name = "apple incorporated company")
  df_near_y <- data.frame(name = "appple incorporated company")  # One extra 'p'

  result <- pFuzzyMatch_discrete(
    x = df_near_x, y = df_near_y,
    by.x = "name", by.y = "name",
    MaxDist = 0.5
  )

  expect_true(nrow(result) > 0)
  expect_gte(result$stringdist[1], 0)
  expect_lt(result$stringdist[1], 0.5)
})

test_that("Strict threshold returns no matches for different strings", {
  result <- pFuzzyMatch_discrete(
    x = data.frame(name = "completely different"),
    y = data.frame(name = "xyz abc"),
    by.x = "name", by.y = "name",
    MaxDist = 0.01
  )

  expect_equal(nrow(result), 0)
})

test_that("Multiple matches are returned with loose threshold", {
  df_x_multi <- data.frame(name = c("apple", "apple inc"))
  df_y_multi <- data.frame(name = c("apple", "apple company", "appple"))

  result <- pFuzzyMatch_discrete(
    x = df_x_multi, y = df_y_multi,
    by.x = "name", by.y = "name",
    MaxDist = 0.8
  )

  expect_gt(nrow(result), 2)
})

test_that("Jaccard distance returns numeric distance", {
  df_exact_x <- data.frame(name = "test company")
  df_exact_y <- data.frame(name = "test company")

  result <- pFuzzyMatch_discrete(
    x = df_exact_x, y = df_exact_y,
    by.x = "name", by.y = "name",
    MaxDist = 0.5, DistanceMeasure = "jaccard"
  )

  expect_true(nrow(result) > 0)
  expect_type(result$stringdist, "double")
})

test_that("OSA distance for 1 insertion is 1", {
  df_near_x <- data.frame(name = "apple incorporated company")
  df_near_y <- data.frame(name = "appple incorporated company")

  result <- pFuzzyMatch_discrete(
    x = df_near_x, y = df_near_y,
    by.x = "name", by.y = "name",
    MaxDist = 5, DistanceMeasure = "osa"
  )

  expect_true(nrow(result) > 0)
  expect_equal(result$stringdist[1], 1)
})

test_that("Jaro-Winkler distance for similar strings is small", {
  df_near_x <- data.frame(name = "apple incorporated company")
  df_near_y <- data.frame(name = "appple incorporated company")

  result <- pFuzzyMatch_discrete(
    x = df_near_x, y = df_near_y,
    by.x = "name", by.y = "name",
    MaxDist = 0.5, DistanceMeasure = "jw"
  )

  expect_true(nrow(result) > 0)
  expect_gt(result$stringdist[1], 0)
  expect_lt(result$stringdist[1], 0.5)
})
