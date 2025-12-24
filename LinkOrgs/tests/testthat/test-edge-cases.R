# =============================================================================
# Edge Case Tests
# =============================================================================

test_that("Empty data frame x returns empty result", {
  df_empty <- data.frame(name = character(0))
  df_normal <- data.frame(name = c("Apple", "Google"))

  result <- pFuzzyMatch_discrete(
    x = df_empty, y = df_normal,
    by.x = "name", by.y = "name",
    MaxDist = 0.5
  )

  expect_equal(nrow(result), 0)
})

test_that("Empty data frame y returns empty result", {
  df_empty <- data.frame(name = character(0))
  df_normal <- data.frame(name = c("Apple", "Google"))

  result <- pFuzzyMatch_discrete(
    x = df_normal, y = df_empty,
    by.x = "name", by.y = "name",
    MaxDist = 0.5
  )

  expect_equal(nrow(result), 0)
})

test_that("Single row x works", {
  df_single <- data.frame(name = "Apple")
  df_normal <- data.frame(name = c("Apple", "Google"))

  expect_no_error(
    pFuzzyMatch_discrete(
      x = df_single, y = df_normal,
      by.x = "name", by.y = "name",
      MaxDist = 0.5
    )
  )
})

test_that("Single row y works", {
  df_single <- data.frame(name = "Apple")
  df_normal <- data.frame(name = c("Apple", "Google"))

  expect_no_error(
    pFuzzyMatch_discrete(
      x = df_normal, y = df_single,
      by.x = "name", by.y = "name",
      MaxDist = 0.5
    )
  )
})

test_that("NA values in columns do not cause crash", {
  df_with_na <- data.frame(name = c("Apple", NA, "Google"))
  df_normal <- data.frame(name = c("Apple", "Google"))

  expect_no_error(
    pFuzzyMatch_discrete(
      x = df_with_na, y = df_normal,
      by.x = "name", by.y = "name",
      MaxDist = 0.5
    )
  )
})

test_that("Duplicate rows are handled", {
  df_dups <- data.frame(name = c("Apple Incorporated Company", "Apple Incorporated Company", "Google Corporation Limited"))
  df_normal_long <- data.frame(name = c("Apple Incorporated Company", "Google Corporation Limited"))

  expect_no_error(
    pFuzzyMatch_discrete(
      x = df_dups, y = df_normal_long,
      by.x = "name", by.y = "name",
      MaxDist = 0.5
    )
  )
})

test_that("Empty strings after preprocessing do not cause crash", {
  df_spaces <- data.frame(name = c("   ", "Apple"))
  df_normal <- data.frame(name = c("Apple", "Google"))

  expect_no_error(
    pFuzzyMatch_discrete(
      x = df_spaces, y = df_normal,
      by.x = "name", by.y = "name",
      MaxDist = 0.5,
      NormalizeSpaces = TRUE
    )
  )
})

test_that("Very long strings complete without timeout", {
  long_string <- paste(rep("verylongword", 100), collapse = " ")
  df_long <- data.frame(name = long_string)
  df_normal <- data.frame(name = c("Apple", "Google"))

  expect_no_error(
    pFuzzyMatch_discrete(
      x = df_long, y = df_normal,
      by.x = "name", by.y = "name",
      MaxDist = 1
    )
  )
})
