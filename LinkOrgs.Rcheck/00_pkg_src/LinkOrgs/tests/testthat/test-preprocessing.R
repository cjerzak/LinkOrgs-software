# =============================================================================
# Text Preprocessing Tests
# =============================================================================

test_that("ToLower conversion works", {
  df_upper <- data.frame(name = "MICROSOFT CORPORATION INTERNATIONAL")
  df_lower <- data.frame(name = "microsoft corporation international")

  expect_no_error(
    pFuzzyMatch_discrete(
      x = df_upper, y = df_lower,
      by.x = "name", by.y = "name",
      MaxDist = 0.5, ToLower = TRUE
    )
  )
})

test_that("Punctuation removal works", {
  df_punct <- data.frame(name = "Microsoft. Corporation! International?")
  df_clean <- data.frame(name = "microsoft corporation international")

  expect_no_error(
    pFuzzyMatch_discrete(
      x = df_punct, y = df_clean,
      by.x = "name", by.y = "name",
      MaxDist = 0.5, RemovePunctuation = TRUE, ToLower = TRUE
    )
  )
})

test_that("Space normalization works", {
  df_spaces <- data.frame(name = "Microsoft   Corporation    International")
  df_clean <- data.frame(name = "microsoft corporation international")

  expect_no_error(
    pFuzzyMatch_discrete(
      x = df_spaces, y = df_clean,
      by.x = "name", by.y = "name",
      MaxDist = 0.5, NormalizeSpaces = TRUE, ToLower = TRUE
    )
  )
})

test_that("Case-sensitive matching returns no match for different cases", {
  df_upper <- data.frame(name = "MICROSOFT CORPORATION INTERNATIONAL")
  df_lower <- data.frame(name = "microsoft corporation international")

  result <- pFuzzyMatch_discrete(
    x = df_upper, y = df_lower,
    by.x = "name", by.y = "name",
    MaxDist = 0.01, ToLower = FALSE
  )

  expect_equal(nrow(result), 0)
})
