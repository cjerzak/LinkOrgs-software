# =============================================================================
# Text Preprocessing Tests
# =============================================================================

test_that("ToLower conversion works", {
  df_upper <- data.frame(name = "MICROSOFT CORPORATION INTERNATIONAL")
  df_lower <- data.frame(name = "microsoft corporation international")

  result <- LinkOrgs(
    x = df_upper, y = df_lower,
    by = "name",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ToLower = TRUE,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_equal(nrow(result), 1)
})

test_that("Punctuation removal works", {
  df_punct <- data.frame(name = "Microsoft. Corporation! International?")
  df_clean <- data.frame(name = "microsoft corporation international")

  result <- LinkOrgs(
    x = df_punct, y = df_clean,
    by = "name",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    RemovePunctuation = TRUE,
    ToLower = TRUE,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_equal(nrow(result), 1)
})

test_that("Space normalization works", {
  df_spaces <- data.frame(name = "Microsoft   Corporation    International")
  df_clean <- data.frame(name = "microsoft corporation international")

  result <- LinkOrgs(
    x = df_spaces, y = df_clean,
    by = "name",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    NormalizeSpaces = TRUE,
    ToLower = TRUE,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_equal(nrow(result), 1)
})

test_that("Case-sensitive matching returns no match for different cases", {
  df_upper <- data.frame(name = "MICROSOFT CORPORATION INTERNATIONAL")
  df_lower <- data.frame(name = "microsoft corporation international")

  result <- LinkOrgs(
    x = df_upper, y = df_lower,
    by = "name",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ToLower = FALSE,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_equal(nrow(result), 0)
})

test_that("punctuation and whitespace options can preserve differences", {
  df_dirty <- data.frame(name = "Microsoft.   Corporation")
  df_clean <- data.frame(name = "microsoft corporation")

  no_punctuation_cleanup <- LinkOrgs(
    x = df_dirty, y = df_clean,
    by = "name",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ToLower = TRUE,
    NormalizeSpaces = TRUE,
    RemovePunctuation = FALSE,
    ReturnProgress = FALSE,
    nCores = 1
  )
  no_space_cleanup <- LinkOrgs(
    x = df_dirty, y = df_clean,
    by = "name",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ToLower = TRUE,
    NormalizeSpaces = FALSE,
    RemovePunctuation = TRUE,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_equal(nrow(no_punctuation_cleanup), 0)
  expect_equal(nrow(no_space_cleanup), 0)
})
