# =============================================================================
# LinkOrgs Workflow Tests
# =============================================================================

test_that("LinkOrgs returns an empty data frame when fuzzy matching finds no pairs", {
  result <- LinkOrgs(
    x = data.frame(org = "apple"),
    y = data.frame(org = "zzzz"),
    by = "org",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("org", "stringdist", "minDist") %in% names(result)))
})

test_that("LinkOrgs handles empty inputs without error", {
  empty_x <- LinkOrgs(
    x = data.frame(org = character(0)),
    y = data.frame(org = "apple"),
    by = "org",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ReturnProgress = FALSE,
    nCores = 1
  )
  empty_y <- LinkOrgs(
    x = data.frame(org = "apple"),
    y = data.frame(org = character(0)),
    by = "org",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_equal(nrow(empty_x), 0)
  expect_equal(nrow(empty_y), 0)
})

test_that("LinkOrgs hashes NA keys without creating invalid row names", {
  result <- LinkOrgs(
    x = data.frame(org = c("apple", NA)),
    y = data.frame(org = "apple"),
    by = "org",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$org, "apple")
})

test_that("LinkOrgs can match with precomputed embeddings", {
  x <- data.frame(id = 1:2, org = c("Apple Inc.", "Microsoft Corp"))
  y <- data.frame(code = c("a", "m"), name = c("apple", "microsoft"))
  embedx <- matrix(c(0, 0, 10, 10), nrow = 2, byrow = TRUE)
  embedy <- matrix(c(0, 0, 10, 10), nrow = 2, byrow = TRUE)

  result <- LinkOrgs(
    x = x,
    y = y,
    by.x = "org",
    by.y = "name",
    embedx = embedx,
    embedy = embedy,
    MaxDist = 0.1,
    AveMatchNumberPerAlias = NULL,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$org, x$org)
  expect_equal(result$name, y$name)
  expect_true(all(c("id", "code", "stringdist") %in% names(result)))
})

test_that("LinkOrgs by shorthand restores original names and minDist for one match", {
  result <- LinkOrgs(
    x = data.frame(id = 1, org = "Apple, Inc."),
    y = data.frame(code = "a", org = "apple inc"),
    by = "org",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$org, "Apple, Inc.")
  expect_equal(result$stringdist, 0)
  expect_equal(result$minDist, 0)
  expect_true(all(c("id", "code", "org", "stringdist", "minDist") %in% names(result)))
})

test_that("ReturnDiagnostics includes internal identifiers while default output hides them", {
  x <- data.frame(id = 1, org = "apple")
  y <- data.frame(code = "a", org = "apple")

  default_result <- LinkOrgs(
    x = x,
    y = y,
    by = "org",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ReturnProgress = FALSE,
    nCores = 1
  )
  diagnostic_result <- LinkOrgs(
    x = x,
    y = y,
    by = "org",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ReturnDiagnostics = TRUE,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_false("UniversalMatchCol" %in% names(default_result))
  expect_false("XYref__ID" %in% names(default_result))
  expect_true(all(c("UniversalMatchCol", "XYref__ID") %in% names(diagnostic_result)))
})

test_that("ReturnDecomposition includes raw and network slots for fuzzy matching", {
  result <- LinkOrgs(
    x = data.frame(org = c("apple", "microsoft")),
    y = data.frame(org = c("apple", "microsoft")),
    by = "org",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ReturnProgress = FALSE,
    ReturnDecomposition = TRUE,
    nCores = 1
  )

  expect_named(result, c("z", "z_RawNames", "z_Network"))
  expect_s3_class(result$z, "data.frame")
  expect_s3_class(result$z_RawNames, "data.frame")
  expect_null(result$z_Network)
})

test_that("ReturnProgress restores the LinkOrgs quiet option", {
  old_quiet <- getOption("LinkOrgs.quiet", NULL)
  on.exit({
    if(is.null(old_quiet)){
      options(LinkOrgs.quiet = NULL)
    } else {
      options(LinkOrgs.quiet = old_quiet)
    }
  }, add = TRUE)
  options(LinkOrgs.quiet = FALSE)

  LinkOrgs(
    x = data.frame(org = "apple"),
    y = data.frame(org = "apple"),
    by = "org",
    algorithm = "fuzzy",
    MaxDist = 0,
    AveMatchNumberPerAlias = NULL,
    ReturnProgress = FALSE,
    nCores = 1
  )

  expect_false(getOption("LinkOrgs.quiet"))
})
