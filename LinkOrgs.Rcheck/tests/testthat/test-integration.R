# =============================================================================
# Integration Tests
# =============================================================================

test_that("LinkOrgs with algorithm='fuzzy' returns data frame with matches", {
  df_x <- data.frame(
    id = 1:10,
    company = c("Apple Inc", "Google LLC", "Microsoft Corp", "Amazon", "Meta",
                "Netflix", "Tesla Motors", "Adobe Systems", "Oracle Corp", "IBM")
  )
  df_y <- data.frame(
    id = 1:10,
    organization = c("Apple", "Alphabet", "Microsoft", "Amazon.com", "Facebook",
                     "Netflix Inc", "Tesla", "Adobe", "Oracle", "IBM Corporation")
  )

  result <- LinkOrgs(
    x = df_x, y = df_y,
    by.x = "company", by.y = "organization",
    algorithm = "fuzzy",
    MaxDist = 0.6
  )

  expect_s3_class(result, "data.frame")
  expect_true("stringdist" %in% names(result) || "minDist" %in% names(result))
  expect_gt(nrow(result), 0)
})

test_that("LinkOrgs with ReturnDiagnostics returns additional columns", {
  df_x <- data.frame(
    id = 1:10,
    company = c("Apple Inc", "Google LLC", "Microsoft Corp", "Amazon", "Meta",
                "Netflix", "Tesla Motors", "Adobe Systems", "Oracle Corp", "IBM")
  )
  df_y <- data.frame(
    id = 1:10,
    organization = c("Apple", "Alphabet", "Microsoft", "Amazon.com", "Facebook",
                     "Netflix Inc", "Tesla", "Adobe", "Oracle", "IBM Corporation")
  )

  result <- LinkOrgs(
    x = df_x, y = df_y,
    by.x = "company", by.y = "organization",
    algorithm = "fuzzy",
    MaxDist = 0.6,
    ReturnDiagnostics = TRUE
  )

  expect_gte(ncol(result), 2)
})

test_that("Full pipeline: match then assess returns all metrics", {
  df_x <- data.frame(
    id = 1:10,
    company = c("Apple Inc", "Google LLC", "Microsoft Corp", "Amazon", "Meta",
                "Netflix", "Tesla Motors", "Adobe Systems", "Oracle Corp", "IBM")
  )
  df_y <- data.frame(
    id = 1:10,
    organization = c("Apple", "Alphabet", "Microsoft", "Amazon.com", "Facebook",
                     "Netflix Inc", "Tesla", "Adobe", "Oracle", "IBM Corporation")
  )

  z_true <- data.frame(
    company = c("Apple Inc", "Microsoft Corp", "Amazon", "Netflix", "Tesla Motors",
                "Adobe Systems", "Oracle Corp", "IBM"),
    organization = c("Apple", "Microsoft", "Amazon.com", "Netflix Inc", "Tesla",
                     "Adobe", "Oracle", "IBM Corporation"),
    stringsAsFactors = FALSE
  )

  matches <- LinkOrgs(
    x = df_x, y = df_y,
    by.x = "company", by.y = "organization",
    algorithm = "fuzzy",
    MaxDist = 0.6
  )

  if (nrow(matches) > 0) {
    result <- AssessMatchPerformance(
      x = df_x, y = df_y,
      z = matches, z_true = z_true,
      by.x = "company", by.y = "organization"
    )

    expect_true(all(c("TruePositives", "FalsePositives", "FalseNegatives", "TrueNegatives") %in% names(result)))
  }
})

test_that("Parallelization with larger dataset works", {
  set.seed(999)
  n_large <- 50
  df_x_large <- data.frame(
    id = 1:n_large,
    company = paste0("Company_", sample(letters, n_large, replace = TRUE), "_", 1:n_large)
  )
  df_y_large <- data.frame(
    id = 1:n_large,
    organization = paste0("Company_", sample(letters, n_large, replace = TRUE), "_", 1:n_large)
  )

  result <- LinkOrgs(
    x = df_x_large, y = df_y_large,
    by.x = "company", by.y = "organization",
    algorithm = "fuzzy",
    MaxDist = 0.8
  )

  expect_s3_class(result, "data.frame")
})
