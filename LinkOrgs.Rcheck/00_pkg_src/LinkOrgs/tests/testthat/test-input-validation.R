# =============================================================================
# Input Validation Tests
# =============================================================================

test_that("LinkOrgs rejects non-data.frame x input", {
  df_y <- data.frame(id = 1:5, org = c("Apple", "Google", "Microsoft", "Amazon", "Meta"))

  expect_error(
    LinkOrgs(x = "not a dataframe", y = df_y, by.x = "name", by.y = "org", algorithm = "fuzzy"),
    regexp = "data.frame"
  )
})

test_that("LinkOrgs rejects non-data.frame y input", {
  df_x <- data.frame(id = 1:5, name = c("Apple Inc", "Google LLC", "Microsoft Corp", "Amazon", "Meta"))

  expect_error(
    LinkOrgs(x = df_x, y = c(1, 2, 3), by.x = "name", by.y = "org", algorithm = "fuzzy"),
    regexp = "data.frame"
  )
})

test_that("LinkOrgs rejects missing by.x column", {
  df_x <- data.frame(id = 1:5, name = c("Apple Inc", "Google LLC", "Microsoft Corp", "Amazon", "Meta"))
  df_y <- data.frame(id = 1:5, org = c("Apple", "Google", "Microsoft", "Amazon", "Meta"))

  expect_error(
    LinkOrgs(x = df_x, y = df_y, by.x = "nonexistent", by.y = "org", algorithm = "fuzzy"),
    regexp = "not found|not exist|column"
  )
})

test_that("LinkOrgs rejects missing by.y column", {
  df_x <- data.frame(id = 1:5, name = c("Apple Inc", "Google LLC", "Microsoft Corp", "Amazon", "Meta"))
  df_y <- data.frame(id = 1:5, org = c("Apple", "Google", "Microsoft", "Amazon", "Meta"))

  expect_error(
    LinkOrgs(x = df_x, y = df_y, by.x = "name", by.y = "nonexistent", algorithm = "fuzzy"),
    regexp = "not found|not exist|column"
  )
})

test_that("LinkOrgs rejects invalid algorithm", {
  df_x <- data.frame(id = 1:5, name = c("Apple Inc", "Google LLC", "Microsoft Corp", "Amazon", "Meta"))
  df_y <- data.frame(id = 1:5, org = c("Apple", "Google", "Microsoft", "Amazon", "Meta"))

  expect_error(
    LinkOrgs(x = df_x, y = df_y, by.x = "name", by.y = "org", algorithm = "invalid_algo"),
    regexp = "algorithm"
  )
})
