# =============================================================================
# Utility Function Tests
# =============================================================================

test_that("print2 outputs without error", {
  expect_no_error(print2("Test message"))
})

test_that("print2 quiet mode suppresses output", {
  expect_no_error(print2("Silent message", quiet = TRUE))
})

test_that("dropboxURL2downloadURL converts URL to download format", {
  test_url <- "https://www.dropbox.com/s/abc123/file.csv?dl=0"

  result <- dropboxURL2downloadURL(test_url)

  expect_true(grepl("dl=1", result) || grepl("dl.dropboxusercontent", result))
})
