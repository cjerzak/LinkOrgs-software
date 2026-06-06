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

test_that("url2dt reads local zipped CSV files", {
  skip_if_not(nzchar(Sys.which("zip")), "zip command is not available")

  tmp_dir <- tempdir()
  csv_file <- file.path(tmp_dir, "url2dt_zip_test.csv")
  zip_file <- file.path(tmp_dir, "url2dt_zip_test.csv.zip")
  write.csv(data.frame(a = 1:2, b = c("x", "y")), csv_file, row.names = FALSE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp_dir)
  utils::zip(zipfile = zip_file, files = basename(csv_file), flags = "-q")

  result <- url2dt(zip_file)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(result$a, 1:2)
})

test_that("url2dt reads local gzipped CSV files", {
  gz_file <- tempfile(fileext = ".csv.gz")
  con <- gzfile(gz_file, open = "wt")
  writeLines(c("a,b", "1,x", "2,y"), con)
  close(con)

  result <- url2dt(gz_file)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(result$a, 1:2)
})

test_that("LinkOrgs cache honors LINKORGS_CACHE_DIR", {
  cache_dir <- tempfile("linkorgs-cache-")
  old_cache <- Sys.getenv("LINKORGS_CACHE_DIR", unset = NA)
  on.exit({
    if(is.na(old_cache)){
      Sys.unsetenv("LINKORGS_CACHE_DIR")
    } else {
      Sys.setenv(LINKORGS_CACHE_DIR = old_cache)
    }
    unlink(cache_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  Sys.setenv(LINKORGS_CACHE_DIR = cache_dir)

  result <- LinkOrgs:::LinkOrgsCacheDir()

  expect_true(dir.exists(result))
  expect_equal(normalizePath(result, winslash = "/", mustWork = TRUE),
               normalizePath(cache_dir, winslash = "/", mustWork = TRUE))
})

test_that("bundled bipartite network data is resolved before download", {
  result <- LinkOrgs:::LinkOrgsNetworkDirectory(
    algorithm = "bipartite",
    url = "https://invalid.example/no-download.zip",
    cache_dir = tempfile("unused-linkorgs-cache-"),
    quiet = TRUE
  )

  expect_true(dir.exists(result))
  expect_true(file.exists(file.path(
    result,
    "directory_data_bipartite_thresh40",
    "LinkIt_directory_bipartite.Rdata"
  )))
})
