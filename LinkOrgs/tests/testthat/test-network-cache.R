# =============================================================================
# Network and Cache Tests
# =============================================================================

test_that("LinkOrgsDownload creates destination directories", {
  skip_on_os("windows")
  source_file <- tempfile("linkorgs-source-")
  writeLines("downloaded", source_file)
  dest_file <- file.path(tempfile("linkorgs-download-"), "nested", "file.txt")

  result <- LinkOrgs:::LinkOrgsDownload(
    paste0("file://", normalizePath(source_file, winslash = "/", mustWork = TRUE)),
    destfile = dest_file,
    quiet = TRUE
  )

  expect_equal(result, dest_file)
  expect_true(file.exists(dest_file))
  expect_equal(readLines(dest_file), "downloaded")
})

test_that("network directory resolves complete cache when bundled files are absent", {
  cache_dir <- tempfile("linkorgs-cache-")
  network_dir <- file.path(cache_dir, "Directory_markov", "directory_data_markov")
  dir.create(network_dir, recursive = TRUE)
  file.create(file.path(network_dir, "LinkIt_directory_markov.Rdata"))
  file.create(file.path(network_dir, "LinkIt_directory_markov_trigrams.Rdata"))
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  result <- LinkOrgs:::LinkOrgsNetworkDirectory(
    algorithm = "markov",
    url = "https://invalid.example/no-download.zip",
    cache_dir = cache_dir,
    quiet = TRUE
  )

  expect_equal(
    normalizePath(result, winslash = "/", mustWork = TRUE),
    normalizePath(file.path(cache_dir, "Directory_markov"), winslash = "/", mustWork = TRUE)
  )
})

test_that("LinkOrgs bipartite path can use bundled network data offline", {
  skip_on_cran()
  bundled_dir <- system.file("extdata", "Directory_bipartite", package = "LinkOrgs")
  skip_if_not(nzchar(bundled_dir), "bundled bipartite data is unavailable")
  skip_if_not(file.exists(file.path(
    bundled_dir,
    "directory_data_bipartite_thresh40",
    "LinkIt_directory_bipartite.Rdata"
  )), "bundled bipartite directory is unavailable")

  result <- LinkOrgs(
    x = data.frame(org = c("apple", "microsoft")),
    y = data.frame(org = c("apple", "microsoft")),
    by = "org",
    algorithm = "bipartite",
    DistanceMeasure = "jaccard",
    MaxDist = 0,
    MaxDist_network = 0,
    AveMatchNumberPerAlias = NULL,
    AveMatchNumberPerAlias_network = NULL,
    ReturnProgress = FALSE,
    ReturnDecomposition = TRUE,
    nCores = 1
  )

  expect_named(result, c("z", "z_RawNames", "z_Network"))
  expect_s3_class(result$z, "data.frame")
  expect_s3_class(result$z_Network, "data.frame")
  expect_gt(nrow(result$z_Network), 0)
  expect_true("stringdist2network" %in% names(result$z))
})
