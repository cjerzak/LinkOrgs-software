# =============================================================================
# Network and Cache Tests
# =============================================================================

local_tiny_network_cache <- function(algorithm = "markov") {
  cache_dir <- tempfile("linkorgs-network-cache-")
  network_dir <- file.path(
    cache_dir,
    sprintf("Directory_%s", algorithm),
    LinkOrgs:::LinkOrgsNetworkSubdir(algorithm)
  )
  dir.create(network_dir, recursive = TRUE)

  directory <- data.frame(
    alias_name = c("apple llc", "apple corp", "microsoft inc", "microsoft corporation"),
    canonical_id = c("A", "A", "M", "M"),
    weights = 1,
    stringsAsFactors = FALSE
  )
  directory_trigrams <- LinkOrgs:::trigram_index(directory$alias_name, "alias_id")
  save(directory, file = file.path(network_dir, sprintf("LinkIt_directory_%s.Rdata", algorithm)))
  save(directory_trigrams, file = file.path(network_dir, sprintf("LinkIt_directory_%s_trigrams.Rdata", algorithm)))

  cache_dir
}

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

test_that("markov network path can return network-only matches from local cache", {
  cache_dir <- local_tiny_network_cache("markov")
  old_cache_dir <- Sys.getenv("LINKORGS_CACHE_DIR", unset = NA_character_)
  Sys.setenv(LINKORGS_CACHE_DIR = cache_dir)
  on.exit({
    if(is.na(old_cache_dir)){
      Sys.unsetenv("LINKORGS_CACHE_DIR")
    } else {
      Sys.setenv(LINKORGS_CACHE_DIR = old_cache_dir)
    }
    unlink(cache_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  result <- LinkOrgs(
    x = data.frame(xid = 1, org_x = "apple llc"),
    y = data.frame(yid = 2, org_y = "apple corp"),
    by.x = "org_x",
    by.y = "org_y",
    algorithm = "markov",
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
  expect_equal(nrow(result$z_RawNames), 0)
  expect_equal(nrow(result$z_Network), 1)
  expect_equal(result$z$org_x, "apple llc")
  expect_equal(result$z$org_y, "apple corp")
  expect_equal(result$z$stringdist2network, 0)
  expect_equal(result$z$minDist, 0)
})

test_that("markov network path combines raw and network distances", {
  cache_dir <- local_tiny_network_cache("markov")
  old_cache_dir <- Sys.getenv("LINKORGS_CACHE_DIR", unset = NA_character_)
  Sys.setenv(LINKORGS_CACHE_DIR = cache_dir)
  on.exit({
    if(is.na(old_cache_dir)){
      Sys.unsetenv("LINKORGS_CACHE_DIR")
    } else {
      Sys.setenv(LINKORGS_CACHE_DIR = old_cache_dir)
    }
    unlink(cache_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  result <- LinkOrgs(
    x = data.frame(xid = 1:2, org_x = c("apple llc", "microsoft inc")),
    y = data.frame(yid = 2:3, org_y = c("apple llc", "microsoft corporation")),
    by.x = "org_x",
    by.y = "org_y",
    algorithm = "markov",
    DistanceMeasure = "jaccard",
    MaxDist = 0,
    MaxDist_network = 0,
    AveMatchNumberPerAlias = NULL,
    AveMatchNumberPerAlias_network = NULL,
    RelThresNetwork = 2,
    ReturnProgress = FALSE,
    ReturnDecomposition = FALSE,
    nCores = 1
  )

  expect_equal(nrow(result), 2)
  expect_true(any(!is.na(result$stringdist)))
  expect_true(any(!is.na(result$stringdist2network)))
  expect_true(all(result$minDist == 0))
})
