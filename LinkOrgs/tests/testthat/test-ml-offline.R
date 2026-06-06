# =============================================================================
# Offline ML Branch Tests
# =============================================================================

local_fake_ml_cache <- function(ml_version = "v1") {
  cache_dir <- tempfile("linkorgs-ml-cache-")
  model_dir <- file.path(cache_dir, sprintf("Model_%s", ml_version), "Analysis")
  dir.create(model_dir, recursive = TRUE)
  file.create(file.path(cache_dir, sprintf("Model_%s.zip", ml_version)))
  file.create(file.path(cache_dir, sprintf("ModelWeights_%s.eqx", ml_version)))
  data.table::fwrite(data.frame(character_id = 1), file.path(cache_dir, "CharIndicatorsLoc.csv"))

  fake_model_script <- c(
    "jax <- list(default_backend = function() 'fake-backend')",
    "jnp <- list()",
    "np <- list()",
    "eq <- list(tree_deserialise_leaves = function(path, state) state)",
    "ModelList <- list('model')",
    "StateList <- list('state')",
    "opt_state <- list('opt')",
    "GetAliasRep_BigBatch <- function(names, nBatch_BigBatch = 50) {",
    "  rows <- lapply(tolower(names), function(name) {",
    "    if(grepl('apple', name)) c(0, 0)",
    "    else if(grepl('microsoft', name)) c(10, 10)",
    "    else c(100, 100)",
    "  })",
    "  do.call(rbind, rows)",
    "}"
  )
  for(script_name in c(
    "LinkOrgs_Helpers.R",
    "JaxTransformer_Imports.R",
    "JaxTransformer_BuildML.R",
    "JaxTransformer_TrainDefine.R"
  )){
    writeLines(fake_model_script, file.path(model_dir, script_name))
  }

  cache_dir
}

test_that("ML export-only path uses cached artifacts and preserves row alignment", {
  cache_dir <- local_fake_ml_cache("v1")
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
    x = data.frame(org = c("apple", "microsoft")),
    y = data.frame(org = c("microsoft", "apple")),
    by = "org",
    algorithm = "ml",
    conda_env = "missing_env",
    conda_env_required = FALSE,
    ExportEmbeddingsOnly = TRUE,
    ReturnProgress = FALSE
  )

  expect_named(result, c("embedx", "embedy"))
  expect_equal(result$embedx$org, c("apple", "microsoft"))
  expect_equal(result$embedy$org, c("microsoft", "apple"))
  expect_equal(as.numeric(result$embedx[1, 2:3]), c(0, 0))
  expect_equal(as.numeric(result$embedx[2, 2:3]), c(10, 10))
})

test_that("ML branch prefers python_executable over conda_env", {
  cache_dir <- local_fake_ml_cache("v1")
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

  calls <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    use_python = function(python, required) {
      calls$python <- list(python = python, required = required)
    },
    use_condaenv = function(...) {
      calls$conda <- list(...)
    },
    .package = "reticulate"
  )

  LinkOrgs(
    x = data.frame(org = "apple"),
    y = data.frame(org = "apple"),
    by = "org",
    algorithm = "ml",
    python_executable = "/tmp/fake-python",
    conda_env = "should-not-be-used",
    conda_env_required = FALSE,
    ExportEmbeddingsOnly = TRUE,
    ReturnProgress = FALSE
  )

  expect_equal(calls$python$python, "/tmp/fake-python")
  expect_false(calls$python$required)
  expect_false(exists("conda", envir = calls, inherits = FALSE))
})
