# =============================================================================
# Backend Setup Tests
# =============================================================================

test_that("backend package helper pins the expected Python packages", {
  packages <- LinkOrgs:::LinkOrgsBackendPackages(
    tryMetal = FALSE,
    sys_info = c(sysname = "Linux", machine = "x86_64")
  )

  expect_true(all(c(
    "tensorflow==2.15",
    "numpy==1.26.4",
    "tensorflow_probability==0.23",
    "jax==0.4.26",
    "jaxlib==0.4.26",
    "optax==0.2.2",
    "equinox==0.11.4",
    "jmp==0.0.4"
  ) %in% packages))
  expect_false("jax-metal==0.1.0" %in% packages)
})

test_that("Apple Silicon backend package helper adds jax-metal when requested", {
  packages <- LinkOrgs:::LinkOrgsBackendPackages(
    tryMetal = TRUE,
    sys_info = c(sysname = "Darwin", machine = "arm64")
  )

  expect_true("jax-metal==0.1.0" %in% packages)
})

test_that("BuildBackendInternal creates conda env and installs packages via injected functions", {
  calls <- new.env(parent = emptyenv())
  fake_conda_create <- function(...) {
    calls$conda_create <- list(...)
  }
  fake_py_install <- function(packages, ...) {
    calls$packages <- packages
    calls$py_install <- list(...)
  }

  expect_output(
    LinkOrgs:::BuildBackendInternal(
      conda_env = "test_env",
      conda = "test_conda",
      tryMetal = FALSE,
      conda_create = fake_conda_create,
      py_install = fake_py_install,
      sys_info = c(sysname = "Linux", machine = "x86_64")
    ),
    "Done building LinkOrgs backend!"
  )

  expect_equal(calls$conda_create$envname, "test_env")
  expect_equal(calls$conda_create$conda, "test_conda")
  expect_equal(calls$conda_create$python_version, "3.11")
  expect_true("jax==0.4.26" %in% calls$packages)
  expect_equal(calls$py_install$envname, "test_env")
  expect_true(calls$py_install$pip)
})
