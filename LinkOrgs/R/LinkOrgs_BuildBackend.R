#!/usr/bin/env Rscript
#' Build the environment for LinkOrgs machine learning models. Builds a conda environment in which jax, optax, equinox, and jmp are installed.
#'
#' @param conda_env (default = `"LinkOrgsEnv"`) Name of the conda environment in which to place the backends.
#' @param conda (default = `auto`) The path to a conda executable. Using `"auto"` allows reticulate to attempt to automatically find an appropriate conda binary.

#' @return Builds the computational environment for `LinkOrgs`. This function requires an Internet connection.
#' You may find out a list of conda Python paths via: `system("which python")`
#'
#' @examples
#' # For a tutorial, see
#' # github.com/cjerzak/linkorgs-software/
#'
#' @export
#' @md

BuildBackend <- function(conda_env = "LinkOrgsEnv", conda = "auto"){
  # Create a new conda environment
  reticulate::conda_create(envname = conda_env,
                           conda = conda,
                           python_version = "3.11")

  # Install Python packages within the environment
  Packages2Install <- c("jax","optax","tensorflow","equinox","numpy",
                          "tensorflow_probability","jmp")
  if(Sys.info()["sysname"] == "Darwin"){
    if("try-error" %in% class(try_)){
      for(Pack_ in Packages2Install){
        try_ <- try(reticulate::py_install(Pack_, conda = conda, pip = TRUE, envname = conda_env), T)
      }
    }

  }
  if(Sys.info()["sysname"] == "Windows"){
    for(Pack_ in Packages2Install){
      reticulate::py_install(Pack_, conda = conda, pip = TRUE, envname = conda_env)
    }
  }
  if(Sys.info()["sysname"] == "Linux"){
    # pip install --upgrade jax jaxlib==0.1.69+cuda111 -f https://storage.googleapis.com/jax-releases/jax_releases.html
    for(Pack_ in Packages2Install){
      reticulate::py_install(Pack_, conda = conda, pip = TRUE, envname = conda_env)
    }
  }
  print("Done building LinkOrgs backend!")
}

