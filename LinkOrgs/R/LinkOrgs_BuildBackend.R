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

BuildBackend <- function(conda_env = "LinkOrgsEnv", conda = "auto", tryMetal = T){
  # Create a new conda environment
  reticulate::conda_create(envname = conda_env,
                           conda = conda,
                           python_version = "3.11")

  # Install Python packages within the environment
  Packages2Install <- c("tensorflow==2.15",
                        #"numpy==2.0",
                        "numpy==1.26.4",
                        "tensorflow_probability==0.23",
                        "jax==0.4.26",
                        "jaxlib==0.4.26",
                        "ml_dtypes==0.4.0",
                        "optax==0.2.2",
                        "equinox==0.11.4",
                        "jmp==0.0.4")
  if(tryMetal){ 
    if(Sys.info()["sysname"] == "Darwin" & 
       Sys.info()["machine"] == "arm64"){
        Packages2Install <- c(Packages2Install,
                              "jax-metal==0.1.0")
    }
        reticulate::py_install(Packages2Install, 
                               conda = conda, 
                               pip = TRUE, 
                               envname = conda_env)
  }
  if(!tryMetal){ 
    if(Sys.info()["sysname"] == "Darwin"){
      reticulate::py_install(Packages2Install, 
                             conda = conda, 
                             pip = TRUE, 
                             envname = conda_env)
    }
  }
  if(Sys.info()["sysname"] == "Windows"){
     reticulate::py_install(Packages2Install, 
                             conda = conda, 
                             pip = TRUE, 
                             envname = conda_env)
  }
  if(Sys.info()["sysname"] == "Linux"){
    reticulate::py_install(Packages2Install, 
                           conda = conda, 
                           pip = TRUE, 
                           envname = conda_env)
  }
  print("Done building LinkOrgs backend!")
}

