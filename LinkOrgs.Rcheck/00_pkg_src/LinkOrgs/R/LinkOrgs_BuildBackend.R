#!/usr/bin/env Rscript
#' Build Backend for LinkOrgs Machine Learning Models
#'
#' Creates and configures a conda environment with all necessary Python packages
#' (JAX, TensorFlow, Optax, Equinox, JMP) for running the machine learning
#' components of LinkOrgs.
#'
#' @param conda_env Character string; name of the conda environment to create.
#'   Default is `"LinkOrgs_env"`.
#' @param conda Character string; path to a conda executable, or `"auto"` to let
#'   reticulate automatically find an appropriate conda binary. Default is `"auto"`.
#' @param tryMetal Logical; if `TRUE` and running on Apple Silicon (arm64 macOS),
#'   attempts to install `jax-metal` for GPU acceleration. Default is `TRUE`.
#'
#' @return Invisibly returns `NULL`. Called for its side effect of creating and
#'   configuring the conda environment. Prints "Done building LinkOrgs backend!"
#'   upon successful completion.
#'
#' @details This function requires an Internet connection to download packages.
#'   The conda environment will include:
#'   \itemize{
#'     \item TensorFlow 2.15
#'     \item TensorFlow Probability 0.23
#'     \item JAX 0.4.26 and JAXlib 0.4.26
#'     \item Optax 0.2.2
#'     \item Equinox 0.11.4
#'     \item JMP 0.0.4
#'     \item NumPy 1.26.4
#'     \item jax-metal 0.1.0 (Apple Silicon only, if `tryMetal = TRUE`)
#'   }
#'
#'   You can find available conda Python paths via: `system("which python")`
#'
#' @examples
#' \dontrun{
#' # Build with default settings
#' BuildBackend()
#'
#' # Build with a specific conda path
#' BuildBackend(conda = "/opt/miniconda3/bin/conda")
#'
#' # Build without attempting Metal support on macOS
#' BuildBackend(tryMetal = FALSE)
#' }
#'
#' @seealso [LinkOrgs()] for using the ML backend after setup.
#' @export
#' @md

BuildBackend <- function(conda_env = "LinkOrgs_env", conda = "auto", tryMetal = T){
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
                        #"ml_dtypes==0.4.0",
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

