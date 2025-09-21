# Instructions for package installation + use
{
  # clear workspace
  rm( list = ls())

  # Download package via github if needed
  # devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs")

  # local install for development team
  # install.packages("~/Documents/LinkOrgs-software/LinkOrgs",repos = NULL, type = "source", force = F)
  
  # build backend - do this once, when before first using ML backend. 
  # LinkOrgs::BuildBackend()

  # See package documentation for help
  # ?LinkOrgs::FastFuzzyMatch
  # ?LinkOrgs::AssessMatchPerformance
  # ?LinkOrgs::LinkOrgs

  # Load package
  library( LinkOrgs )

  # Create synthetic data to try package out
  x <- data.frame("orgnames_x" = x_orgnames <- c("apple","oracle",
                                                 "enron inc.","mcdonalds corporation"))
  y <- data.frame("orgnames_y"= y_orgnames <- c("apple corp","oracle inc",
                                                "enron","mcdonalds co"))

  #############################################
  # getting started with various merge types
  #############################################

  # Perform a merge using parallelized fuzzy matching
  LinkedOrgs_fuzzy <- LinkOrgs(x = x, by.x = "orgnames_x",
                               y = y, by.y = "orgnames_y",
                               algorithm = "fuzzy")

  # Perform a merge using Markov network representation of LinkedIn network
  LinkedOrgs_Markov <- LinkOrgs(
                          x = x, by.x = "orgnames_x",
                          y = y, by.y = "orgnames_y",
                          algorithm = "markov")

  # Perform a merge using bipartite network representation of LinkedIn network
  LinkedOrgs_Bipartite <- LinkOrgs(
                         x = x, by.x = "orgnames_x",
                         y = y, by.y = "orgnames_y",
                         algorithm = "bipartite")

  # Build backend for ML model (do this only once):
  # try(LinkOrgs::BuildBackend( conda_env = "LinkOrgsEnv", conda = "auto" ),T)

  # if conda = "auto" fails, try to specify the path to the correct conda to use like so: 
  # LinkOrgs::BuildBackend( conda_env = "LinkOrgsEnv", conda = "/Users/cjerzak/miniforge3/bin/python" )

  # Perform a merge using machine learning approach
  LinkedOrgs_ML <- LinkOrgs(
                          x = x, by.x = "orgnames_x",
                          y = y, by.y = "orgnames_y",
                          algorithm = "ml", ml_version = "v1")
  
  # Perform a merge using the ML approach, just exporting the name representations
  rep_joint <- LinkOrgs( # returns list(embedx = ..., embedy = ...) for manual linkage.
    x = x, y = y,
    by.x = "orgnames_x", by.y = "orgnames_y",
    algorithm = "ml",
    ExportEmbeddingsOnly = TRUE
  )
  rep_x <- LinkOrgs( # returns list(embedx = ...)
    x = x, y = NULL,
    by.x = "orgnames_x",
    algorithm = "ml",
    ExportEmbeddingsOnly = TRUE
  ) 
  rep_y <- LinkOrgs( # returns list(embedy = ...)
    x = NULL, y = y,
    by.y = "orgnames_y",
    algorithm = "ml",
    ExportEmbeddingsOnly = TRUE) 
  
  
}
