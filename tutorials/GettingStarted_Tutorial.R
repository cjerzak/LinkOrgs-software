# Instructions for package installation + use
{
  # clear workspace
  rm( list = ls())

  # Download package via github if needed
  # devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs")

  # local install for development team
  # install.packages("~/Documents/LinkOrgs-software/LinkOrgs",repos = NULL, type = "source", force = F)

  # See package documentation for help
  # ?LinkOrgs::FastFuzzyMatch
  # ?LinkOrgs::AssessMatchPerformance
  # ?LinkOrgs::LinkOrgs

  # Load package
  library( LinkOrgs )

  # Create synthetic data to try everything out
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

  # Perform a merge with a end-to-end meachine learning backend
  LinkedOrgs_ML <- LinkOrgs::LinkOrgs(x = x, by.x = "orgnames_x",
                                      y = y, by.y = "orgnames_y",
                                      algorithm = "ml", ml_version = "v4")

  # Perform a merge with package using bipartite network representation
  LinkedOrgs_Bipartite <- LinkOrgs(
                         x = x, by.x = "orgnames_x",
                         y = y, by.y = "orgnames_y",
                         algorithm = "bipartite")

  # Perform a merge with package using Markov network representation
  LinkedOrgs_Markov <- LinkOrgs(
                          x = x, by.x = "orgnames_x",
                          y = y, by.y = "orgnames_y",
                          algorithm = "markov")

  # Perform a merge with package using markov network representation and ML-based distance metric for names
  LinkedOrgs_MarkovWithML <- LinkOrgs(
                                x = x, by.x = "orgnames_x",
                                y = y, by.y = "orgnames_y",
                                algorithm = "markov",
                                DistanceMeasure = "ml")

  # Perform a simple merge with package using bipartite network representation and ML-based distance metric for names
  LinkedOrgs_BipartiteWithML <- LinkOrgs(
                                x = x, by.x = "orgnames_x",
                                y = y, by.y = "orgnames_y",
                                algorithm = "bipartite",
                                DistanceMeasure = "ml")

}
