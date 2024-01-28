# Instructions for package installation + use
{
  # Download package via github if needed
  # devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs")

  # local install for development team
  # install.packages("~/Documents/LinkOrgs-software/LinkOrgs",repos = NULL, type = "source",force = F)

  # See package documentation for help
  # ?LinkOrgs::FastFuzzyMatch
  # ?LinkOrgs::AssessMatchPerformance
  # ?LinkOrgs::LinkOrgs

  # Load package
  library( LinkOrgs )

  # Create synthetic data to try everything out
  x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
  y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
  x <- data.frame("orgnames_x"=x_orgnames)
  y <- data.frame("orgnames_y"=y_orgnames)

  # Perform a simple merge with package using default (machine-learning model)
  LinkedOrgs_ML <- LinkOrgs::LinkOrgs(x = x,
                                      y = y,
                                      by.x = "orgnames_x",
                                      by.y = "orgnames_y")

  # Perform a simple merge with package using bipartite network representation
  LinkedOrgs_Bipartite <- LinkOrgs(x = x,
                         y = y,
                         by.x = "orgnames_x",
                         by.y = "orgnames_y",
                         algorithm = "bipartite",
                         openBrowser= F)

  # Perform a simple merge with package using Markov network representation
  LinkedOrgs_Markov <- LinkOrgs(x = x,
                                   y = y,
                                   by.x = "orgnames_x",
                                   by.y = "orgnames_y",
                                   algorithm = "markov",
                                   openBrowser= F)

  # Perform a simple merge with package using bipartite network representation and ML-based distance metric for names
  LinkedOrgs_BipartiteWithML <- LinkOrgs(x = x,
                                y = y,
                                by.x = "orgnames_x",
                                by.y = "orgnames_y",
                                algorithm = "bipartite",
                                DistanceMeasure = "ml",
                                openBrowser= F)

  # Print results
  print( linkedOrgs )
}
