# Instructions for package installation + use
{
  # clear workspace
  rm( list = ls())

  # Download package via github if needed
  # devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs")

  # local install for development team
  # install.packages("~/Documents/LinkOrgs-software/LinkOrgs",repos = NULL, type = "source", force = F)
  library( LinkOrgs )

  # install.packages( 'zoomerjoin', repos = c('https://beniaminogreen.r-universe.dev', getOption("repos")) )
  # development branch on github / runiverse / cran
  # See package documentation for help
  # ?LinkOrgs::FastFuzzyMatch
  # ?LinkOrgs::AssessMatchPerformance
  # ?LinkOrgs::LinkOrgs

  # gen data
  nCol <- 2^10
  nRow <- 2^16
  a <- cbind(letters[1:nRow %% 24+1],
             as.data.frame(  matrix(round(rnorm(nRow*nCol),5),
                                    ncol = nCol) ))
  b <- cbind(letters[1:nRow %% 24+1],
             as.data.frame(  matrix(round(rnorm(nRow*nCol),5),
                                    ncol = nCol) ))
  colnames(a) <- colnames(b) <- c("ID",  by_cols <- paste("D", 1:nCol, sep = ""))
  a[1:100,by_cols] - t( b[1:100,by_cols] )

  x <- embed1 <- a[1:1000,2:50]
  y <- embed2 <- a[(1:1000)+1000,2:50]

  # apply fxns
  AveMatchNumberPerAlias <- 5L
  CalibratedThres <- LinkOrgs::GetCalibratedDistThres(x, y, AveMatchNumberPerAlias = AveMatchNumberPerAlias)
  joined_xy_hashed <- zoomerjoin::euclidean_inner_join(x, y,
                                                threshold = CalibratedThres, # distribution of projection weights depend on task
                                                # band_width = 10000, #  makes it more difficult for things to match
                                                n_bands = 400, # main thing to tweak;increases sensitivity (better chance at finding pair)
                                                # r = .01, # alters collision probability, higher is more collisions
                                                progress = T)
  # theory to guide n_bands? -> use zoomerjoin::euclidean_probability?
  zoomerjoin::jaccard_curve(40,5)
  # euclidean inner produces candidates, then run pDistMatch
  # we want high recall
  # zoomerjoin::euclidean_probability -> normality assumption; computes chance two things are compared
  joined_xy_exact <- LinkOrgs::pDistMatch_euclidean(x, y, MaxDist = CalibratedThres)
  # index 1, index 2, distance # LinkOrgs::pDistMatch
  dim( joined_xy_exact )
  dim( x ) * AveMatchNumberPerAlias
  dim( joined_xy_hashed )

  # checks
  check1 <- LinkOrgs::pDistMatch_euclidean(x,y); hist( check1$stringdist ); abline(v=CalibratedThres, lwd = 3)
  check2 <- as.matrix(dist(rbind(x,y)))[1:nrow(x), c((1+nrow(x)):(nrow(x)+nrow(y)))]

  # confirm equivalence
  hist( check1$stringdist )
  hist(check2)
  sum(check1$stringdist <= CalibratedThres)
  sum(check2 <= CalibratedThres)

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

  # Perform a merge with a end-to-end meachine learning backend
  LinkedOrgs_ML <- LinkOrgs::LinkOrgs(x = x, by.x = "orgnames_x",
                                      y = y, by.y = "orgnames_y",
                                      algorithm = "ml")

  # Perform a merge with a transfer learning backend
  # LinkedOrgs_transfer <- LinkOrgs::LinkOrgs( x = x, by.x = "orgnames_x",
                                      # y = y, by.y = "orgnames_y",
                                      # algorithm = "transfer")

  # Perform a merge with package using bipartite network representation
  LinkedOrgs_Bipartite <- LinkOrgs(
                         x = x, by.x = "orgnames_x",
                         y = y, by.y = "orgnames_y",
                         algorithm = "bipartite")

  # Perform a merge with package using Markov network representation
  LinkedOrgs_Markov <- LinkOrgs(
                          x = x, by.x = "orgnames_x",
                          y = y, by.y = "orgnames_y",
                          algorithm = "markov",
                          openBrowser= F)

  # Perform a merge with package using markov network representation and ML-based distance metric for names
  LinkedOrgs_MarkovWithML <- LinkOrgs(
                                x = x, by.x = "orgnames_x",
                                y = y, by.y = "orgnames_y",
                                algorithm = "markov",
                                DistanceMeasure = "ml",
                                openBrowser= F)

  # Perform a simple merge with package using bipartite network representation and ML-based distance metric for names
  LinkedOrgs_BipartiteWithML <- LinkOrgs(
                                x = x, by.x = "orgnames_x",
                                y = y, by.y = "orgnames_y",
                                algorithm = "bipartite",
                                DistanceMeasure = "ml",
                                openBrowser= F)

  # note:
  # algorithm %in% c("bipartite", "markov") and DistanceMeasure = "transfer"
  # are not supported currently due to the time-complexity of the transfer learning backend
}
