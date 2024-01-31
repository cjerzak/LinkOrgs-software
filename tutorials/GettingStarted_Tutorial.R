# Instructions for package installation + use
{
  # clear workspace
  rm( list = ls())

  # Download package via github if needed
  # devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs")

  # local install for development team
  # install.packages("~/Documents/LinkOrgs-software/LinkOrgs",repos = NULL, type = "source",force = F)
  library( LinkOrgs )

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
                                                threshold = CalibratedThres,
                                                #band_width = 10000,
                                                n_bands = 400,
                                                r = .01,
                                                progress = T)
  joined_xy_exact <- LinkOrgs::pDistMatch(x, y, AcceptThreshold = CalibratedThres)
  dim( joined_xy_exact )
  dim( x ) * AveMatchNumberPerAlias
  dim( joined_xy_hashed )

  # checks
  check1 <- LinkOrgs::pDistMatch(x,y); hist( check1$dist ); abline(v=CalibratedThres, lwd = 3)
  check2 <- as.matrix(dist(rbind(x,y)))[1:nrow(x), c((1+nrow(x)):(nrow(x)+nrow(y)))]

  # confirm equivalence
  hist( check1$dist )
  hist(check2)
  sum(check1$dist <= CalibratedThres)
  sum(check2 <= CalibratedThres)

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
