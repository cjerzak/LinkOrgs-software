{
########################################################
#### DOCUMENTATION GENERATION + INSTALL INSTRUCTIONS ###
########################################################

# Specify package name
package_name <- "LinkOrgs"

# Generate documentation
setwd( sprintf("~/Documents/%s-software",package_name) )
devtools::document(  package_path <- sprintf("./%s",package_name)  )
try(file.remove(sprintf("./%s.pdf",package_name)),T)
system(sprintf("R CMD Rd2pdf %s",package_name))

# Check package to ensure it meets CRAN standards.
devtools::check( package_path )

# Instructions for package installation + use
if(T == F){
  # Download package via github
  devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs")

  # See package documentation for help
  # ?LinkOrgs::FastFuzzyMatch
  # ?LinkOrgs::AssessMatchPerformance
  # ?LinkOrgs::LinkOrgs

  # Load package
  library(LinkOrgs)

  # Create synthetic data to try everything out
  x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
  y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
  x <- data.frame("orgnames_x"=x_orgnames)
  y <- data.frame("orgnames_y"=y_orgnames)

  # Perform a simple merge with package
  linkedOrgs <- LinkOrgs(x = x,
                         y = y,
                         by.x = "orgnames_x",
                         by.y = "orgnames_y",
                         algorithm = "bipartite",
                         openBrowser= F)

  # Print results
  print( linkedOrgs )
}
}
