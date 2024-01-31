{
  #######################
  #### Generate docs ####
  #######################

  # clear workspace
  rm( list=ls() )

  # Specify package name
  package_name <- "LinkOrgs"

  # Generate documentation
  setwd( sprintf("~/Documents/%s-software",package_name) )
  devtools::document(  package_path <- sprintf("./%s",package_name)  )
  try(file.remove(sprintf("./%s.pdf",package_name)),T)
  system(sprintf("R CMD Rd2pdf %s",package_name))

  # Check package to ensure it meets CRAN standards.
  # devtools::check( package_path )
}
