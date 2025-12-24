{
  rm(list = ls())
  options(error = NULL)

  # Set path and specify package name
  package_name <- "LinkOrgs"
  setwd(sprintf("~/Documents/%s-software", package_name))

  # Version number (should match DESCRIPTION)
  versionNumber <- "0.01"

  # Package path
  package_path <- sprintf("~/Documents/%s-software/%s", package_name, package_name)


  # Add data list if package has data
  tools::add_datalist(package_path, force = TRUE, small.size = 1L)

  # Build vignettes
  devtools::build_vignettes(package_path)

  # Generate documentation from roxygen comments
  devtools::document(package_path)

  # Remove old PDF manual
  try(file.remove(sprintf("%s/../%s.pdf", package_path, package_name)), silent = TRUE)

  # Create new PDF manual
  system(sprintf("R CMD Rd2pdf %s", package_path))
  
  # Manual commands for reference:
  # R CMD build --resave-data ~/Documents/LinkOrgs-software/LinkOrgs
  # R CMD check --as-cran ~/Documents/LinkOrgs_0.01.tar.gz
  install.packages("~/Documents/LinkOrgs-software/LinkOrgs", repos = NULL, type = "source", force = FALSE) # install from local
  # devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs") # install from github

  # Run tests (stop on failure)
  test_results <- devtools::test(package_path)
  if (any(as.data.frame(test_results)$failed > 0)) {
    stop("Tests failed! Stopping build process.")
  }
  cat("\n✓ All tests passed!\n\n")

  # Show object sizes in environment (for debugging memory usage)
  log(sort(sapply(ls(), function(l_) { object.size(eval(parse(text = l_))) })))

  # Check package to ensure it meets CRAN standards
  devtools::check(package_path)

  # Build tar.gz
  system(paste(
    shQuote(file.path(R.home("bin"), "R")),
    "CMD build --resave-data",
    shQuote(package_path)
  ))

  # Check as CRAN
  system(paste(
    shQuote(file.path(R.home("bin"), "R")),
    "CMD check --as-cran",
    shQuote(paste0(package_name, "_", versionNumber, ".tar.gz"))
  ))
  
  install.packages("~/Documents/LinkOrgs-software/LinkOrgs", repos = NULL, type = "source", force = FALSE) # install from local
  # devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs") # install from github
}
