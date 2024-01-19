#!/usr/bin/env Rscript
{
  ##########################################
  # code for testing most functionalities of LinkOrgs on your hardware
  ##########################################
  tryTests <- try({
    # remote install latest version of the package
    # devtools::install_github(repo = "cjerzak/LinkOrgs-software/LinkOrgs")

    # local install for development team
    # install.packages("~/Documents/LinkOrgs-software/LinkOrgs",repos = NULL, type = "source",force = F)

    print("Starting starter tutorial..."); setwd("~");
    t_ <- try(source("~/Documents/LinkOrgs-software/tutorials/GettingStarted_Tutorial.R"),T)
    if("try-error" %in% class(t_)){ stop("Failed at starter tutorial...") }
  }, T)

  if('try-error' %in% class(tryTests)){ print("At least one test failed..."); print( tryTests ) }
  if(!'try-error' %in% class(tryTests)){ print("All tests succeeded!") }
}
