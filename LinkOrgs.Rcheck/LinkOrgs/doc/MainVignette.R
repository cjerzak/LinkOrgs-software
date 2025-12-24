## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# install.packages("remotes") # if you haven't installed 'remotes' previously
# remotes::install_github("cjerzak/LinkOrgs-software/LinkOrgs")

## -----------------------------------------------------------------------------
library(LinkOrgs)

# This function calls reticulate::conda_create() under the hood to create a new conda env
#LinkOrgs::BuildBackend(conda_env = "LinkOrgsEnv",  # or choose custom name
             #conda = "auto", tryMetal = TRUE)

## -----------------------------------------------------------------------------
# Let's create two synthetic datasets:
x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")

x <- data.frame("orgnames_x" = x_orgnames)
y <- data.frame("orgnames_y" = y_orgnames)

# A straightforward fuzzy match with default settings:
linkedOrgs_fuzzy <- LinkOrgs(x = x,
                             y = y,
                             by.x = "orgnames_x",
                             by.y = "orgnames_y",
                             # MaxDist is a maximum allowed fuzzy distance
                             MaxDist = 0.5,
                             DistanceMeasure = "jaccard",
                             algorithm = "fuzzy")

linkedOrgs_fuzzy

## ----eval = FALSE-------------------------------------------------------------
# linkedOrgs_ml <- LinkOrgs(x = x,
#                           y = y,
#                           by.x = "orgnames_x",
#                           by.y = "orgnames_y",
#                           MaxDist = 2.0,  # Euclidean distance in embedding space
#                           algorithm = "ml",
#                           conda_env = "LinkOrgsEnv",
#                           conda_env_required = T)
# 
# linkedOrgs_ml

## ----eval = FALSE-------------------------------------------------------------
# linkedOrgs_ensemble <- LinkOrgs(x = x,
#                                 y = y,
#                                 by.x = "orgnames_x",
#                                 by.y = "orgnames_y",
#                                 MaxDist_network = 2.0,
#                                 algorithm = "bipartite",  # or "markov"
#                                 DistanceMeasure = "ml",
#                                 conda_env = "LinkOrgsEnv",
#                                 conda_env_required = T)

## -----------------------------------------------------------------------------
# Synthetic example: 
x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")

x <- data.frame("orgnames_x"=x_orgnames)
y <- data.frame("orgnames_y"=y_orgnames)

# Suppose z is your matched dataset from LinkOrgs (or anything else)
z <- data.frame("orgnames_x"=x_orgnames[1:2], 
                "orgnames_y"=y_orgnames[1:2])

# Suppose z_true is your best ground truth:
z_true <- data.frame("orgnames_x"=x_orgnames, 
                     "orgnames_y"=y_orgnames)

# Evaluate:
PerformanceMatrix <- AssessMatchPerformance(x = x,
                                           y = y,
                                           z = z,
                                           z_true = z_true,
                                           by.x = "orgnames_x",
                                           by.y = "orgnames_y")

PerformanceMatrix

## ----eval = FALSE-------------------------------------------------------------
# # Download a zipped CSV from a Dropbox link and read it as a data.table
# my_dt <- url2dt(url = "https://www.dropbox.com/s/iqf9ids77dckopf/Directory_LinkIt_bipartite_Embeddings.csv.zip?dl=0")
# 
# # Internally, url2dt uses dropboxURL2downloadURL() to fix the link so that
# # it becomes a direct download (dl.dropboxusercontent.com).
# 
# # The function creates a temporary folder, downloads the file, unzips it, and returns a data.table.

## ----eval = FALSE-------------------------------------------------------------
# library(LinkOrgs)
# 
# ### 1. Suppose we have two large data frames:
# ###    lobbying_data (org name in col "lobby_org")
# ###    finance_data  (org name in col "ticker_org")
# 
# # For illustration, use smaller synthetic ones
# lobbying_data <- data.frame("lobby_org" = c("International Business Machs",
#                                             "microsft",
#                                             "Oracle inc."))
# finance_data  <- data.frame("ticker_org"  = c("Microsoft Corporation",
#                                               "International Business Machines",
#                                               "Micron Tech",
#                                               "Oracle corp"))
# 
# ### 2. Fuzzy matching approach:
# fuzzy_matches <- LinkOrgs(
#   x = lobbying_data,
#   y = finance_data,
#   by.x = "lobby_org",
#   by.y = "ticker_org",
#   algorithm = "fuzzy",
#   DistanceMeasure = "jaccard",
#   MaxDist = 0.7
# )
# 
# ### 3. ML approach (requires conda environment):
# # BuildBackend(conda_env = "LinkOrgsEnv") # if not done yet
# ml_matches <- LinkOrgs(
#   x = lobbying_data,
#   y = finance_data,
#   by.x = "lobby_org",
#   by.y = "ticker_org",
#   algorithm = "ml",
#   # We want an automatic threshold based on about 3 matches per name on average:
#   AveMatchNumberPerAlias = 3,
#   conda_env = "LinkOrgsEnv",
#   conda_env_required = T
# )
# 
# ### 4. Evaluate with a small ground truth set (hypothetical):
# z_true <- data.frame(
#   "lobby_org" = c("International Business Machs", "microsft", "Oracle inc."),
#   "ticker_org" = c("International Business Machines", "Microsoft Corporation",
#                    "Oracle corp")
# )
# 
# Perf_fuzzy <- AssessMatchPerformance(
#   x = lobbying_data,
#   y = finance_data,
#   z = fuzzy_matches,
#   z_true = z_true,
#   by.x = "lobby_org",
#   by.y = "ticker_org"
# )
# 
# Perf_ml <- AssessMatchPerformance(
#   x = lobbying_data,
#   y = finance_data,
#   z = ml_matches,
#   z_true = z_true,
#   by.x = "lobby_org",
#   by.y = "ticker_org"
# )
# 
# Perf_fuzzy
# Perf_ml
# 
# # Typically, the ML approach will better match "microsft" to "Microsoft Corporation"
# # and "International Business Machs" to "International Business Machines".

