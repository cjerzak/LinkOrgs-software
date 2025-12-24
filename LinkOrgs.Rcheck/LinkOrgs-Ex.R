pkgname <- "LinkOrgs"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "LinkOrgs-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('LinkOrgs')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("AssessMatchPerformance")
### * AssessMatchPerformance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: AssessMatchPerformance
### Title: AssessMatchPerformance
### Aliases: AssessMatchPerformance

### ** Examples

# Create synthetic data
x_orgnames <- c("apple", "oracle", "enron inc.", "mcdonalds corporation")
y_orgnames <- c("apple corp", "oracle inc", "enron", "mcdonalds co")
x <- data.frame("orgnames_x" = x_orgnames)
y <- data.frame("orgnames_y" = y_orgnames)
z <- data.frame("orgnames_x" = x_orgnames[1:2], "orgnames_y" = y_orgnames[1:2])
z_true <- data.frame("orgnames_x" = x_orgnames, "orgnames_y" = y_orgnames)

# Obtain match performance data
PerformanceMatrix <- AssessMatchPerformance(x = x,
                                            y = y,
                                            z = z,
                                            z_true = z_true,
                                            by.x = "orgnames_x",
                                            by.y = "orgnames_y")
print(PerformanceMatrix)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("AssessMatchPerformance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BuildBackend")
### * BuildBackend

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BuildBackend
### Title: Build Backend for LinkOrgs Machine Learning Models
### Aliases: BuildBackend

### ** Examples

## Not run: 
##D # Build with default settings
##D BuildBackend()
##D 
##D # Build with a specific conda path
##D BuildBackend(conda = "/opt/miniconda3/bin/conda")
##D 
##D # Build without attempting Metal support on macOS
##D BuildBackend(tryMetal = FALSE)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BuildBackend", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("LinkOrgs")
### * LinkOrgs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: LinkOrgs
### Title: LinkOrgs
### Aliases: LinkOrgs

### ** Examples

# Create synthetic data
x_orgnames <- c("apple", "oracle", "enron inc.", "mcdonalds corporation")
y_orgnames <- c("apple corp", "oracle inc", "enron", "mcdonalds co")
x <- data.frame("orgnames_x" = x_orgnames)
y <- data.frame("orgnames_y" = y_orgnames)

# Perform merge with fuzzy matching
linkedOrgs <- LinkOrgs(x = x,
                       y = y,
                       by.x = "orgnames_x",
                       by.y = "orgnames_y",
                       algorithm = "fuzzy",
                       MaxDist = 0.6)

print(linkedOrgs)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("LinkOrgs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dropboxURL2downloadURL")
### * dropboxURL2downloadURL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dropboxURL2downloadURL
### Title: Convert Dropbox Share URL to Direct Download URL
### Aliases: dropboxURL2downloadURL

### ** Examples

# Convert a Dropbox share link
direct_url <- dropboxURL2downloadURL(
  "https://www.dropbox.com/s/abc123/myfile.csv?dl=0"
)
# Returns: "https://dl.dropboxusercontent.com/s/abc123/myfile.csv?dl=0"




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dropboxURL2downloadURL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pDistMatch_discrete")
### * pDistMatch_discrete

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pDistMatch_discrete
### Title: Compute Discrete String Distances (Internal)
### Aliases: pDistMatch_discrete

### ** Examples

# Create synthetic data
x_orgnames <- c("apple", "oracle", "enron inc.", "mcdonalds corporation")
y_orgnames <- c("apple corp", "oracle inc", "enron", "mcdonalds co")
x <- data.frame("orgnames_x" = x_orgnames)
y <- data.frame("orgnames_y" = y_orgnames)

# Compute distances
distances <- pDistMatch_discrete(x = x,
                                 y = y,
                                 by.x = "orgnames_x",
                                 by.y = "orgnames_y",
                                 MaxDist = 0.5)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pDistMatch_discrete", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pDistMatch_euclidean")
### * pDistMatch_euclidean

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pDistMatch_euclidean
### Title: Compute Euclidean Distances Between Embeddings (Internal)
### Aliases: pDistMatch_euclidean

### ** Examples

## Not run: 
##D # Create synthetic embeddings
##D embedx <- matrix(rnorm(4 * 256), nrow = 4)
##D embedy <- matrix(rnorm(4 * 256), nrow = 4)
##D 
##D # Compute distances
##D distances <- pDistMatch_euclidean(embedx = embedx,
##D                                   embedy = embedy,
##D                                   MaxDist = 5.0)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pDistMatch_euclidean", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pFuzzyMatch_discrete")
### * pFuzzyMatch_discrete

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pFuzzyMatch_discrete
### Title: Fuzzy Match with Discrete String Distances
### Aliases: pFuzzyMatch_discrete

### ** Examples

# Create synthetic data
x_orgnames <- c("apple", "oracle", "enron inc.", "mcdonalds corporation")
y_orgnames <- c("apple corp", "oracle inc", "enron", "mcdonalds co")
x <- data.frame("orgnames_x" = x_orgnames)
y <- data.frame("orgnames_y" = y_orgnames)

# Perform fuzzy matching
matched <- pFuzzyMatch_discrete(x = x,
                                y = y,
                                by.x = "orgnames_x",
                                by.y = "orgnames_y",
                                MaxDist = 0.5)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pFuzzyMatch_discrete", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pFuzzyMatch_euclidean")
### * pFuzzyMatch_euclidean

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pFuzzyMatch_euclidean
### Title: Fuzzy Match with Euclidean Distance on Embeddings
### Aliases: pFuzzyMatch_euclidean

### ** Examples

## Not run: 
##D # Create synthetic data with embeddings
##D x <- data.frame("orgnames_x" = c("apple", "oracle"))
##D y <- data.frame("orgnames_y" = c("apple corp", "oracle inc"))
##D 
##D # Assume embedx and embedy are pre-computed embedding matrices
##D # (typically produced by LinkOrgs ML backend)
##D embedx <- matrix(rnorm(2 * 256), nrow = 2)
##D embedy <- matrix(rnorm(2 * 256), nrow = 2)
##D 
##D matched <- pFuzzyMatch_euclidean(x = x,
##D                                  y = y,
##D                                  by.x = "orgnames_x",
##D                                  by.y = "orgnames_y",
##D                                  embedx = embedx,
##D                                  embedy = embedy,
##D                                  MaxDist = 5.0)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pFuzzyMatch_euclidean", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print2")
### * print2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print2
### Title: print2
### Aliases: print2

### ** Examples


print2("Hello world!")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("url2dt")
### * url2dt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: url2dt
### Title: Download CSV from URL to data.table
### Aliases: url2dt

### ** Examples

## Not run: 
##D # Download from Dropbox
##D my_dt <- url2dt("https://www.dropbox.com/s/example/data.csv.zip?dl=0")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("url2dt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
