% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LinkOrgs_AssessMatches.R
\name{AssessMatchPerformance}
\alias{AssessMatchPerformance}
\title{AssessMatchPerformance}
\usage{
AssessMatchPerformance(x,y,by,...)
}
\arguments{
\item{x, y}{data frames to be merged}

\item{z}{the merged data frame to be analyzed. Should contain \code{by},\code{by.x}, and/or \code{by.y} as column names, depending on usage.}

\item{z_true}{a reference data frame containing target/true matched dataset. Should contain \code{by},\code{by.x}, and/or \code{by.y} as column names, depending on usage.}

\item{by, by.x, by.y}{character strings specifying of the columns used for merging.}
}
\value{
\code{ResultsMatrix} A matrix containing the information on the true positive, false positive,
true negative, and false negative rate, in addition to the matched dataset size.  These quantities are calculated based off
all possible \code{nrow(x)*nrow(y)} match pairs.
}
\description{
Automatically computes the true/false positive and true/false negative rates based on a ground-truth (preferably human-generated) matched dataset.
}
\examples{
# Create synthetic data
x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
x <- data.frame("orgnames_x"=x_orgnames)
y <- data.frame("orgnames_y"=y_orgnames)
z <- data.frame("orgnames_x"=x_orgnames[1:2], "orgnames_y"=y_orgnames[1:2])
z_true <- data.frame("orgnames_x"=x_orgnames, "orgnames_y"=y_orgnames)

# Obtain match performance data
PerformanceMatrix <- AssessMatchPerformance(x = x,
                                   y = y,
                                   z = z,
                                   z_true = z_true,
                                   by.x = "orgnames_x",
                                   by.y = "orgnames_y")
print( PerformanceMatrix )


}
