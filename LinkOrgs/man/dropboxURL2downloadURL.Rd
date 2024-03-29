% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LinkOrgs_BuildHelpers.R
\name{dropboxURL2downloadURL}
\alias{dropboxURL2downloadURL}
\title{dropboxURL2downloadURL}
\usage{
dropboxURL2downloadURL(url)
}
\arguments{
\item{url}{character string with the URL housing the data object.}

\item{target_extension}{(default = \code{".csv"}) character string describing
the target extension of the file in the downloaded .zip folder.}
}
\value{
\code{z} The
}
\description{
Downloads
}
\details{
\code{dropboxURL2downloadURL}
}
\examples{

# Example download
my_dt <- dropboxURL2downloadURL(url="https://www.dropbox.com/s/iqf9ids77dckopf/Directory_LinkIt_bipartite_Embeddings.csv.zip?dl=0")

}
