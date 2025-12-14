#' AssessMatchPerformance
#'
#' Automatically computes the true/false positive and true/false negative rates based on a ground-truth (preferably human-generated) matched dataset.
#'
#' @usage
#'
#' AssessMatchPerformance(x,y,by,...)
#'
#' @param x,y data frames to be merged
#'
#' @param by,by.x,by.y character strings specifying of the columns used for merging.
#'
#' @param z the merged data frame to be analyzed. Should contain `by`,`by.x`, and/or `by.y` as column names, depending on usage.
#'
#' @param z_true a reference data frame containing target/true matched dataset. Should contain `by`,`by.x`, and/or `by.y` as column names, depending on usage.
#'
#' @return `ResultsMatrix` A matrix containing the information on the true positive, false positive,
#' true negative, and false negative rate, in addition to the matched dataset size.  These quantities are calculated based off
#' all possible `nrow(x)*nrow(y)` match pairs.
#'
#' @examples
#' # Create synthetic data
#' x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
#' y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
#' x <- data.frame("orgnames_x"=x_orgnames)
#' y <- data.frame("orgnames_y"=y_orgnames)
#' z <- data.frame("orgnames_x"=x_orgnames[1:2], "orgnames_y"=y_orgnames[1:2])
#' z_true <- data.frame("orgnames_x"=x_orgnames, "orgnames_y"=y_orgnames)
#'
#' # Obtain match performance data
#' PerformanceMatrix <- AssessMatchPerformance(x = x,
#'                                    y = y,
#'                                    z = z,
#'                                    z_true = z_true,
#'                                    by.x = "orgnames_x",
#'                                    by.y = "orgnames_y")
#' print( PerformanceMatrix )
#'
#'
#' @export
#' @md

AssessMatchPerformance = function(x, y, z, z_true, by, by.x=by, by.y=by, openBrowser=F){
  x[[by.x]] <- enc2utf8(x[[by.x]])
  y[[by.y]] <- enc2utf8(y[[by.y]])

  try(z[[by.x]] <- enc2utf8(z[[by.x]]),T)
  try(z[[by.y]] <- enc2utf8(z[[by.y]]),T)

  z_true[[by.x]] <- enc2utf8(z_true[[by.x]])
  z_true[[by.y]] <- enc2utf8(z_true[[by.y]])

  if(openBrowser==T){browser()}
  `%fin%` <- function(x, table) {fmatch(x, table, nomatch = 0L) > 0L}
  x <- as.matrix(x);y <- as.matrix(y);z <- as.matrix(z);z_true <- as.matrix(z_true);
  totalCombs <- length( unique(x[,by.x]) ) * length( unique(y[,by.y]) )
  ResultsMatrix =  c(matrix(0,nrow=1,ncol=5) )
  names(ResultsMatrix) <- c("TruePositives",
                            "FalsePositives",
                            "FalseNegatives",
                            "TrueNegatives",
                            "MatchedDatasetSize")

  #drop remaining duplicates
  dup_z_ <- duplicated(paste(z[,by.x], z[,by.y], sep= "___"))
  dup_zhuman_ <- duplicated(paste(z_true[,by.x],z_true[,by.y],sep= "___"))
  if( length(dup_z_) > 0 & nrow(z)>1 ){ z <- z[!dup_z_,] }
  if( length(dup_z_) > 0 & nrow(z_true)>1 ){ z_true <- z_true[!dup_zhuman_,]  }

  {
    z_vec = paste(z[,by.x], z[,by.y], sep="____LINKED____")
    z_true_vec <- paste(z_true[,by.x],z_true[,by.y],sep="____LINKED____")
    z_in_truth <- table(  z_vec %fin% z_true_vec )
    truth_in_z <- table(  z_true_vec %fin% z_vec )
    NA20 <- function(ze){ if(is.na(ze)){ze <- 0};ze}
    ResultsMatrix["TruePositives"] <- NA20(z_in_truth["TRUE"])
    ResultsMatrix["FalsePositives"] <- NA20(z_in_truth["FALSE"])
    ResultsMatrix["FalseNegatives"] <- NA20(truth_in_z["FALSE"])
    ResultsMatrix["TrueNegatives"] <- totalCombs -  ResultsMatrix["TruePositives"] - ResultsMatrix["FalsePositives"]
  }
  ResultsMatrix["MatchedDatasetSize"] <- nrow(z)
  return( ResultsMatrix  )
}
