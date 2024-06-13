#' pDistMatch_euclidean
#'
#' Performs parallelized distance computation strings based on the string distance measure specified in `DistanceMeasure`. Matching is parallelized using all available CPU cores to increase execution speed.
#'
#' @param x,y data frames to be merged
#'
#' @param by,by.x,by.y specifications of the columns used for merging. We follow the general syntax of `base::merge`; see `?base::merge` for more details.
#'
#' @param ... For additional options, see ``Details''.
#'
#' @return z The merged data frame.
#' @export
#'
#' @details
#' `pDistMatch_euclidean` can automatically process the `by` text for each dataset. Users may specify the following options:
#'
#' - Set `DistanceMeasure` to control algorithm for computing pairwise string distances. Options include "`osa`", "`jaccard`", "`jw`". See `?stringdist::stringdist` for all options. (Default is "`jaccard`")
#'
#' @examples
#'
#' #Create synthetic data
#' x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
#' y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds co")
#' x <- data.frame("orgnames_x"=x_orgnames)
#' y <- data.frame("orgnames_y"=y_orgnames)
#' z <- data.frame("orgnames_x"=x_orgnames[1:2], "orgnames_y"=y_orgnames[1:2])
#' z_true <- data.frame("orgnames_x"=x_orgnames, "orgnames_y"=y_orgnames)
#'
#' # Perform merge
#' linkedOrgs_fuzzy <- pFuzzyMatch(x = x,
#'                        y = y,
#'                        by.x = "orgnames_x",
#'                        by.y = "orgnames_y")
#'
#' @import stringdist
#' @import plyr
#' @import doParallel
#' @import data.table
#'
#' @export
#' @md
#'
pDistMatch_euclidean <- function(embedx, embedy, MaxDist = NULL, ReturnProgress = T){
  # parallelized distance matching

  # broadcast across larger matrix for fast vectorization
  if(swappedXY <- (nrow(embedy) < nrow(embedx))){
    print2("Swapping for efficient use of vectorization...")
    # reason for condition: y should be larger than x for efficient vectorization potential
    embedy_old <- embedy
    embedx_old <- embedx
    embedy <- embedx_old
    embedx <- embedy_old
    rm(embedx_old, embedy_old); gc()
  }

  # cast to matrix
  print2("Casting to matrix")
  embedx <- as.matrix(embedx); embedy <- t(as.matrix(embedy))

  # parallelized dist calc
  library(foreach);cl <- doParallel::registerDoParallel(ncl<- max(c(1L,(parallel::detectCores() - 2L))));
  # print(  sort( sapply(ls(),function(x){object.size(get(x))}))  )

  # perform the op
  NoExport <- c(ls(), ls(parent.env(environment())), ls(globalenv()))
  NoExport <- NoExport[!NoExport %in% (Export <- c("embedx", "embedy", "MaxDist","ncl","ReturnProgress"))]

  CountDecimalPlaces <- function(xx) {
    xx <- sapply(c(xx), function(x){
    # Convert the number to a string
    str_num <- as.character(x)

    # Find the position of the decimal point
    decimal_pos <- regexpr("\\.", str_num)

    # Calculate the number of characters after the decimal point
    # If there is no decimal point, return 0
    if (decimal_pos < 0) { return(0) } else { return(nchar(substr(str_num, decimal_pos + 1, nchar(str_num)))) }
    })
    return( median(xx) )
  }

  UseDigits <- c(median(CountDecimalPlaces(embedx[1:min(c(10,nrow(embedx))), 1:10])),
                  median(CountDecimalPlaces(embedy[1:10,1:min(c(10,ncol(embedy)))])))
  if(which.min(UseDigits) == 1){ embedy <- round(embedy, min(UseDigits)) }
  if(which.min(UseDigits) == 2){ embedx <- round(embedx, min(UseDigits)) }

  print2(sprintf("Starting parallel Euclidean distance calculations with dim(x) = [%s, %s] and dim(y) = [%s, %s]...",
                dim(embedx)[1], dim(embedx)[2], dim(embedy)[2], dim(embedy)[1]))

  # open browser to analyze performance in large n case
   #if(ncol(embedy) > 100000){ browser() }

  if("jax" %in% ls()){
    embedy <- jnp$array(embedy, dtype = jnp$float16)
    ColDists_jit <- jax$jit(function(an_x){
      jnp$sqrt( 0.0001 + jnp$sum( (jnp$expand_dims(an_x,1L) - embedy)^2, 0L) )
    })
  }

  #gc(); distMat <- (foreach(ix = 1:nrow(embedx), .export = Export, .noexport = NoExport) %dopar% { # useful for small embedding dims
  gc(); library(Rfast); distMat <- lapply(1:nrow(embedx), function(ix){ # useful for big embedding dims
    if(ix %% 25 == 0 & ReturnProgress | ix < 10){ gc(); write.csv(data.frame("Current ix" = max(ix),
                                                           "Total ix" = nrow(embedx)),
                                                file = './PROGRESS_pDistMatch_euclidean.csv')}

    # calculate distances
    if(!"jax" %in% ls()){
       dist_vec <- colSums( (embedx[ix,] - embedy)^2 )^0.5  # embed2 is already transposed for row broadcasting
    }
    if("jax" %in% ls()){
      dist_vec <- np$array( ColDists_jit( jnp$array(embedx[ix,], jnp$float16) ) )
    }

    # select iy matches to ix
    iy <- ifelse(is.null(MaxDist),
                 yes = list( belowThres_ <- (1:length( dist_vec ))),
                 no = list( belowThres_ <- which(dist_vec <= MaxDist)))[[1]]

    match_ <- NULL; if(length(iy) > 0){
      match_ <- cbind(ix, matrix( c(iy, dist_vec[belowThres_]),
                                  nrow = length(iy),
                                  ncol = 2L, byrow = F))
    }
    return( list( match_ ))
  })
  print2("Done with Euclidean distance calculations...")
  gc(); distMat <- as.data.frame( do.call(rbind, unlist(distMat,recursive=F) ))
  colnames(distMat) <- c("ix","iy","stringdist")

  print2("Wrapping up call to pDistMatch_euclidean()...")
  if(swappedXY){
    distMat <- data.frame("ix" = distMat[,"iy"],
                          "iy" = distMat[,"ix"],
                          "stringdist" = distMat[,"stringdist"])
  }

  return( distMat )
}
