#' pFuzzyMatch_discrete
#'
#' Performs parallelized fuzzy matching of strings based on the string distance measure specified in `DistanceMeasure`. Matching is parallelized using all available CPU cores to increase execution speed.
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
#' `pFuzzyMatch` can automatically process the `by` text for each dataset. Users may specify the following options:
#'
#' - Set `DistanceMeasure` to control algorithm for computing pairwise string distances. Options include "`osa`", "`jaccard`", "`jw`". See `?stringdist::stringdist` for all options. (Default is "`jaccard`")
#'
#' - Set `MaxDist` to control the maximum allowed distance between two matched strings
#'
#' - Set `AveMatchNumberPerAlias` to control the maximum allowed distance between two matched strings. Takes priority over `MaxDist` if both specified.
#'
#' - Set `qgram` to control the character-level q-grams used in the distance measure. (Default is `2`)
#'
#' - Set `RemoveCommonWords` to TRUE to remove common words (those appearing in > 10% of aliases). (Default is `FALSE`)
#'
#' - Set `NormalizeSpaces` to TRUE to remove hanging whitespaces. (Default is `TRUE`)
#'
#' - Set `RemovePunctuation` to TRUE to remove punctuation. (Default is `TRUE`)
#'
#' - Set `ToLower` to TRUE to ignore case. (Default is `TRUE`)
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
#' @import data.table
#'
#' @export
#' @md

pDistMatch_discrete <- function(x, y, by = NULL, by.x = NULL, by.y = NULL,
                           return_stringdist = T, onlyUFT = T,
                           qgram =2, DistanceMeasure = "jaccard", MaxDist = 0.20,
                           ReturnProgress=T, nCores = NULL, 
                           ReturnMaxDistThreshold = F){
  library(stringdist)
  if(is.null(by.x) & is.null(by.y)){ by.x <- by.y <- by }
  if(swappedXY <- (nrow(y) < nrow(x))){
    # condition: y should be larger than x for efficient parallelization 
    # if not, swap
    x_old <- x; y_old <- y;
    by.y_old <- by.y;
    by.x_old <- by.x
    x <- y_old; by.x = by.y_old; rm(y_old)
    y <- x_old; by.y = by.x_old; rm(x_old)
  }
  if(by.x == by.y){
    colnames(x)[colnames(x) == by.x] <- paste(by.x, ".x", sep = "")
    colnames(y)[colnames(y) == by.y] <- paste(by.y, ".y", sep = "")
    by.x = paste(by.x, ".x", sep = "");by.y = paste(by.y, ".y", sep = "")
  }
  x = as.data.table(x); x$by.x_ORIG <- x[[by.x]]
  y = as.data.table(y); y$by.y_ORIG <- y[[by.y]]
  x_tri_index = trigram_index(x[[by.x]],"the.row")
  y_tri_index = trigram_index(y[[by.y]],"the.row")
  
  # test 
  # trigram_index(c("hi","hiaer", "hi","hi"),"the.row")

  # start pdist calc
  {
    split_list_x <- list(1:(n_iters <- nrow(x))) # # x is arbitrary reference (y is larger base)
    if(n_iters>50){
      split_list_x = round(seq(0.5,n_iters,length.out = nCores + 1))
      split_list_x = as.numeric(cut(1:n_iters,breaks=split_list_x))
      split_list_x = sapply(1:nCores, function(as){ list(which(split_list_x ==as))})
    }
    
    Export <- c("split_list_x", "DistanceMeasure", "qgram", "nCores", "ReturnProgress",
                "x_tri_index", "y_tri_index", "MaxDist",
                "x", "by.x", "y", "by.y")
    NoExport <- c(ls(), ls(parent.env(environment())), ls(globalenv()))
    NoExport <- NoExport[!NoExport %in% Export]
    loop_ <- foreach::foreach(outer_ix = 1:nCores,
                     .export = Export,
                     .noexport = NoExport
                     ) %dopar% {
      counter_ <- 0
      my_matched_inner <- matrix(NA, nrow = 0,ncol=3)
      for(ix in split_list_x[[outer_ix]]){
        counter_ = counter_ + 1
        if(ix %% 100==0 & ReturnProgress){write.csv(data.frame("Current Split" = outer_ix,
                                                   "Total Splits" = nCores,
                                                   "Current Iters in Split" = counter_,
                                                   "Total Iters in Split" = length(split_list_x[[outer_ix]])),
                                                   file = './PROGRESS_pDistMatch_discrete.csv')}
        
        # get the trigrams of the name, x[ix,][[by.x]]
        # find the set of entries in directory_LinkIt_red that have x% in common trigram
        MinNumSharedTriGrams <- ceiling( length(
                 my_entry_trigrams <- x_tri_index[the.row==ix, trigram] )*0.05 )
        LT_entries <- table( y_tri_index[trigram %fin% my_entry_trigrams, the.row] )
        if( length(LT_entries) > 0 ){
          LT_entries <- f2n(names(LT_entries[LT_entries >= MinNumSharedTriGrams]))
          dist_vec_red <- stringdist(x[ix,][[by.x]], # match my entry 
                                     y[LT_entries,][[by.y]],
                                     method = DistanceMeasure, q = qgram)
          iy <- LT_entries[ belowThres_ <- which(dist_vec_red <= MaxDist) ]
        if(length(iy) > 0){
          my_matched_inner = rbind(my_matched_inner,
                                    cbind(ix, matrix( c(iy, dist_vec_red[belowThres_]),
                                                      nrow = length(iy), ncol = 2L, byrow = F) ) )
        }
        }
      }
      return( my_matched_inner )
                     }
    myMatched = as.data.frame( do.call(rbind, loop_) )
    colnames(myMatched) <- c("ix","iy","stringdist")
  }
  if(swappedXY){
    myMatched <- cbind("ix" = myMatched[,"iy"],
                       "iy" = myMatched[,"ix"],
                       "stringdist" = myMatched[,"stringdist"])
  }
  return( myMatched )
  }

