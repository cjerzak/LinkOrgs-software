#' pFuzzyMatch
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

pFuzzyMatch <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, return_stringdist = T, onlyUFT = T,
                           qgram =2, DistanceMeasure = "jaccard", MaxDist = 0.20,
                           AverageReference = NULL,
                           AveMatchNumberPerAlias=NULL,openBrowser=F,ReturnProgress=T,
                           ReturnMaxDistThreshold = F){
  library(stringdist)
  f2n <- function(.){as.numeric(as.character(.))}
  if(openBrowser == T){browser()}

  #WARNING: X SHOULD ALWAYS BE THE LARGER SET
  if(is.null(by.x) & is.null(by.y)){by.x <- by.y <- by}
  if(nrow(y)>nrow(x)){
    x_old = x; y_old = y;by.y_old = by.y;by.x_old =by.x
    x <- y_old;by.x = by.y_old
    y <- x_old;by.y = by.x_old
    rm(x_old,y_old)
  }
  if(by.x == by.y){
    colnames(x)[colnames(x) == by.x] <- paste(by.x, ".x", sep = "")
    colnames(y)[colnames(y) == by.y] <- paste(by.y, ".y", sep = "")
    by.x = paste(by.x, ".x", sep = "");by.y = paste(by.y, ".y", sep = "")
  }
  y = as.data.table(y)
  x = as.data.table(x)
  x$by.x_ORIG <- x[[by.x]]
  y$by.y_ORIG <- y[[by.y]]
  x[[by.x]] <- tolower(x[[by.x]] )
  y[[by.y]] <- tolower(y[[by.y]] )
  x_tri_index = trigram_index(x[[by.x]],"the.row")
  y_tri_index = trigram_index(y[[by.y]],"the.row")
  n_iters = max(nrow(x), nrow(y))

  MIN_MATCH_DIST_fuzzy <- MaxDist
  if(!is.null(AveMatchNumberPerAlias)){
    MATCH_DIST_fuzzy <- replicate(nFuzzySamp <- 20000,
                            {  stringdist(sample(x[[by.x]],1), sample(y[[by.y]],1), method = DistanceMeasure, q = qgram) })

    log_NPossibleMatches <- log(f2n(length(y[[by.y]]))) + log(f2n(length(x[[by.x]])))
    ReferenceNumb2 <- max(c(length(x[[by.x]]),length(y[[by.y]])))
    if(!is.null(AverageReference)){
      ReferenceNumb2 <- ifelse(AverageReference == "x",
                               yes = length(x[[by.x]]),
                               no = length(y[[by.y]]))
    }
    log_NObsTimesMatchesPerObs <- log(AveMatchNumberPerAlias) + log( ReferenceNumb2  )
    ImpliedQuantile <- exp(log_NObsTimesMatchesPerObs - log_NPossibleMatches)

    #MIN_MATCH_DIST_fuzzy <- mean(extremeStat::distLquantile(MATCH_DIST_fuzzy, probs = ImpliedQuantile )[1:3,1])
    MIN_MATCH_DIST_fuzzy <- quantile(MATCH_DIST_fuzzy,
                                     probs = min(1,ImpliedQuantile))
    if('try-error' %in% class(MIN_MATCH_DIST_fuzzy) | is.na(MIN_MATCH_DIST_fuzzy)){
      MIN_MATCH_DIST_fuzzy <- quantile(MATCH_DIST_fuzzy,  probs = ImpliedQuantile)
    }
    MIN_MATCH_DIST_fuzzy <- f2n(  MIN_MATCH_DIST_fuzzy )
  }
  if(ReturnMaxDistThreshold){return( MIN_MATCH_DIST_fuzzy )}

  {
    library("foreach",quietly=T); library("doMC",quietly=T)
    ncl <- 1; split_list <- list(1:n_iters)
    if(n_iters>50){
      ncl = parallel::detectCores()
      split_list = round(seq(0.5,n_iters,length.out = ncl+1))
      split_list = as.numeric(cut(1:n_iters,breaks=split_list))
      split_list = sapply(1:ncl, function(as){ list(which(split_list ==as))})
    }
    f2n = function(.){as.numeric(as.character(.))}
    cl<-doMC::registerDoMC(ncl);
    loop_ <- foreach(outer_i = 1:ncl) %dopar% {
      counter_ <- 0
      my_matched_inner = matrix(NA,nrow = 0,ncol=3)
      colnames(my_matched_inner) <- c("my_entry",by.y,"stringdist")
      for(i in split_list[[outer_i]]){
        counter_ = counter_ + 1
        if(i %% 100==0 & ReturnProgress){write.csv(data.frame("Current Split"=outer_i,
                                                   "Total Splits"=ncl,
                                                   "Current Iters in Split"=counter_,
                                                   "Total Iters in Split"=length(split_list[[outer_i]])),
                                                   file='./PROGRESS_FUZZY.csv')}


        #get the name we want to fuzzy match against
        my_entry = x[i,][[by.x]]
        #get the trigrams of this name
        my_entry_trigrams = x_tri_index[the.row==i,trigram]

        #find the set of entries in directory_LinkIt_red that have some common trigram
        #LT_entries = unique(x_tri_index[trigram %fin% my_entry_trigrams,the.row])
        if(nrow(y)<1e5){
          LT_entries = 1:nrow(y)
        }
        if(nrow(y)>=1e5){
          MinNumSharedTriGrams = ceiling(length(my_entry_trigrams)*0.1)
          LT_entries = table(y_tri_index[trigram %fin% my_entry_trigrams,the.row])
          LT_entries = f2n(names(LT_entries[LT_entries>=MinNumSharedTriGrams]))
        }

        #calculate the nearest match accordfng to string distance
        match_ = sprintf("y[LT_entries,.(
                         my_entry=my_entry,%s,
                         stringdist = stringdist(my_entry,%s,method=DistanceMeasure,q = qgram))]",by.y,by.y)
        match_ = eval(parse(text=match_))
        if(nrow(match_)>0){
          #match_ = match_[,.(which(stringdist<=MaxDist)) ]
          match_ = as.data.frame(match_)
          match_ = match_[which(match_$stringdist <= MIN_MATCH_DIST_fuzzy ),]
          my_matched_inner = rbind(my_matched_inner,match_)
        }
      }
      return( my_matched_inner )
    }
    my_matched = do.call(rbind,loop_)
}
  colnames(my_matched)[1] <- by.x
  myMatched = merge(as.data.frame(x), as.data.frame(my_matched),
                    by.x=by.x,by.y=by.x,all.x = F,all.y=T)
  myMatched = merge(as.data.frame(y), as.data.frame(myMatched),
                    by.x=by.y,by.y=by.y,all.x = F,all.y=T)
  myMatched = as.data.frame( myMatched )

  myMatched = myMatched[!duplicated(paste(myMatched[[by.x]],
                                          myMatched[[by.y]],sep="__")),]

  # revert back to original names
  myMatched[[by.x]] <- myMatched[["by.x_ORIG"]]
  myMatched[[by.y]] <- myMatched[["by.y_ORIG"]]
  return( myMatched )
  }

