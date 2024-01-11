#'
#' A primarily internal function which builds the organizational record linkage models used in Libgober and Jerzak (2023+).
#'
#' @usage
#'
#' BuildML(...)
#'
#' @export
#'
#' @md


BuildTransfer <- function(){
  getRepresentation_transfer <- function(names){
    text_options <- list(tokenizer_parallelism = F,
                         model = "bert-base-multilingual-uncased",
                         layers = -1L,
                         device = "cpu",
                         logging_level = "error")
    transferRep = readme::undergrad(
      documentText = names,
      numericization_method = "transformer_based",
      textEmbed_control = text_options)
    return( transferRep )
  }
  getProbMat_transfer <- function(names_raw1,
                                  names_raw2,
                                  embed1_t = NULL,
                                  embed2_t = NULL,
                                  training,
                                  roundAt = 6L,
                                  AcceptThreshold = NULL){

    library(foreach);cl <- doMC::registerDoMC(ncl<-parallel::detectCores());
    n_iters <- length(names_raw1)
    if(! "ReturnProgress" %in% ls()){ ReturnProgress <- T }

    transformerSummaries2 <- embed2_t
    probMat <- foreach(ier = 1:n_iters) %dopar% {
      if(ier %% 100 == 0 & ReturnProgress){write.csv(data.frame("Current Iters"=ier,"Total Iters"=n_iters),file='./PROGRESS_LINKIT_ml.csv')}
      transformerSummaries1 <- c(embed1_t[,ier])

      # get representations
      transformerSummariesDiff <- rbind(
           rbind((globalCosSim <- colSums( transformerSummaries1*transformerSummaries2)/(sum( transformerSummaries1^2)*colSums(transformerSummaries2^2))^0.5),
                (abs(max(transformerSummaries1)-apply(transformerSummaries2,2,max))),
                (abs(mean(transformerSummaries1)-apply(transformerSummaries2,2,median))),
                (abs(min(transformerSummaries1)-apply(transformerSummaries2,2,min)))),
                                        abs(transformerSummaries1-transformerSummaries2),
                                        transformerSummaries1*transformerSummaries2
      )

      matchProb_vec <- transferCoefs %*% rbind(1,transformerSummariesDiff)

      if(is.null(AcceptThreshold)){  match_indices <- 1:length(matchProb_vec) }
      if(!is.null(AcceptThreshold)){  match_indices <- which(matchProb_vec >= AcceptThreshold) }
      if(length(match_indices) > 0){
        match_ <- data.frame(
          "x1" = names_raw1[ier],
          "i1" = ier,
          "x2" = names_raw2[match_indices],
          "i2" = match_indices,
          "matchprob"= (matchProb_vec[match_indices]),
          "stringdist"= (1-matchProb_vec[match_indices]))
      }
      return( list(match_ ))
    }
    probMat <- unlist(probMat,recursive=F)
    probMat <- as.data.frame( do.call(rbind,probMat) )

    # return
    return(  probMat  )
    }
}
