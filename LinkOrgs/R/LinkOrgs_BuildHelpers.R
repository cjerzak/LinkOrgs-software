#!/usr/bin/env Rscript
#' url2dt
#'
#' Downloads a .zip file from a URL as a data.table from a URL.
#'
#' @param url character string with the URL housing the data object.
#'
#' @param target_extension (default = `".csv"`) character string describing
#' the target extension of the file in the downloaded .zip folder.
#'
#' @return `z` The downloaded data object from the URL.
#' @export
#'
#' @details `url2dt` downloads a zipped `.csv` file and loads it into memory based on the input URL.
#'
#' @examples
#'
#' # Example download
#' my_dt <- url2dt(url="https://www.dropbox.com/s/iqf9ids77dckopf/Directory_LinkIt_bipartite_Embeddings.csv.zip?dl=0")
#'
#' @export
#'
#' @md

url2dt <- function(url){
  # clean URL if from dropbox
  url <- dropboxURL2downloadURL(url)

  # setup temporary folder, download .zip into it, unzip
  temp_folder <- tempdir()

  # download
  theExtension <- ifelse(grepl(url,pattern = "\\.csv\\.gz"),
                         yes = "gz", no = "zip")
  download.file( url, destfile =  (destfile_zip <- sprintf("%s/%s.%s",
                                                           temp_folder,
                                                           digest::digest(url),
                                                           theExtension)))

  # unzip into folder
  destfolder_unzip <- gsub(pattern = sprintf("\\.%s", theExtension),
                           replacement = "",
                           x = destfile_zip)
  unzip(destfile_zip, junkpaths = T, exdir = destfolder_unzip)

  # load unzipped file into memory as a data.table
  file_in_zip <- list.files( destfolder_unzip )
  file_in_zip <- grep(file_in_zip, pattern="\\.csv",value = T)
  returned_dt <- data.table::fread( sprintf("%s/%s", destfolder_unzip, file_in_zip) )

  # cleanup
  file.remove( destfile_zip )

  # return
  return( returned_dt )
}


#!/usr/bin/env Rscript
#' dropboxURL2downloadURL
#'
#' Downloads
#'
#' @param url character string with the URL housing the data object.
#'
#' @param target_extension (default = `".csv"`) character string describing
#' the target extension of the file in the downloaded .zip folder.
#'
#' @return `z` The
#' @export
#'
#' @details `dropboxURL2downloadURL`
#'
#' @examples
#'
#' # Example download
#' my_dt <- dropboxURL2downloadURL(url="https://www.dropbox.com/s/iqf9ids77dckopf/Directory_LinkIt_bipartite_Embeddings.csv.zip?dl=0")
#'
#' @export
#'
#' @md
dropboxURL2downloadURL <- function( url ){
  # clean URL if from dropbox
  url <- gsub(pattern = "https://www.dropbox.com",
              replacement = "https://dl.dropboxusercontent.com",
              x = url)
  url <- gsub(pattern = "www.dropbox.com",
              replacement = "dl.dropboxusercontent.com",
              x = url)
  url <- gsub(pattern = "dropbox.com",
              replacement = "dl.dropboxusercontent.com",
              x = url)

  # return
  return( url )
}

#' print2
#'
#' Prints a message with a timestamp prefix.
#'
#' @param text Character string to print.
#' @param quiet Logical; if TRUE, suppress output. Default is FALSE.
#'
#' @return Invisibly returns NULL. Called for its side effect of printing.
#'
#' @examples
#'
#' print2("Hello world!")
#'
#' @export
#' @md
print2 <- function(text, quiet = F){
  if(!quiet){ print( sprintf("[%s] %s" ,format(Sys.time(), "%Y-%m-%d %H:%M:%S"),text) ) }
}

f2n <- function(.){as.numeric(as.character(.))}

trigram_index <- function(phrase, phrasename = 'phrase.no', openBrowser = F){
  DT <- data.table(phrase, phrase.no = 1:length(phrase))
  t <- DT[, .(phrase, phrase.no, phrase.length = nchar(phrase))][
    data.table(start_pos = 1:100),
    .(phrase, phrase.no, start_pos),
    on = "phrase.length>=start_pos",
    nomatch = 0, allow.cartesian = T][
      order(phrase.no)]
  t[, end_pos := pmin(start_pos + 2, nchar(phrase))]
  directory_trigrams <- t[start_pos == 1 | start_pos + 2 == end_pos,
                          .(trigram = substr(phrase, start_pos, end_pos),
                            phrase.no)]
  setkey(directory_trigrams, trigram)
  colnames(directory_trigrams) <- c("trigram", phrasename)
  return(directory_trigrams)
}

inf20 <- function(ze){ if(is.infinite(ze)){ze<-0};ze}
na20 <- function(ze){ ze[is.na(ze)] <- 0;ze}

DeconflictNames <- function(z){
  names_raw <- colnames(z)
  names_clean <- gsub(pattern = "\\.y",
                      replacement = "",
                      x = gsub(pattern = "\\.x",
                              replacement = "",
                              x = names_raw))
  FracMatchAmongSharedCols <- tapply(1:ncol(z), names_clean, function(col_){
    value_ <- NA
    if(length(col_) == 2){
      value_ <- mean(z[,col_[1]] == z[,col_[2]], na.rm=T)
    }
    if(length(col_) == 3){
      value_ <- mean((z[,col_[1]] == z[,col_[2]]) & (z[,col_[1]] == z[,col_[3]]), na.rm=T)
    }
    return( value_ )
  })
  DropOneCopy <- na.omit(as.character(names(FracMatchAmongSharedCols[which(FracMatchAmongSharedCols  == 1)])))
  DropFlags <- na.omit(as.character(names(FracMatchAmongSharedCols[which( is.na(FracMatchAmongSharedCols) )])))
  KeepBothCopies <- na.omit(as.character(names(FracMatchAmongSharedCols[which(FracMatchAmongSharedCols  < 1)])))

  # drop flags if no collisions
  colnames(z)[colnames(z) %in% names_raw[names_clean %in% DropFlags] ] <- names_clean[names_clean %in% DropFlags]

  # drop flags if colliding with same output
  colnames(z)[colnames(z) %in% names_raw[names_clean %in% DropOneCopy] ] <- names_clean[names_clean %in% DropOneCopy]
  z <- z[,!duplicated(colnames(z))]
  return( z )
}


NA2ColMean <- function(data_){
  for(i in 1:ncol(data_)) { data_[is.na(data_[,i]), i] <- mean(data_[,i], na.rm = TRUE) }
  return(data_)
}
