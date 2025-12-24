#!/usr/bin/env Rscript
#' Download CSV from URL to data.table
#'
#' Downloads a zipped CSV file from a URL and loads it into memory as a data.table.
#' Automatically handles Dropbox URLs by converting them to direct download links.
#'
#' @param url Character string; the URL pointing to a `.csv.zip` or `.csv.gz` file.
#'   Dropbox share links are automatically converted to direct download URLs.
#'
#' @return A data.table containing the downloaded data.
#'
#' @details This function:
#' 1. Converts Dropbox share links to direct download URLs using [dropboxURL2downloadURL()]
#' 2. Downloads the file to a temporary directory
#' 3. Unzips (if `.zip`) or decompresses (if `.gz`) the file
#' 4. Reads the CSV file using [data.table::fread()]
#' 5. Cleans up the temporary file
#'
#' @examples
#' \dontrun{
#' # Download from Dropbox
#' my_dt <- url2dt("https://www.dropbox.com/s/example/data.csv.zip?dl=0")
#' }
#'
#' @seealso [dropboxURL2downloadURL()] for URL conversion, [data.table::fread()]
#'   for the underlying CSV reader.
#' @importFrom utils download.file unzip
#' @export
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


#' Convert Dropbox Share URL to Direct Download URL
#'
#' Converts a Dropbox share link to a direct download URL by replacing the
#' domain with `dl.dropboxusercontent.com`.
#'
#' @param url Character string; a Dropbox share URL (e.g.,
#'   `"https://www.dropbox.com/s/..."` or `"https://dropbox.com/s/..."`).
#'
#' @return Character string; the converted direct download URL. If the input
#'   is not a Dropbox URL, it is returned unchanged.
#'
#' @details Dropbox share links require modification to enable direct file
#'   downloads. This function replaces:
#'   - `https://www.dropbox.com` with `https://dl.dropboxusercontent.com`
#'   - `www.dropbox.com` with `dl.dropboxusercontent.com`
#'   - `dropbox.com` with `dl.dropboxusercontent.com`
#'
#' @examples
#' # Convert a Dropbox share link
#' direct_url <- dropboxURL2downloadURL(
#'   "https://www.dropbox.com/s/abc123/myfile.csv?dl=0"
#' )
#' # Returns: "https://dl.dropboxusercontent.com/s/abc123/myfile.csv?dl=0"
#'
#' @seealso [url2dt()] which uses this function internally.
#' @export
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
