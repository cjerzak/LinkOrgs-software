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

url2dt <- function(url,
                   target_extension = ".csv"){
  # clean URL if from dropbox
  url <- dropboxURL2downloadURL(url)

  # setup temporary folder, download .zip into it, unzip
  temp_folder <- tempdir()

  # download
  download.file( url, destfile =  (destfile_zip <- sprintf("%s/%s.zip",
                                                           temp_folder,
                                                           digest::digest(url))))

  # unzip into folder
  destfolder_unzip <- gsub(destfile_zip, pattern="\\.zip", replace="")
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

dropboxURL2downloadURL <- function( url ){
  # clean URL if from dropbox
  url <- gsub(url, pattern="https://www.dropbox.com",
              replace="https://dl.dropboxusercontent.com")
  url <- gsub(url, pattern="www.dropbox.com", replace="dl.dropboxusercontent.com")
  url <- gsub(url, pattern="dropbox.com", replace="dl.dropboxusercontent.com")

  # return
  return( url )
}
