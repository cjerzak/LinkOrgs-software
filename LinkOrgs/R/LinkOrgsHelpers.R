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
  url <- gsub(url, pattern="https://www.dropbox.com",
              replace="https://dl.dropboxusercontent.com")
  url <- gsub(url, pattern="www.dropbox.com", replace="dl.dropboxusercontent.com")
  url <- gsub(url, pattern="dropbox.com", replace="dl.dropboxusercontent.com")

  # setup temporary folder, download .zip into it, unzip
  temp_folder_name_zip <- "tmp1432351232142323196039z"
  temp_folder_zip <- tempfile(pattern = temp_folder_name_zip)

  temp_folder_name_unzip <- "tmp1432351232142323196039u"
  temp_folder_unzip <- tempfile(pattern = temp_folder_name_unzip)

  # download
  download.file( url, destfile =  temp_folder_zip)

  # unzip into folder
  unzip(temp_folder_zip,
        junkpaths = T,
        exdir = temp_folder_unzip)

  # load unzipped file into memory as a data.table
  file_in_zip <- list.files( temp_folder_unzip )
  file_in_zip <- grep(file_in_zip,pattern="\\.csv",value = T)
  returned_dt <- data.table::fread(
        sprintf("%s/%s", temp_folder_unzip, file_in_zip) )

  # cleanup
  file.remove( temp_folder_zip )

  # return
  return( returned_dt )
}
