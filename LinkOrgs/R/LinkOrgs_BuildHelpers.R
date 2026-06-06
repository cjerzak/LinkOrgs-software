#!/usr/bin/env Rscript
#' Download CSV from URL to data.table
#'
#' Downloads a zipped or gzipped CSV file from a URL and loads it into memory as a data.table.
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
#' @importFrom utils download.file read.csv unzip
#' @export
#' @md

url2dt <- function(url){
  # clean URL if from dropbox
  url <- dropboxURL2downloadURL(url)

  # setup temporary folder, download archive into it, and read the CSV
  temp_folder <- tempdir()

  theExtension <- ifelse(grepl(pattern = "\\.csv\\.gz($|[?])", x = url),
                         yes = "gz", no = "zip")
  destfile_archive <- sprintf("%s/%s.%s",
                              temp_folder,
                              digest::digest(url),
                              theExtension)
  if(file.exists(url)){
    file.copy(url, destfile_archive, overwrite = TRUE)
  } else {
    download.file(url, destfile = destfile_archive)
  }

  if(theExtension == "gz"){
    con <- gzfile(destfile_archive, open = "rt")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    returned_dt <- data.table::as.data.table(utils::read.csv(con, stringsAsFactors = FALSE))
  } else {
    destfolder_unzip <- gsub(pattern = sprintf("\\.%s", theExtension),
                             replacement = "",
                             x = destfile_archive)
    unzip(destfile_archive, junkpaths = T, exdir = destfolder_unzip)
    file_in_zip <- grep(list.files(destfolder_unzip), pattern = "\\.csv$", value = TRUE)
    returned_dt <- data.table::fread(sprintf("%s/%s", destfolder_unzip, file_in_zip[1]))
    unlink(destfolder_unzip, recursive = TRUE, force = TRUE)
  }

  # cleanup
  file.remove( destfile_archive )

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
print2 <- function(text, quiet = getOption("LinkOrgs.quiet", FALSE)){
  if(!quiet){ print( sprintf("[%s] %s" ,format(Sys.time(), "%Y-%m-%d %H:%M:%S"),text) ) }
}

LinkOrgsCacheDir <- function(create = TRUE){
  cache_dir <- Sys.getenv("LINKORGS_CACHE_DIR", unset = "")
  if(!nzchar(cache_dir)){
    if(Sys.info()[["sysname"]] == "Darwin"){
      cache_dir <- file.path(path.expand("~"), "Library", "Caches", "LinkOrgs")
    } else if(.Platform$OS.type == "windows" && nzchar(Sys.getenv("LOCALAPPDATA", unset = ""))){
      cache_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "LinkOrgs", "Cache")
    } else {
      cache_root <- Sys.getenv("XDG_CACHE_HOME", unset = file.path(path.expand("~"), ".cache"))
      cache_dir <- file.path(cache_root, "LinkOrgs")
    }
  }
  if(create && !dir.exists(cache_dir)){
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
}

LinkOrgsDownload <- function(url, destfile, quiet = FALSE){
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  download.file(url, destfile = destfile, quiet = quiet)
  destfile
}

LinkOrgsNetworkSubdir <- function(algorithm){
  ifelse(algorithm == "bipartite",
         yes = "directory_data_bipartite_thresh40",
         no = "directory_data_markov")
}

LinkOrgsNetworkDirectory <- function(algorithm, url, cache_dir = LinkOrgsCacheDir(), quiet = FALSE){
  subdir <- LinkOrgsNetworkSubdir(algorithm)
  has_network_files <- function(base_dir){
    file.exists(file.path(base_dir, subdir, sprintf("LinkIt_directory_%s_trigrams.Rdata", algorithm))) &&
      file.exists(file.path(base_dir, subdir, sprintf("LinkIt_directory_%s.Rdata", algorithm)))
  }

  bundled_dir <- system.file("extdata", sprintf("Directory_%s", algorithm), package = "LinkOrgs")
  if(nzchar(bundled_dir) && has_network_files(bundled_dir)){
    return(bundled_dir)
  }

  directory_loc <- file.path(cache_dir, sprintf("Directory_%s", algorithm))
  if(!has_network_files(directory_loc)){
    directory_zip_loc <- file.path(cache_dir, sprintf("Directory_%s.zip", algorithm))
    LinkOrgsDownload(url, directory_zip_loc, quiet = quiet)
    unzip(directory_zip_loc, exdir = directory_loc)
  }
  if(!has_network_files(directory_loc)){
    stop(sprintf("Could not find LinkOrgs %s network data in bundled files or cache.", algorithm))
  }
  directory_loc
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
