#' Read in stored RDS data
#'
#' Reads in RDS stored data files typically stored on GitHub
#'
#' @param file_url URL to RDS file(s) for reading in
#'
#' @return data type dependent on RDS file being read in
#'
#'

.file_reader <- function(file_url) {
  readRDS(url(file_url))
}
