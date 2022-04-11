

#' @importFrom purrr safely
#' @importFrom jsonlite fromJSON
.safely_from_json <- purrr::safely(jsonlite::fromJSON, otherwise = NULL, quiet = TRUE)

#' @importFrom rvest read_html html_elements html_text
#' @importFrom purrr keep
#' @importFrom stringr str_detect
.fotmob_extract_meta <- function() {
  page <- "https://www.fotmob.com/" %>%
    rvest::read_html()

  page %>%
    rvest::html_elements("script") %>%
    rvest::html_text(trim = TRUE) %>%
    purrr::keep(stringr::str_detect, "lsLeague")
}

#' @importFrom stringr str_extract
#' @importFrom jsonlite fromJSON
.fotmob_get_build_id <- function() {
  meta <- .fotmob_extract_meta()
  meta %>%
    stringr::str_extract('(?<=\\"buildId\\"[:]).*(?=\\,\\"isFallback\\")') %>%
    jsonlite::fromJSON()

}
