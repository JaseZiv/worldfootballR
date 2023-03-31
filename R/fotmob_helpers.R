#' @importFrom rvest read_html html_elements html_text
#' @importFrom purrr keep
#' @importFrom stringr str_detect
.fotmob_extract_meta <- function() {
  url <- "https://www.fotmob.com/"
  resp <- safely_get_content(url)
  if (is.null(resp$result)) {
    stop(
      sprintf("Error in `.fotmob_extract_meta`:\n%s", resp$error)
    )
  }

  resp$result %>%
    rvest::html_elements("script") %>%
    rvest::html_text(trim = TRUE) %>%
    purrr::keep(stringr::str_detect, "props")
}

#' @importFrom stringr str_extract
#' @importFrom purrr pluck
.fotmob_get_build_id <- function() {
  meta <- .fotmob_extract_meta()
  meta %>%
    stringr::str_extract('(?<=\\"buildId\\"[:]).*(?=\\,\\"isFallback\\")') %>%
    safely_from_json() %>%
    pluck("result")
}

#' @importFrom purrr map_lgl
all_list_len1 <- function(x) {
  is.list(x) & all(
    purrr::map_lgl(
      x,
      ~length(.x) == 1L
    )
  )
}

#' @importFrom tidyr unnest
#' @importFrom tidyselect vars_select_helpers
#' @noRd
unnest_where_all_list_len1 <- function(df) {
  ## stopping conditions
  list_columns_to_unnest  <- names(df)[sapply(df, all_list_len1)]
  if (length(list_columns_to_unnest) == 0) {
    return(df)
  }

  print(sprintf('unnesting %s list len 1 cols:\n%s', length(list_columns_to_unnest), paste(list_columns_to_unnest, collapse = '\n')))
  for (col in list_columns_to_unnest) {
    df <- tidyr::unnest(df, tidyselect::vars_select_helpers$all_of(col))
  }

  df_columns_to_unnest <- names(df)[sapply(df, is.data.frame)]
  if (length(df_columns_to_unnest) > 0) {
    df <- unnest_where_is_df(df)
  }

  ## recurse
  unnest_where_all_list_len1(df)
}

#' @noRd
unnest_where_is_df <- function(df) {
  df_columns_to_unnest <- names(df)[sapply(df, is.data.frame)]
  if (length(df_columns_to_unnest) == 0) {
    return(df)
  }

  print(sprintf('unnesting %s df cols:\n%s', length(df_columns_to_unnest), paste(df_columns_to_unnest, collapse = '\n')))
  for(col in df_columns_to_unnest) {
    df[[col]] <- unnest_where_all_list_len1(df)
  }

  df
}

#' Unnest data.frame where reasonable
#'
#' Unnest all list columns where all rows have length 1. And do the same for all data.frame columns.
#'
#' @param df data.frame
#'
#' @importFrom janitor clean_names
#' @export
auto_unnest_df <- function(df) {
  df |>
    unnest_where_all_list_len1() %>%
    unnest_where_is_df() %>%
    janitor::clean_names()
}

#' Re-parse JSON
#'
#' This is for backwards compatability with result formats when we used jsonlite::fromJSON({raw url})
#' Sometimes httr::content() doesn't parse the result in the same manner, so we can "reset" the JSON resul
#' to a format closer to what it would have been before.
#'
#' @param x list to re-parse
#' @importFrom jsonlite toJSON fromJSON
#' @noRd
reset_json <- function(x) {
  jsonlite::fromJSON(jsonlite::toJSON(x))
}
