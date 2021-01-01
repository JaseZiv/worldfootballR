#' Get match URLs
#'
#' Returns the URL for each match played for a given league season
#'
#' @param country the three character country code
#' @param gender gender of competition, either "M" or "F"
#' @param season the year the season concludes, in quotes, ie "2021"
#'
#' @return returns a character vector of all fbref match URLs for a given competition, season and gender
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_match_urls(country = "ENG", gender = "M", season_end_year = "2020")
#' }

get_match_urls <- function(country, gender, season) {
  main_url <- "https://fbref.com"

  country <- toupper(country)
  gender <- toupper(gender)
  season <- season

  selected_season <- .get_league_season_url(country, gender, season)

  fixtures_url <- xml2::read_html(selected_season) %>%
    rvest::html_nodes(".hoversmooth") %>%
    rvest::html_nodes(".full") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>% .[grepl("Fixtures", .)] %>% paste0(main_url, .)

  match_report_urls <- xml2::read_html(fixtures_url) %>%
    # html_nodes(".left~ .left+ .left a") %>%
    rvest::html_nodes("td.left~ .left+ .left a") %>%
    rvest::html_attr("href") %>%
    paste0(main_url, .) %>% unique()

  return(match_report_urls)

}
