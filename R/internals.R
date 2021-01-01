#' Get Tier 1 competitions
#'
#' Returns a df of the top leagues around the world
#'
#' @return returns a dataframe with the tier 1 competitions from around the world
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'

.get_tier1_competitions <- function() {
  main_url <- "https://fbref.com"
  # read page to all competitions
  all_comps_url <- xml2::read_html("https://fbref.com/en/comps/")
  # this just gets the Tier 1 club comps - this will need to be modified if more comps are required
  comps <- all_comps_url %>% rvest::html_nodes("#all_comps_1_fa_club_league_senior")
  # get the urls for each competition, then paste fbref url
  competition_urls <- comps %>% rvest::html_node("tbody") %>% rvest::html_nodes("th a") %>% rvest::html_attr("href")
  competition_urls <- paste0(main_url, competition_urls)
  # scrape the table that contains the competitons
  competitions <- comps %>% rvest::html_nodes(".sortable") %>% rvest::html_table() %>% data.frame()
  # add the competition url column
  competitions <- cbind(competitions, competition_urls)
  # remove the two character country code for the flag, and only leave the 3 character code
  competitions$Country <- gsub(".*? ", "", competitions$Country)

  return(competitions)
}



#' Get URL of league season
#'
#' Returns a URL for the selected league seasons
#'
#' @param country the three character country code
#' @param gender gender of competition, either "M" or "F"
#' @param season the year the season concludes, in quotes, ie "2021"

#' @return a URL for the selected league seasons
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
.get_league_season_url <- function(country, gender, season) {
  main_url <- "https://fbref.com"

  competitions <- .get_tier1_competitions()

  league_url <- competitions %>%
    dplyr::filter(toupper(.data$Country) == toupper(country),
           toupper(.data$Gender) == toupper(gender)) %>%
    dplyr::pull(.data$competition_urls)

  league_url <- xml2::read_html(league_url)

  seasons <- league_url %>%
    rvest::html_nodes("th a") %>%
    rvest::html_text() %>%
    gsub(".*-", "", .)

  seasons_urls <- league_url %>%
    rvest::html_nodes("th a") %>%
    rvest::html_attr("href") %>%
    paste0(main_url, .)

  select_season <- cbind(seasons, seasons_urls) %>% data.frame() %>%
    dplyr::filter(seasons == season) %>%
    dplyr::pull(seasons_urls)

  return(select_season)

}
