#' Get fbref League URLs
#'
#' Returns the URLs for season leagues of a selected country
#'
#' @param country the three character country code
#' @param gender gender of competition, either "M" or "F"
#' @param season_end_year the year the season(s) concludes (defaults to all available seasons)
#' @param tier the tier of the league, ie '1st' for the EPL or '2nd' for the Championship and so on
#'
#' @return returns a character vector of all fbref league URLs for selected country, season, gender and tier
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fb_league_urls(country = "ENG", gender = "M", season_end_year = 2021, tier = '1st')
#' }
fb_league_urls <- function(country, gender, season_end_year, tier = "1st") {

  print("Getting league URLs")

  main_url <- "https://fbref.com"

  country_abbr <- country
  gender_M_F <- gender
  season_end_year_num <- season_end_year
  comp_tier <- tier

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv")

  league_seasons_urls <- seasons %>%
    dplyr::filter(.data$country %in% country_abbr,
           .data$gender %in% gender_M_F,
           .data$season_end_year %in% season_end_year_num,
           .data$tier %in% comp_tier) %>%
    dplyr::pull(.data$seasons_urls)

  return(league_seasons_urls)

}


#' Get fbref Team URLs
#'
#' Returns the URLs for all teams for a given league
#'
#' @param league_url the league URL (can be from fb_league_urls())
#'
#' @return returns a character vector of all fbref team URLs for a selected league
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
#' }
fb_teams_urls <- function(league_url) {

  print("Scraping team URLs")

  league_season_page <- xml2::read_html(league_url)

  main_url <- "https://fbref.com"

  urls <- league_season_page %>%
    rvest::html_nodes("img+ a") %>%
    rvest::html_attr("href") %>%
    unique() %>% paste0(main_url, .)

  return(urls)
}


#' Get fbref Player URLs
#'
#' Returns the URLs for all players for a given team
#'
#' @param team_url the player's team URL (can be from fb_team_urls())
#'
#' @return returns a character vector of all fbref player URLs for a selected team
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fb_player_urls("https://fbref.com/en/squads/fd962109/Fulham-Stats")
#' }
fb_player_urls <- function(team_url) {

  print("Scraping Player URLs")

  main_url <- "https://fbref.com"

  player_page <- xml2::read_html(team_url)

  player_urls <- player_page %>%
    rvest::html_nodes("#all_stats_standard th a") %>%
    rvest::html_attr("href") %>%
    paste0(main_url, .)

  return(player_urls)
}



