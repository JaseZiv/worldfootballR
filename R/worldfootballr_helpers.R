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

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  league_seasons_urls <- seasons %>%
    dplyr::filter(.data$country %in% country_abbr,
           .data$gender %in% gender_M_F,
           .data$season_end_year %in% season_end_year_num,
           .data$tier %in% comp_tier) %>%
    dplyr::pull(.data$seasons_urls) %>% unique()

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


#' Get transfermarkt Team URLs
#'
#' Returns the URLs for all teams for a given league season
#'
#' @param country_name the country of the league's players
#' @param start_year the start year of the season (2020 for the 20/21 season)
#' @param league_url league url from transfermarkt.com. To be used when country_name not avalilable in main function
#'
#' @return returns a character vector of all transfermarkt team URLs for a selected league
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' team_urls <- tm_league_team_urls(country_name = "England", start_year = 2020)
#' }
tm_league_team_urls <- function(country_name, start_year, league_url = NA) {
  main_url <- "https://www.transfermarkt.com"

  if(is.na(league_url)) {
    meta_df <- read.csv(url("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"),
                        stringsAsFactors = F)

    tryCatch({meta_df_seasons <- meta_df %>%
      dplyr::filter(.data$country %in% country_name, .data$season_start_year %in% start_year)}, error = function(e) {meta_df_seasons <- data.frame()})

    if(nrow(meta_df_seasons) == 0) {
      stop(glue::glue("Country {country_name} or season {start_year} not found. Check that the country and season exists at https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"))
    }

    season_url <- meta_df_seasons$season_urls

  } else {
    tryCatch({league_page <- xml2::read_html(league_url)}, error = function(e) {league_page <- c()})

    tryCatch({country_name <- league_page %>%
      rvest::html_nodes(".profilheader") %>%
      rvest::html_node("img") %>%
      rvest::html_attr("alt") %>% .[!is.na(.)]}, error = function(e) {country_name <- NA_character_})

    if(length(league_page) == 0) {
      stop(glue::glue("League URL(s) {league_url} not found. Please check transfermarkt.com for the correct league URL"))
    }

    season_url <- paste0(league_url, "?saison_id=", start_year)

  }

  season_page <- xml2::read_html(season_url)

  team_urls <- season_page %>%
    rvest::html_nodes(".items") %>%
    rvest::html_nodes(".vereinprofil_tooltip") %>% rvest::html_attr("href") %>%
    unique() %>% paste0(main_url, .)

  return(team_urls)
}


#' Get transfermarkt Player URLs
#'
#' Returns the transfermarkt URLs for all players for a given team
#'
#' @param team_url the player's team URL (can be from tm_league_team_urls())
#'
#' @return returns a character vector of all transfermarkt player URLs for a selected team
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' team_url <- "https://www.transfermarkt.com/fc-burnley/startseite/verein/1132/saison_id/2020"
#' tm_team_player_urls(team_url = team_url)
#' }
tm_team_player_urls <- function(team_url) {

  main_url <- "https://www.transfermarkt.com"

  tryCatch({team_page <- xml2::read_html(team_url)}, error = function(e) {team_page <- c()})

  player_urls <- team_page %>%
    rvest::html_nodes("#yw1") %>% rvest::html_nodes(".hauptlink") %>%
    rvest::html_nodes("a") %>% rvest::html_attr("href") %>%
    unique() %>%
    paste0(main_url, .)

  return(player_urls)
}

