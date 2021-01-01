#' Get match results
#'
#' Returns the game results for a given league season
#'
#' @param country the three character country code
#' @param gender gender of competition, either "M" or "F"
#' @param season_end_year the year the season concludes, in quotes, ie "2021"
#'
#' @return returns a dataframe with the results of the competition, season and gender
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_match_results(country = "ITA", gender = "M", season_end_year = "2020")
#' }


get_match_results <- function(country, gender, season_end_year) {

  print(paste0("Scraping ", country, " fist division for the ", season_end_year, " season (gender = ", gender, ")"))

  competitions <- .get_tier1_competitions()

  main_url <- "https://fbref.com"

  country <- country
  gender <- gender
  season <- season_end_year
  select_season <- .get_league_season_url(country, gender, season)


  fixtures_url <- xml2::read_html(select_season) %>%
    rvest::html_nodes(".hoversmooth") %>%
    rvest::html_nodes(".full") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>% .[grepl("Fixtures", .)] %>% paste0(main_url, .)


  season_summary <- xml2::read_html(fixtures_url) %>%
    rvest::html_table() %>% .[1] %>% data.frame() %>%
    dplyr::filter(.data$Date != "")


  suppressWarnings(
    season_summary <- season_summary %>%
      tidyr::separate(.data$Score, into = c("HomeGoals", "AwayGoals"), sep = "–") %>%
      dplyr::mutate(HomeGoals = as.numeric(.data$HomeGoals),
             AwayGoals = as.numeric(.data$AwayGoals),
             Attendance = as.numeric(gsub(",", "", .data$Attendance)))
  )

  season_summary <- cbind(season_end_year, season_summary)

  if(!any(stringr::str_detect(names(season_summary), "Round"))) {
    Round <- rep(NA, nrow(season_summary))
    season_summary <- cbind(Round, season_summary)
  }

  if(any(stringr::str_detect(names(season_summary), "xG"))) {
    season_summary <- season_summary %>%
      dplyr::select(Season=season_end_year, Round, .data$Wk, .data$Day, .data$Date, .data$Time, .data$Home, .data$HomeGoals, Home_xG=.data$xG, .data$Away, .data$AwayGoals, Away_xG=.data$xG.1, .data$Attendance, .data$Venue, .data$Referee, .data$Notes)
  } else {
    season_summary <- season_summary %>%
      dplyr::select(Season=season_end_year, Round, .data$Wk, .data$Day, .data$Date, .data$Time, .data$Home, .data$HomeGoals, .data$Away, .data$AwayGoals, .data$Attendance, .data$Venue, .data$Referee, .data$Notes)
  }


  return(season_summary)

}
