#' Get Understat season match results
#'
#' Returns match results for all matches played in the selected league season from Understat.com
#'
#' @param league the available leagues in Understat as outlined below
#' @param season_start_year the year the season started
#'
#' The leagues currently available for Understat are:
#' \emph{"EPL"}, \emph{"La liga}", \emph{"Bundesliga"},
#' \emph{"Serie A"}, \emph{"Ligue 1"}, \emph{"RFPL"}
#'
#' @return returns a dataframe of match results for a selected league season
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \donttest{
#' understat_league_match_results(league = "EPL", season_start_year = 2020)
#' }
understat_league_match_results <- function(league, season_start_year) {
  # .pkg_message("Scraping match results data for {league} {season_start_year} season. Please acknowledge understat.com as the data source")
  main_url <- "https://understat.com/"

  leagues <- c("EPL", "La liga", "Bundesliga", "Serie A", "Ligue 1", "RFPL")
  if(!league %in% leagues) stop("Check league name")

  if(league == "La liga") {
    league <- "La_liga"
  } else if (league == "Serie A") {
    league <- "Serie_A"
  } else if (league == "Ligue 1") {
    league <- "Ligue_1"
  }

  league_url <- paste0(main_url, "league/", league, "/", season_start_year)

  # to get available seasons:
  # league_page <- xml2::read_html(league_url)
  # avail_seasons <- league_page %>% rvest::html_nodes(xpath = '//*[@name="season"]') %>%
  #   rvest::html_nodes("option") %>% rvest::html_text() %>% gsub("/.*", "", .)

  match_results <- .get_clean_understat_json(page_url = league_url, script_name = "datesData") %>%
    dplyr::filter(.data$isResult)

  match_results <- cbind(league, match_results)

  match_results <- match_results %>%
    dplyr::rename(match_id=.data$id, home_id=.data$h.id, home_team=.data$h.title, home_abbr=.data$h.short_title, away_id=.data$a.id, away_team=.data$a.title, away_abbr=.data$a.short_title,
                  home_goals=.data$goals.h, away_goals=.data$goals.a, home_xG=.data$xG.h, away_xG=.data$xG.a,
                  forecast_win=.data$forecast.w, forecast_draw=.data$forecast.d, forecast_loss=.data$forecast.l)

  match_results <- match_results %>%
    dplyr::mutate_at(c("home_goals", "away_goals", "home_xG", "away_xG", "forecast_win", "forecast_draw", "forecast_loss"), as.numeric)


  return(match_results)
}
