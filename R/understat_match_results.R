#' Get Understat season match results
#'
#' Returns match results for all matches played in the selected league season from Understat.com
#'
#' @param league the avaliable leagues in Understat as outlined below
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
#' \dontrun{
#' understat_league_match_results(league = "EPL", season_start_year = 2020)
#' }
understat_league_match_results <- function(league, season_start_year) {
  print(glue::glue("Scraping match results data for {league} {season_start_year} season. Please acknowledge understat.com as the data source"))
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

  shots_data <- .get_clean_understat_json(page_url = league_url, script_name = "datesData") %>%
    dplyr::filter(.data$isResult)

  shots_data <- cbind(league, shots_data)

  return(shots_data)
}
