LEAGUES <- list(
  "EPL" = "EPL",
  "La liga" = "La_liga",
  "Bundesliga" = "Bundesliga",
  "Serie A" = "Serie_A",
  "Ligue 1" = "Ligue_1",
  "RFPL" = "RFPL"
)
#' Load Understat league shot locations
#'
#' Loading version of \code{understat_league_season_shots}, but for all seasons
#' Returns shooting locations for all matches played in the selected league
#'
#' @param league the available leagues in Understat as outlined below
#'
#' The leagues currently available for Understat are:
#' \emph{"EPL"}, \emph{"La liga}", \emph{"Bundesliga"},
#' \emph{"Serie A"}, \emph{"Ligue 1"}, \emph{"RFPL"}
#'
#' @return returns a dataframe of shooting locations for a selected league
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' try({
#'   df <- load_understat_league_shots(league = "Serie A")
#' })
#' }
#' @export
load_understat_league_shots <- function(league) {
  check_league_name(league)
  league <- LEAGUES[[league]]


  league_name_clean <- janitor::make_clean_names(league)
  # then read in data

  dat_urls <- paste0("https://github.com/JaseZiv/worldfootballR_data/releases/download/understat_shots/", league_name_clean, "_shot_data.rds?raw=true")

  dat_df <- .file_reader(dat_urls)


  if (nrow(dat_df) == 0) {
    cli::cli_alert("Data not loaded. Please check parameters.")
  } else {
    cli::cli_alert("Data last updated {attr(dat_df, 'scrape_timestamp')} UTC")
  }

  return(dat_df)
}

check_league_name <- function(league) {
  leagues <- names(LEAGUES)
  rlang::arg_match0(league, leagues)
}
