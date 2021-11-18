#' Get Understat league season shot locations
#'
#' Returns shooting locations for all matches played in the selected league season from Understat.com
#'
#' @param league the avaliable leagues in Understat as outlined below
#' @param season_start_year the year the season started
#'
#' The leagues currently available for Understat are:
#' \emph{"EPL"}, \emph{"La liga}", \emph{"Bundesliga"},
#' \emph{"Serie A"}, \emph{"Ligue 1"}, \emph{"RFPL"}
#'
#' @return returns a dataframe of shooting locations for a selected league season
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \donttest{
#' understat_league_season_shots(league = "EPL", season_start_year = 2020)
#' }
understat_league_season_shots <- function(league, season_start_year) {
  # .pkg_message("Scraping shots data for {league} {season_start_year} season. Please acknowledge understat.com as the data source")
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

  shots_data <- .understat_shooting(type_url = league_url)

  shots_data <- cbind(league, shots_data)

  shots_data <- shots_data %>%
    dplyr::rename(home_team=.data$h_team, away_team=.data$a_team, home_goals=.data$h_goals, away_goals=.data$a_goals) %>%
    dplyr::mutate_at(c("X", "Y", "xG", "home_goals", "away_goals"), as.numeric)

  return(shots_data)
}



#' Get Understat team season shot locations
#'
#' Returns shooting locations for all matches played by a selected team from Understat.com
#'
#' @param team_url the URL of the team season
#'
#' @return returns a dataframe of shooting locations for a selected team season
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \donttest{
#' understat_team_season_shots(team_url = "https://understat.com/team/Manchester_City/2020")
#' }
understat_team_season_shots <- function(team_url) {
  # .pkg_message("Scraping all shots for team {team_url}. Please acknowledge understat.com as the data source")

  shots_df <- .understat_shooting(type_url = team_url)

  shots_df <- shots_df %>%
    dplyr::rename(home_away=.data$h_a, home_team=.data$h_team, away_team=.data$a_team, home_goals=.data$h_goals, away_goals=.data$a_goals) %>%
    dplyr::mutate_at(c("minute", "X", "Y", "xG", "home_goals", "away_goals"), as.numeric)

  return(shots_df)
}





#' Get Understat match shot locations
#'
#' Returns shooting locations for a selected match from Understat.com
#'
#' @param match_url the URL of the match played
#'
#' @return returns a dataframe of shooting locations for a selected team season
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \donttest{
#' understat_match_shots(match_url = "https://understat.com/match/14789")
#' }
understat_match_shots <- function(match_url) {
  # .pkg_message("Scraping all shots for match {match_url}. Please acknowledge understat.com as the data source")

  match_shots_df <- .get_clean_understat_json(page_url = match_url, script_name = "shotsData")

  match_shots_df <- match_shots_df %>%
    dplyr::rename(home_away=.data$h_a, home_team=.data$h_team, away_team=.data$a_team, home_goals=.data$h_goals, away_goals=.data$a_goals) %>%
    dplyr::mutate_at(c("minute", "X", "Y", "xG", "home_goals", "away_goals"), as.numeric)

  return(match_shots_df)
}


#' Get all Understat shot locations for a player
#'
#' Returns shooting locations for a selected player for all matches played from Understat.com
#'
#' @param player_url the URL of a selected player
#'
#' @return returns a dataframe of shooting locations for a selected player
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \donttest{
#' sterling <- understat_player_shots(player_url = "https://understat.com/player/618")
#' }
understat_player_shots <- function(player_url) {
  # .pkg_message("Scraping all shots for player {player_url}. Please acknowledge understat.com as the data source")

  shots_df <- understat_match_shots(match_url = player_url)
  return(shots_df)
}

