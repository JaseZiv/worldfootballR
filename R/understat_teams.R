
#' Get Understat team player stats
#'
#' Retrieve Understat team player stats.
#'
#' @inheritParams understat_team_season_shots
#'
#' @return a dataframe of player stats for a selected team season
#'
#' @export
#' @examples \dontrun{
#' understat_team_players_stats(team_url = c("https://understat.com/team/Liverpool/2020", "https://understat.com/team/Manchester_City/2020"))
#' }
understat_team_players_stats <- function(team_url) {
  f_possibly <- purrr::possibly(.understat_team_players_stats, otherwise = data.frame(), quiet = FALSE)
  purrr::map_dfr(
    team_url,
    f_possibly
  )
}

.understat_team_players_stats <- function(team_url) {
  print(glue::glue("Scraping player stats for team {team_url}. Please acknowledge understat.com as the data source"))

  players_data <- .get_clean_understat_json(team_url, "playersData")

  names(players_data)[names(players_data) == "team_title"] <- "team_name"
  names(players_data)[names(players_data) == "id"] <- "player_id"

  withr::local_options(list(readr.num_columns = 0))
  players_data <- readr::type_convert(players_data)

  return(players_data)
}
