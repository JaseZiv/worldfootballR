
#' Get Understat match stats table data
#'
#' Returns the Stats values for a selected match from Understat.com
#'
#' @param match_url the URL of the match played
#'
#' @return returns a dataframe with data from the stats table for the match
#'
#' @importFrom magrittr %>%
#'
#' @export

understat_match_stats <- function(match_url) {
  # .pkg_message("Scraping all shots for match {match_url}. Please acknowledge understat.com as the data source")

  match_stats <- .get_understat_json(page_url = match_url) %>%
    rvest::html_nodes("div.scheme-block.is-hide[data-scheme='stats']") %>%
    rvest::html_nodes(".progress-value") %>%
    rvest::html_text()

  away <- match_stats[seq(1, length(match_stats), by=2)]
  home <- match_stats[seq(2, length(match_stats), by=2)]

  match_stats <- data.frame(

    match_id = gsub("[^0-9]", "", match_url),

    home_team = away[1],
    home_chances = away[2],
    home_goals = home[3],
    home_xG = home[4],
    home_shots = home[5],
    home_shot_on_target = home[6],
    home_deep = home[7],
    home_PPDA = home[8],
    home_xPTS = home[9],

    draw_chances = home[2],

    away_team = home[1],
    away_chances = away[3],
    away_goals = away[4],
    away_xG = away[5],
    away_shots = away[6],
    away_shot_on_target = away[7],
    away_deep = away[8],
    away_PPDA = away[9],
    away_xPTS = away[10]

  )

  return(match_stats)
}


