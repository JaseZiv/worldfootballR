
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

    match_id = as.integer(gsub("[^0-9]", "", match_url)),

    home_team = as.character(away[1]),
    home_chances = as.integer(gsub("[^0-9]", "", away[2]))/100,
    home_goals = as.integer(home[3]),
    home_xG = as.numeric(home[4]),
    home_shots = as.integer(home[5]),
    home_shot_on_target = as.integer(home[6]),
    home_deep = as.integer(home[7]),
    home_PPDA = as.numeric(home[8]),
    home_xPTS = as.numeric(home[9]),

    draw_chances = as.integer(gsub("[^0-9]", "", home[2]))/100,

    away_team = home[1],
    away_chances = as.integer(gsub("[^0-9]", "", away[3]))/100 ,
    away_goals = as.integer(away[4]),
    away_xG = as.numeric(away[5]),
    away_shots = as.integer(away[6]),
    away_shot_on_target = as.integer(away[7]),
    away_deep = as.integer(away[8]),
    away_PPDA = as.numeric(away[9]),
    away_xPTS = as.numeric(away[10])

  )

  return(match_stats)
}


