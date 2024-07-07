
#' Get Understat match player data
#'
#' Returns player values for a selected match from Understat.com
#'
#' @param match_url the URL of the match played
#'
#' @return returns a dataframe with data for all players for the match
#'
#' @importFrom magrittr %>%
#'
#' @export

understat_match_players <- function(match_url) {
  # .pkg_message("Scraping all shots for match {match_url}. Please acknowledge understat.com as the data source")

  match_id <- gsub("[^0-9]", "", match_url)

  match_player_data <- .get_understat_json(page_url = match_url)  %>%
    rvest::html_nodes("script") %>%
    as.character()

  match_player_data <- match_player_data[grep("rostersData\t=", match_player_data)] %>%
    stringi::stri_unescape_unicode() %>%

    substr(41,nchar(.)) %>%
    substr(0,nchar(.)-13) %>%
    paste0('[', . , ']') %>%

    unlist() %>%
    stringr::str_subset("\\[\\]", negate = TRUE)

  match_player_data <- lapply(match_player_data, jsonlite::fromJSON) %>%
    do.call("rbind", .)

  match_player_data_home <- do.call(rbind.data.frame, match_player_data$h)
  match_player_data_away <- do.call(rbind.data.frame, match_player_data$a)

  match_player_data <- dplyr::bind_rows(match_player_data_home,match_player_data_away) %>%

    dplyr::mutate(match_id = as.integer(match_id),
                  team_id = as.integer(team_id),
                  team_status = as.character(team_status),
                  player_id = as.integer(player_id),
                  swap_id = as.integer(id),
                  player = as.character(player),
                  position = as.character(position),
                  positionOrder = as.integer(positionOrder),
                  time_played =  as.integer(time),
                  goals = as.integer(goals),
                  own_goals = as.integer(own_goals),
                  shots = as.integer(shots),
                  xG = as.numeric(xG),
                  yellow_card = as.integer(yellow_card),
                  red_card = as.integer(red_card),
                  roster_in = as.integer(roster_in),
                  roster_out = as.integer(roster_out),
                  key_passes = as.integer(key_passes),
                  assists = as.integer(assists),
                  xA = as.numeric(xA),
                  xGChain = as.numeric(xGChain),
                  XGBuildup = as.numeric(XGBuildup))  %>%

    dplyr::select(-c(time,id)) %>%

    dplyr::mutate(team_status = ifelse(team_status=="h","home","away"))

  return(match_player_data)
}


