
#' Get Understat match player data
#'
#' Returns player values for a selected match from Understat.com.
#'
#' @param match_url A `character` string with the URL of the match played.
#'
#' @return returns a `data.frame` with data for all players for the match.
#'
#' @importFrom magrittr %>%
#' @importFrom rvest html_elements
#' @importFrom stringi stri_unescape_unicode
#' @importFrom stringr str_subset
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#'
#' @export

understat_match_players <- function(match_url) {

  match_id <- gsub("[^0-9]", "", match_url)

  match_player_data <- .get_understat_json(page_url = match_url)  %>%
    rvest::html_elements("script") %>%
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


  match_player_data_rebind <- dplyr::bind_rows(match_player_data_home, match_player_data_away)

  match_players <- data.frame(
    match_id = as.integer(match_id),
    id = as.integer(match_player_data_rebind[["id"]]),
    team_id = as.integer(match_player_data_rebind[["team_id"]]),
    home_away = as.character(match_player_data_rebind[["h_a"]]),
    player_id = as.integer(match_player_data_rebind[["player_id"]]),
    swap_id = as.integer(match_player_data_rebind[["id"]]),
    player = as.character(match_player_data_rebind[["player"]]),
    position = as.character(match_player_data_rebind[["position"]]),
    positionOrder = as.integer(match_player_data_rebind[["positionOrder"]]),
    time_played = as.integer(match_player_data_rebind[["time"]]),
    goals = as.integer(match_player_data_rebind[["goals"]]),
    own_goals = as.integer(match_player_data_rebind[["own_goals"]]),
    shots = as.integer(match_player_data_rebind[["shots"]]),
    xG = as.numeric(match_player_data_rebind[["xG"]]),
    yellow_card = as.integer(match_player_data_rebind[["yellow_card"]]),
    red_card = as.integer(match_player_data_rebind[["red_card"]]),
    roster_in = as.integer(match_player_data_rebind[["roster_in"]]),
    roster_out = as.integer(match_player_data_rebind[["roster_out"]]),
    key_passes = as.integer(match_player_data_rebind[["key_passes"]]),
    assists = as.integer(match_player_data_rebind[["assists"]]),
    xA = as.numeric(match_player_data_rebind[["xA"]]),
    xGChain = as.numeric(match_player_data_rebind[["xGChain"]]),
    xGBuildup = as.numeric(match_player_data_rebind[["xGBuildup"]])
  )

  return(match_players)
}


