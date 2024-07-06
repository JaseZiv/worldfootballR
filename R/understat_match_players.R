
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
  do.call("rbind", .) %>%
  select(match_id,  team_id,
         team_status = h_a,
         player_id, swap_id = id,
         player, position, positionOrder,
         time_played = time,
         everything()) %>%
  mutate(team_status = ifelse(team_status=="h","home","away"))


match_player_data_home <- do.call(rbind.data.frame, match_player_data$h)
match_player_data_away <- do.call(rbind.data.frame, match_player_data$a)

match_player_data <- bind_rows(match_player_data_home,match_player_data_away) %>%
  mutate(match_id = match_id)

  return(match_player_data)
}


