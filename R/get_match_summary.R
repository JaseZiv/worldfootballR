#' Get match summary
#'
#' Returns match summary data for selected match URLs, including goals, subs and cards
#'
#' @param match_url the fbref.com URL for the required match
#'
#' @return returns a dataframe with the match events (goals, cards, subs) for selecetd matches
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' match <- "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League"
#' get_match_summary(match_url = match)
#' }
#'
get_match_summary <- function(match_url) {

  get_each_match_summary <- function(match_url) {
    match_report <- worldfootballR::get_match_report(match_url = match_url)
    each_game_page <- NA

    tryCatch( {each_game_page <- xml2::read_html(match_url)}, error = function(e) {each_game_page <- NA})

    if(!is.na(each_game_page)) {
      tryCatch( {Home_Team <- each_game_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[1]}, error = function(e) {Home_Team <- NA})
      tryCatch( {Away_Team <- each_game_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[2]}, error = function(e) {Away_Team <- NA})

      events <- each_game_page %>% rvest::html_nodes("#events_wrap")

      events_home <- events %>% rvest::html_nodes(".a") %>% rvest::html_text() %>% stringr::str_squish()
      events_away <- events %>% rvest::html_nodes(".b") %>% rvest::html_text() %>% stringr::str_squish()

      home_events <- data.frame(Team=Home_Team, Home_Away="Home", events_string=events_home)
      away_events <- data.frame(Team=Away_Team, Home_Away="Away", events_string=events_away)

      events_df <- dplyr::bind_rows(home_events, away_events)


      events_df <- events_df %>%
        dplyr::mutate(event_time = gsub("&rsquor.*", "", .data$events_string) %>% ifelse(stringr::str_detect(., "\\+"), (as.numeric(gsub("\\+.*", "", .)) + as.numeric(gsub(".*\\+", "", .))), .),
                      event_time = as.numeric(.data$event_time),
                      event_type = ifelse(stringr::str_detect(tolower(.data$events_string), "penalty"), "Penalty",
                                          ifelse(stringr::str_detect(tolower(.data$events_string), "own goal"), "Own Goal",
                                                 gsub(".* — ", "", .data$events_string) %>% gsub("[[:digit:]]:[[:digit:]]", "", .))) %>% stringr::str_squish(),
                      event_players = gsub(".*\\;", "", .data$events_string) %>% gsub(" —.*", "", .),
                      score_progression = stringr::str_extract(.data$event_players, "[[:digit:]]:[[:digit:]]"),
                      event_players = gsub("[[:digit:]]:[[:digit:]]", "", .data$event_players) %>% stringr::str_squish()) %>%
        dplyr::select(-.data$events_string) %>%
        dplyr::arrange(.data$event_time)
    } else {
      events_df <- NA
    }

    events_df <- cbind(match_report, events_df)

    return(events_df)
  }

  all_events_df <- match_url %>%
    purrr::map_df(get_each_match_summary)

  return(all_events_df)
}
