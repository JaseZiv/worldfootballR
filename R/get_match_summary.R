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

    each_game_page <- tryCatch(xml2::read_html(match_url), error = function(e) NA)

    if(!is.na(each_game_page)) {
      match_report <- .get_match_report_page(match_page = each_game_page)
      Home_Team <- tryCatch(each_game_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[1], error = function(e) NA)
      Away_Team <- tryCatch(each_game_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[2], error = function(e) NA)

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

      events_df <- cbind(match_report, events_df)

    } else {
      print(glue::glue("Match Summary not available for {match_url}"))
      events_df <- data.frame()
    }

    return(events_df)
  }

  all_events_df <- match_url %>%
    purrr::map_df(get_each_match_summary)

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/league_seasons/all_tier1_season_URLs.csv")

  seasons <- seasons %>%
    dplyr::filter(.data$seasons_urls %in% all_events_df$League_URL) %>%
    dplyr::select(League=.data$competition_name, Gender=.data$gender, Country=.data$country, Season=.data$seasons)

  all_events_df <- cbind(seasons, all_events_df) %>%
    dplyr::select(-.data$League_URL)

  return(all_events_df)
}
