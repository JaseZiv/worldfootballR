#' Get match summary
#'
#' Returns match summary data for selected match URLs, including goals, subs and cards
#'
#' @param match_url the fbref.com URL for the required match
#' @param time_pause the wait time (in seconds) between page loads
#'
#' @return returns a dataframe with the match events (goals, cards, subs) for selected matches
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' match <- get_match_urls(country = "AUS", gender = "F", season_end_year = 2021, tier = "1st")[1]
#' df <- get_match_summary(match_url = match)
#' }
#'
get_match_summary <- function(match_url, time_pause=2) {

  time_wait <- time_pause

  get_each_match_summary <- function(match_url, time_pause=time_wait) {
    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    each_game_page <- tryCatch(xml2::read_html(match_url), error = function(e) NA)

    if(!is.na(each_game_page)) {
      match_report <- .get_match_report_page(match_page = each_game_page)
      Home_Team <- tryCatch(each_game_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[1], error = function(e) NA)
      Away_Team <- tryCatch(each_game_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[2], error = function(e) NA)

      events <- each_game_page %>% rvest::html_nodes("#events_wrap")

      events_home <- events %>% rvest::html_nodes(".a") %>% rvest::html_text() %>% stringr::str_squish()
      events_away <- events %>% rvest::html_nodes(".b") %>% rvest::html_text() %>% stringr::str_squish()

      home_events <- tryCatch(data.frame(Team=Home_Team, Home_Away="Home", events_string=events_home), error = function(e) data.frame())
      away_events <- tryCatch(data.frame(Team=Away_Team, Home_Away="Away", events_string=events_away), error = function(e) data.frame())

      events_df <- dplyr::bind_rows(home_events, away_events)

      if(nrow(events_df) > 0) {
        events_df <- events_df %>%
          dplyr::mutate(Event_Time = gsub("&rsquor.*", "", .data$events_string)) %>%
          dplyr::mutate(Is_Pens = stringr::str_detect(.data$Event_Time, "[A-Z]"))

        suppressWarnings(
          events_df <- events_df %>%
            dplyr::mutate(Event_Half = dplyr::case_when(
              !.data$Is_Pens & as.numeric(gsub("\\+.*", "", .data$Event_Time)) <= 45 ~ 1,
              !.data$Is_Pens & dplyr::between(as.numeric(gsub("\\+.*", "", .data$Event_Time)), 46, 90) ~ 2,
              !.data$Is_Pens & dplyr::between(as.numeric(gsub("\\+.*", "", .data$Event_Time)), 91, 105) ~ 3,
              !.data$Is_Pens & dplyr::between(as.numeric(gsub("\\+.*", "", .data$Event_Time)), 106, 120) ~ 4,
              TRUE ~ 5
            ))
        )

        events_df <- events_df %>%
          dplyr::mutate(Event_Time = gsub("&rsquor.*", "", .data$events_string) %>% ifelse(stringr::str_detect(., "\\+"), (as.numeric(gsub("\\+.*", "", .)) + as.numeric(gsub(".*\\+", "", .))), .),
                        Event_Time = ifelse(.data$Is_Pens, 121, .data$Event_Time),
                        Event_Time = as.numeric(.data$Event_Time),
                        Event_Type = ifelse(stringr::str_detect(tolower(.data$events_string), "penalty"), "Penalty",
                                            ifelse(stringr::str_detect(tolower(.data$events_string), "own goal"), "Own Goal",
                                                   gsub(".* [^\x20-\x7E] ", "", .data$events_string) %>% gsub("[[:digit:]]:[[:digit:]]", "", .))) %>% stringr::str_squish(),
                        Event_Players = gsub(".*\\;", "", .data$events_string) %>% gsub(" [^\x20-\x7E] .*", "", .),
                        Score_Progression = stringr::str_extract(.data$Event_Players, "[[:digit:]]:[[:digit:]]"),
                        Event_Players = gsub("[[:digit:]]:[[:digit:]]", "", .data$Event_Players) %>% stringr::str_squish(),
                        Penalty_Number = dplyr::case_when(
                          .data$Is_Pens ~ gsub("([0-9]+).*$", "\\1", .data$Event_Players),
                          TRUE ~ NA_character_
                        ),
                        Penalty_Number = as.numeric(.data$Penalty_Number),
                        Event_Players = gsub("[[:digit:]]+\\s", "", .data$Event_Players),
                        Event_Type = ifelse(.data$Is_Pens, "Penalty Shootout", .data$Event_Type)) %>%
          dplyr::select(-.data$events_string) %>%
          dplyr::arrange(.data$Event_Half, .data$Event_Time)


        events_df <- cbind(match_report, events_df)
      } else {
        events_df <- data.frame()
      }

    } else {
      print(glue::glue("Match Summary not available for {match_url}"))
      events_df <- data.frame()
    }

    return(events_df)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(match_url))

  all_events_df <- match_url %>%
    purrr::map_df(get_each_match_summary)

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  seasons <- seasons %>%
    dplyr::filter(.data$seasons_urls %in% all_events_df$League_URL) %>%
    dplyr::select(League=.data$competition_name, Gender=.data$gender, Country=.data$country, Season=.data$seasons, League_URL=.data$seasons_urls)


  all_events_df <- seasons %>%
    dplyr::left_join(all_events_df, by = "League_URL") %>%
    dplyr::select(-.data$League_URL) %>% dplyr::distinct(.keep_all = T)

  return(all_events_df)
}
