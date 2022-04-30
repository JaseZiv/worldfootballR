#' Get match shooting event data
#'
#' Returns detailed player shooting data for home and away teams for a selected match(es)
#'
#' @param match_url the fbref.com URL for the required match
#' @param time_pause the wait time (in seconds) between page loads
#'
#' @return returns a dataframe
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' match <- "https://fbref.com/en/matches/bf52349b/Fulham-Arsenal-September-12-2020-Premier-League"
#' df <- get_match_shooting(match_url = match)
#' }
#'

get_match_shooting <- function(match_url, time_pause=2) {
  # .pkg_message("Scraping detailed shot and shot creation data...")

  time_wait <- time_pause

  get_each_match_shooting_data <- function(match_url, time_pause=time_wait) {

    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    match_page <- xml2::read_html(match_url)

    tryCatch( {home_team <- match_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[1]}, error = function(e) {home_team <- NA})
    tryCatch( {away_team <- match_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[2]}, error = function(e) {away_team <- NA})

    # home_away_df <- data.frame(Team=home_team, Home_Away="Home") %>%
    #   rbind(data.frame(Team=away_team, Home_Away = "Away"))

    match_date <- match_page %>% rvest::html_nodes(".venuetime") %>% rvest::html_attr("data-venue-date")

    all_shots <- match_page %>% rvest::html_nodes("#switcher_shots") %>% rvest::html_nodes("div")

    if(length(all_shots) > 0) {
      # all_shots <- match_page %>% rvest::html_nodes("#shots_all")
      # shot_df <- all_shots %>% rvest::html_table() %>% data.frame()

      # function to clean home and away df
      prep_shot_df <- function(shot_df) {
        names(shot_df) <- c("Minute", "Shooting_Player", "Squad", "Outcome", "Distance", "Body_Part", "Shot_Notes", "SCA1_Player", "SCA1_Event", "SCA2_Player", "SCA2_Event")
        shot_df <- shot_df[-1, ]

        shot_df <- shot_df %>%
          dplyr::mutate(Match_Half = dplyr::case_when(
            as.numeric(gsub("\\+.*", "", .data$Minute)) <= 45 ~ 1,
            dplyr::between(as.numeric(gsub("\\+.*", "", .data$Minute)), 46, 90) ~ 2,
            dplyr::between(as.numeric(gsub("\\+.*", "", .data$Minute)), 91, 105) ~ 3,
            dplyr::between(as.numeric(gsub("\\+.*", "", .data$Minute)), 106, 120) ~ 4,
            TRUE ~ 5))

        shot_df <- shot_df %>%
          dplyr::filter(.data$Minute != "")

        shot_df <- dplyr::bind_cols(Date=match_date, shot_df)

        return(shot_df)
      }

      home_shot_df <- tryCatch(all_shots[2] %>% rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame(), error = function(e) data.frame())
      if(nrow(home_shot_df > 0)) {
        home_shot_df <- prep_shot_df(home_shot_df)
      }

      away_shot_df <- tryCatch(all_shots[3] %>% rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame(), error = function(e) data.frame())
      if(nrow(away_shot_df > 0)) {
        away_shot_df <- prep_shot_df(away_shot_df)
      }

      all_shot_df <- home_shot_df %>%
        rbind(away_shot_df)

      all_shot_df <- all_shot_df %>%
        dplyr::mutate(Home_Away = ifelse(.data$Squad == home_team, "Home", "Away")) %>%
        dplyr::select(.data$Date, .data$Squad, .data$Home_Away, .data$Match_Half, dplyr::everything())
    } else {
      print(glue::glue("Detailed shot data unavailable for {match_url}"))
      all_shot_df <- data.frame()
    }



    return(all_shot_df)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(match_url))

  all_shooting <- match_url %>%
    purrr::map_df(get_each_match_shooting_data)

  return(all_shooting)
}

