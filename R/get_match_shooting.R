#' Get match shooting event data
#'
#' Returns detailed player shooting data for home and away teams for a selected match(es)
#'
#' @param match_url the fbref.com URL for the required match
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
#' get_match_shooting(match_url = match)
#' }
#'

get_match_shooting <- function(match_url) {
  print("Scraping detailed shot and shot creation data...")

  get_each_match_shooting_data <- function(match_url) {
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

      home_shot_df <- all_shots[2] %>% rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame()
      home_shot_df$team_name <- home_team
      home_shot_df$Home_Away <- "Home"
      away_shot_df <- all_shots[3] %>% rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame()
      away_shot_df$team_name <- away_team
      away_shot_df$Home_Away <- "Away"

      prep_shot_df <- function(shot_df) {
        shot_df <- home_shot_df
        names(shot_df) <- c("Minute", "Shooting_Player", "Squad", "Outcome", "Distance", "Body_Part", "Shot_Notes", "SCA1_Player", "SCA1_Event", "SCA2_Player", "SCA2_Event", "team_name", "Home_Away")
        shot_df <- shot_df[-1, ]

        mins_idx <- which(shot_df$Minute == "")
        if(length(mins_idx)==0) {
          half <- "1st"
        } else {
          mins_max_idx <- nrow(shot_df) - (mins_idx)

          first_half <- rep("1st", times=mins_idx-1)
          second_half <- rep("2nd", times=mins_max_idx)

          shot_df <- shot_df %>%
            dplyr::filter(.data$Minute != "")

          half <- c(first_half, second_half)
        }


        shot_df <- shot_df %>%
          dplyr::mutate(Match_Half = half)

        shot_df <- dplyr::bind_cols(Date=match_date, shot_df)

        return(shot_df)
      }

      all_shot_df <- prep_shot_df(home_shot_df) %>%
        rbind(prep_shot_df(away_shot_df))


      all_shot_df <- all_shot_df %>%
        dplyr::select(.data$Date, Team=.data$team_name, .data$Home_Away, .data$Match_Half, dplyr::everything())
    } else {
      print(glue::glue("Detailed shot data unavailable for {match_url}"))
      all_shot_df <- data.frame()
    }



    return(all_shot_df)
  }

  all_shooting <- match_url %>%
    purrr::map_df(get_each_match_shooting_data)

  return(all_shooting)
}

