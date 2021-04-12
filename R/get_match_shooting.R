#' Get match shooting data
#'
#' Returns detailed player shooting data for home and away teams for a selected match
#'
#' @param match_url the fbref.com URL for the required match
#'
#' @return returns a dataframe with detailed player shooting data for a selected match
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

get_match_shooting <- function(match_url) {

  get_each_match_shooting_data <- function(match_url) {
    match_page <- xml2::read_html(match_url)

    match_date <- match_page %>% rvest::html_nodes(".venuetime") %>% rvest::html_attr("data-venue-date")

    all_shots <- match_page %>% rvest::html_nodes("#shots_all")


    shot_df <- all_shots %>% rvest::html_table() %>% data.frame()

    names(shot_df) <- c("Minute", "Shooting_Player", "Team", "Outcome", "Distance", "Body_Part", "Shot_Notes", "SCA1_Player", "SCA1_Event", "SCA2_Player", "SCA2_Event")
    shot_df <- shot_df[-1, ]

    mins_idx <- which(shot_df$Minute == "")
    mins_max_idx <- nrow(shot_df) - (mins_idx)

    first_half <- rep("1st", times=mins_idx-1)
    second_half <- rep("2nd", times=mins_max_idx)

    shot_df <- shot_df %>%
      dplyr::filter(.data$Minute != "")

    half <- c(first_half, second_half)

    shot_df <- shot_df %>%
      dplyr::mutate(Match_Half = half)

    shot_df <- dplyr::bind_cols(Matchday=match_date, shot_df)


    tryCatch( {home_team <- match_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[1]}, error = function(e) {home_team <- NA})
    tryCatch( {away_team <- match_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[2]}, error = function(e) {away_team <- NA})

    home_away_df <- data.frame(Team=home_team, Home_Away="Home") %>%
      rbind(data.frame(Team=away_team, Home_Away = "Away"))


    shot_df <- home_away_df %>%
      dplyr::left_join(shot_df, by = "Team") %>%
      dplyr::select(.data$Matchday, .data$Team, .data$Home_Away, dplyr::everything())

    return(shot_df)
  }

  all_shooting <- match_url %>%
    purrr::map_df(get_each_match_shooting_data)

  return(all_shooting)
}

urls <- c("https://fbref.com/en/matches/bf52349b/Fulham-Arsenal-September-12-2020-Premier-League",
          "https://fbref.com/en/matches/a3eb7a37/Sheffield-United-Wolverhampton-Wanderers-September-14-2020-Premier-League")

test <- get_match_shooting(urls)


