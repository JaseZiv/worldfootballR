#' Get FBref team match results
#'
#' Returns all game results for a team in a given season
#' Replaces the deprecated function get_team_match_results
#'
#' @param team_url the URL for the team season
#' @param time_pause the wait time (in seconds) between page loads
#'
#' @return returns a dataframe with the results of all games played by the selected team(s)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' # for single teams:
#' man_city_url <- "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats"
#' fb_team_match_results(man_city_url)
#' })
#' }

fb_team_match_results <- function(team_url, time_pause=3) {
  # .pkg_message("Scraping team match logs...")

  time_wait <- time_pause

  get_each_team_log <- function(team_url, time_pause=time_wait) {
    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    team_page <- .load_page(team_url)

    team_name <- sub('.*\\/', '', team_url) %>% gsub("-Stats", "", .) %>% gsub("-", " ", .)

    opponent_names <- team_page %>% rvest::html_nodes(".left:nth-child(10) a") %>% rvest::html_text()

    team_log <- team_page %>%
      rvest::html_nodes("#all_matchlogs") %>%
      rvest::html_nodes("table") %>%
      rvest::html_table() %>% data.frame()

    team_log$Opponent <- opponent_names

    team_log <- team_log %>%
      dplyr::mutate(Team_Url = team_url,
                    Team = team_name) %>%
      dplyr::select(.data$Team_Url, .data$Team, dplyr::everything(), -.data$Match.Report)

    team_log <- team_log %>%
      dplyr::mutate(Attendance = gsub(",", "", .data$Attendance) %>% as.numeric(),
                    GF = as.character(.data$GF),
                    GA = as.character(.data$GA))

    return(team_log)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(team_url))

  all_team_logs <- team_url %>%
    purrr::map_df(get_each_team_log)
}




#' Get team match results
#'
#' Returns all game results for a team in a given season
#'
#' @param team_url the URL for the team season
#' @param time_pause the wait time (in seconds) between page loads
#'
#' @return returns a dataframe with the results of all games played by the selected team(s)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' # for single teams:
#' man_city_url <- "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats"
#' get_team_match_results(man_city_url)
#' })
#' }

get_team_match_results <- function(team_url, time_pause=3) {
  # .pkg_message("Scraping team match logs...")

  .Deprecated("fb_team_match_results")

  time_wait <- time_pause

  get_each_team_log <- function(team_url, time_pause=time_wait) {
    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    team_page <- xml2::read_html(team_url)

    team_name <- sub('.*\\/', '', team_url) %>% gsub("-Stats", "", .) %>% gsub("-", " ", .)

    opponent_names <- team_page %>% rvest::html_nodes(".left:nth-child(10) a") %>% rvest::html_text()

    team_log <- team_page %>%
      rvest::html_nodes("#all_matchlogs") %>%
      rvest::html_nodes("table") %>%
      rvest::html_table() %>% data.frame()

    team_log$Opponent <- opponent_names

    team_log <- team_log %>%
      dplyr::mutate(Team_Url = team_url,
                    Team = team_name) %>%
      dplyr::select(.data$Team_Url, .data$Team, dplyr::everything(), -.data$Match.Report)

    team_log <- team_log %>%
      dplyr::mutate(Attendance = gsub(",", "", .data$Attendance) %>% as.numeric(),
             GF = as.character(.data$GF),
             GA = as.character(.data$GA))

    return(team_log)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(team_url))

  all_team_logs <- team_url %>%
    purrr::map_df(get_each_team_log)
}

