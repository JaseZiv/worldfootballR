#' Get team shot logs
#'
#' Returns the team's season shot logs
#'
#' @param team_urls the URL(s) of the teams(s) (can come from fb_teams_urls())
#' @param time_pause the wait time (in seconds) between page loads
#' @param team_opposition select whether to return data of goals scored, goals conceded, or both [not implemented yet]
#'
#' @return returns a dataframe of the team's goals scored and conceded in the season
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' # for single teams:
#' man_city_url <- "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats"
#' fb_team_goal_logs(team_urls = man_city_url)
#' })
#' }

fb_team_goal_logs <- function(team_urls, time_pause=3, team_opposition="both") {

  main_url <- "https://fbref.com"
  time_wait <- time_pause
  # not implemented yet
  table_type <- team_opposition # one of: team, opposition, both

  get_each_team_goal_log <- function(team_url, time_pause=time_wait, team_opposition=table_type) {

    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    page <- .load_page(team_url)
    team_name <- sub('.*\\/', '', team_url) %>% gsub("-Stats", "", .) %>% gsub("-", " ", .)
    league <- page %>% rvest::html_elements(".prevnext+ p a") %>% rvest::html_text()
    season <- page %>% rvest::html_nodes("h1") %>% rvest::html_text() %>% stringr::str_squish() %>% sub(" .*", "", .)
    page_urls <- page %>% rvest::html_elements("#inner_nav .hoversmooth a")

    # this is a hack for now - the goals log is the last of four "All Competitions" urls
    index <- grep("All Competitions", rvest::html_text(page_urls))[4]
    goal_log_url <- page_urls[index] %>% rvest::html_attr("href") %>% unique() %>% paste0(main_url, .)
    goal_log_page <- tryCatch(.load_page(goal_log_url), error = function(e) NA)

    if(!is.na(goal_log_page)) {

      tryCatch({tab_box_for <- goal_log_page %>% rvest::html_nodes(".stats_table") %>% .[1]}, error=function(e) {tab_box_for <- NA})
      tryCatch({tab_box_against <- goal_log_page %>% rvest::html_nodes(".stats_table") %>% .[2]}, error=function(e) {tab_box_against <- NA})

      goals_for <-
        tab_box_for %>%
        rvest::html_table() %>%
        data.frame() %>%
        dplyr::rename(Body_Part=Body.Part,GCA_Type1=Type,GCA_Type2=Type.1) %>%
        dplyr::mutate(Team_Opposition="team")

      goals_against <-
        tab_box_against %>%
        rvest::html_table() %>%
        data.frame() %>%
        dplyr::rename(Body_Part=Body.Part,GCA_Type1=Type,GCA_Type2=Type.1) %>%
        dplyr::mutate(Team_Opposition="opposition")

      goals <-
        rbind(goals_for,goals_against) %>%
        dplyr::arrange(Date,Minute)

      return(goals)
    }
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(team_urls))

  all_stats_df <- team_urls %>%
    purrr::map_df(get_each_team_goal_log)

  return(all_stats_df)
}
