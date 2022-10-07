#' Get team goal logs
#'
#' Returns the team's season goal logs
#'
#' @param team_urls the URL(s) of the team(s) (can come from fb_teams_urls())
#' @param time_pause the wait time (in seconds) between page loads
#' @param for_or_against select whether to return data of goals "for" (the default), goals "against", or "both"
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
#' fb_team_goal_logs(team_urls = man_city_url, for_or_against = "for")
#' })
#' }

fb_team_goal_logs <- function(team_urls, time_pause=3, for_or_against="for") {

  t <- time_pause
  f_a <- for_or_against # one of "team", "opposition", or "both"

  get_each_team_goal_log <- function(team_url, time_pause=t, for_or_against=f_a) {

    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    page <- .load_page(team_url)
    page_urls <- page %>% rvest::html_elements("#inner_nav .hoversmooth a")

    # if there is only one competition the "Goal Logs" text is a url
    # if there is more than one competitions it is a drop down menu
    # access from the "All Competitions" url instead
    # (may be better ways to get the All Competitions url)

    index <- ifelse(
      !is.na(grep("Goal Logs", rvest::html_text(page_urls))[1]),
      grep("Goal Logs", rvest::html_text(page_urls))[1],
      grep("All Competitions", rvest::html_text(page_urls))[4]
    )

    goal_log_url <- page_urls[index] %>% rvest::html_attr("href") %>% unique() %>% paste0("https://fbref.com", .)
    goal_log_page <- tryCatch(.load_page(goal_log_url), error = function(e) NA)

    if(!is.na(goal_log_page)) {

      # get goals for, or an empty data frame
      tab_box_goals_for <-
        {
          if(for_or_against %in% c("for","both"))
            tryCatch({goal_log_page %>% rvest::html_nodes(".stats_table") %>% .[1] %>% rvest::html_table()},
                     error=function(e) {tab_box_for <- NA})
          else data.frame()
        }

      # get goals against, or an empty data frame
      tab_box_goals_against <-
        {
          if(for_or_against %in% c("against","both"))
            tryCatch({goal_log_page %>% rvest::html_nodes(".stats_table") %>% .[2] %>% rvest::html_table()},
                     error=function(e) {tab_box_for <- NA})
          else data.frame()
        }

      # turn html tables into data frames, force minute to be a character, and label the goals
      goals_for <-
        tab_box_goals_for %>%
        data.frame() %>%
        mutate(across(contains("Minute"), as.character)) %>%
        dplyr::mutate(For_or_Against="for")

      goals_against <-
        tab_box_goals_against %>%
        data.frame() %>%
        mutate(across(contains("Minute"), as.character)) %>%
        dplyr::mutate(For_or_Against="against")

      # bind the tables together
      goals <-
        dplyr::bind_rows(goals_for,goals_against) %>%
        dplyr::rename(Body_Part=Body.Part,GCA_Type1=Type,GCA_Type2=Type.1) %>%
        filter(!is.na(Rk)) %>%
        dplyr::arrange(desc(For_or_Against),Date,Minute)

      return(goals)
    }
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(team_urls))

  goals_map <- team_urls %>%
    purrr::map_df(get_each_team_goal_log)

  return(goals_map)
}
