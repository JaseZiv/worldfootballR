#' Get player goal logs
#'
#' Returns the player's career goal and assist logs
#'
#' @param player_urls the URL(s) of the player(s)
#' @param time_pause the wait time (in seconds) between page loads
#' @param goals_or_assists select whether to return data of "goals" (the default), "assists", or "both"
#'
#' @return returns a dataframe of the player's goals and assists
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' # for single players:
#' jwp_url <- "https://fbref.com/en/players/3515d404/"
#' fb_player_goal_logs(player_urls = jwp_url, goals_or_assists = "goals")
#' })
#' }

fb_player_goal_logs <- function(player_urls, time_pause=3, goals_or_assists="goals") {

  t <- time_pause
  g_a <- goals_or_assists # one of "goals", "assists", or "both"

  get_each_player_goal_log <- function(player_url, time_pause=t, goals_or_assists=g_a) {

    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    page <- .load_page(player_url)
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

      # get goals, or an empty data frame
      tab_box_goals <-
        {
          if(goals_or_assists %in% c("goals","both"))
            tryCatch({goal_log_page %>% rvest::html_nodes(".stats_table") %>% .[1] %>% rvest::html_table()},
                     error=function(e) {tab_box_for <- NA})
          else data.frame()
        }

      # get assists, or an empty data frame
      tab_box_assists <-
        {
          if(goals_or_assists %in% c("assists","both"))
            tryCatch({goal_log_page %>% rvest::html_nodes(".stats_table") %>% .[2] %>% rvest::html_table()},
                     error=function(e) {tab_box_for <- NA})
          else data.frame()
        }

      # turn html tables into data frames, force minute to be a character, and label the goals and assists
      goals <-
        tab_box_goals %>%
        data.frame() %>%
        dplyr::mutate(dplyr::across(tidyselect::contains("Minute"), as.character)) %>%
        dplyr::mutate(Goal_or_Assist="goal")

      assists <-
        tab_box_assists %>%
        data.frame() %>%
        dplyr::mutate(dplyr::across(tidyselect::contains("Minute"), as.character)) %>%
        dplyr::mutate(Goal_or_Assist="assist")

      # bind the tables together
      goals_and_assists <-
        dplyr::bind_rows(goals,assists) %>%
        dplyr::rename(Body_Part=.data$Body.Part,GCA_Type1=.data$Type,GCA_Type2=.data$Type.1) %>%
        dplyr::filter(!is.na(.data$Rk)) %>%
        dplyr::arrange(dplyr::desc(.data$Goal_or_Assist),.data$Rk)

      return(goals_and_assists)
    }
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(player_urls))

  goals_map <- player_urls %>%
    purrr::map_df(get_each_player_goal_log)

  return(goals_map)
}
