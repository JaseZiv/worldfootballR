#' Get team match log stats
#'
#' Returns all match statistics for a team(s) in a given season
#'
#' @param team_urls the URL(s) of the teams(s) (can come from fb_teams_urls())
#' @param stat_type the type of statistic required
#' @param time_pause the wait time (in seconds) between page loads
#'
#' The statistic type options (stat_type) include:
#'
#' \emph{"shooting"}, \emph{"keeper"}, \emph{"passing"},
#' \emph{"passing_types"}, \emph{"gca"}, \emph{"defense"},
#' \emph{"misc"}
#'
#' @return returns a dataframe with the selected stat outputs of all games played by the selected team(s)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # for single teams:
#' man_city_url <- "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats"
#' fb_team_match_log_stats(team_urls = man_city_url, stat_tyoe = "passing")
#' }

fb_team_match_log_stats <- function(team_urls, stat_type, time_pause=2) {
  # .pkg_message("Scraping team match logs...")
  main_url <- "https://fbref.com"

  stat_types <- c("shooting", "keeper", "passing", "passing_types", "gca", "defense", "possession", "misc")
  if(!stat_type %in% stat_types) stop("check stat type")

  time_wait <- time_pause

  get_each_team_log <- function(team_url, time_pause=time_wait) {
    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    team_page <- xml2::read_html(team_url)

    team_name <- sub('.*\\/', '', team_url) %>% gsub("-Stats", "", .) %>% gsub("-", " ", .)


    stat_urls <- team_page %>% rvest::html_nodes("#content .filter")
    stat_urls <- stat_urls[grep("Match Log Types", rvest::html_text(stat_urls))]
    stat_urls <- stat_urls %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% .[!is.na(.)]

    selected_url <- stat_urls[grep(paste0(stat_type, "/"), stat_urls)]
    if(length(selected_url) == 0) print(glue::glue("{stat_type} stat not available for {team_name}"))

    selected_url <- paste0(main_url, selected_url)

    stat_page <- xml2::read_html(selected_url)

    for_against <- stat_page %>%
      rvest::html_nodes("#all_matchlogs #switcher_matchlogs .table_container")

    df_for <- tryCatch(for_against[1] %>% rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame(), error = function(e) data.frame())

    if(nrow(df_for) > 0) {

      colnames(df_for)[grep("For.", colnames(df_for))] <- ""
      df_for <- .clean_table_names(df_for)
      df_for <- df_for %>%
        dplyr::filter(.data$Date != "")

      df_for$ForAgainst <- "For"

      # because the opponent names also contain some country abbreviations in the table output, will get the opponent names separately
      # to do this, the most reliable way is to find the index of the `Opponent` column and then grab that - ensures if some
      # leagues/teams have differnt col numbers, this will be dynamic
      tab_names <- for_against[1] %>% rvest::html_nodes("thead tr") %>% .[2] %>% rvest::html_nodes("th") %>% rvest::html_text()
      opp_idx <- grep("opponent", tolower(tab_names))
      opponent_names <- for_against[1] %>% rvest::html_nodes(paste0(".left:nth-child(", opp_idx, ") a")) %>% rvest::html_text()
      df_for$Opponent <- opponent_names
    }

    # now do for against:
    df_against <- tryCatch(for_against[2] %>% rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame(), error = function(e) data.frame())

    if(nrow(df_against) > 0) {
      colnames(df_against)[grep("Against.", colnames(df_against))] <- ""
      df_against <- .clean_table_names(df_against)
      df_against <- df_against %>%
        dplyr::filter(.data$Date != "")

      df_against$ForAgainst <- "Against"
      df_against$Opponent <- opponent_names
    }

    team_log <- tryCatch(dplyr::bind_rows(df_for, df_against), error = function(e) data.frame())

    if(nrow(team_log) > 0) {
      team_log <- team_log %>%
        dplyr::mutate(Team_Url = team_url,
                      Team = team_name) %>%
        dplyr::select(.data$Team_Url, .data$Team, .data$ForAgainst, dplyr::everything(), -.data$`Match Report`)


      cols_to_transform <- team_log %>%
        dplyr::select(-.data$Team_Url, -.data$Team, -.data$ForAgainst, -.data$Date, -.data$Time, -.data$Comp, -.data$Round, -.data$Day,
                      -.data$Venue, -.data$Result, -.data$GF, -.data$GA, -.data$Opponent) %>% names()

      team_log <- team_log %>%
        dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub(",", "", x)}) %>%
        dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub("+", "", x)}) %>%
        dplyr::mutate_at(.vars = cols_to_transform, .funs = as.numeric)
    }

    return(team_log)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(team_urls))

  all_team_logs <- team_urls %>%
    purrr::map_df(get_each_team_log)

}
