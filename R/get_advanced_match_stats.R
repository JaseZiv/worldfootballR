#' @importFrom xml2 xml_find_all xml_attr xml_text
#' @importFrom dplyr mutate
#' @importFrom stats setNames
.add_player_href <- function(df, parent_element, player_xpath) {
  player_elements <- xml2::xml_find_all(parent_element, player_xpath)
  player_hrefs <- xml2::xml_attr(player_elements, "href")
  ## we need to apd for fb_advanced_stats, where there is a total row at the bottom
  n_diff <- nrow(df) - length(player_hrefs)
  res <- dplyr::mutate(
    df,
    "Player_Href" = c(player_hrefs, rep(NA_character_, n_diff)),
    .after = "Player"
  )
  return(res)
}

#' @importFrom rvest html_nodes html_text html_table
#' @importFrom purrr pluck
.extract_team_players <- function(match_page, xml_elements, team_idx, home_away) {

  team <- match_page %>%
    rvest::html_nodes("div+ strong a") %>%
    rvest::html_text() %>%
    purrr::pluck(team_idx)

  team_stat <- xml_elements[team_idx] %>%
    rvest::html_table() %>%
    data.frame() %>%
    .clean_match_advanced_stats_data()

  team_stat <- .add_player_href(
    team_stat,
    parent_element = xml_elements[team_idx],
    player_xpath = ".//tbody/tr/th/a"
  )
  res <- cbind(list("Team" = team, "Home_Away" = home_away), team_stat)
  return(res)
}

#' Get FBref advanced match stats
#'
#' Returns data frame of selected statistics for each match, for either whole team or individual players.
#' Multiple URLs can be passed to the function, but only one `stat_type` can be selected.
#' Replaces the deprecated function get_advanced_match_stats()
#'
#' @param match_url the three character country code for all countries
#' @param stat_type the type of team statistics the user requires
#' @param team_or_player result either summarised for each team, or individual players
#' @param time_pause the wait time (in seconds) between page loads
#'
#' The statistic type options (stat_type) include:
#'
#' \emph{"summary"}, \emph{"passing"}, \emph{"passing"_types},
#' \emph{"defense" }, \emph{"possession"}, \emph{"misc"}, \emph{"keeper"}
#'
#' @return returns a dataframe of a selected team statistic type for a selected match(es)
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
#' urls <- fb_match_urls(country = "AUS", gender = "F", season_end_year = 2021, tier = "1st")
#'
#' df <- fb_advanced_match_stats(match_url=urls,stat_type="possession",team_or_player="player")
#' })
#' }
fb_advanced_match_stats <- function(match_url, stat_type, team_or_player, time_pause=3) {
  main_url <- "https://fbref.com"

  time_wait <- time_pause

  get_each_match_statistic <- function(match_url, time_pause=time_wait) {
    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    match_page <- tryCatch(.load_page(match_url), error = function(e) NA)

    if(!is.na(match_page)) {
      match_report <- .get_match_report_page(match_page = match_page)

      league <- match_page %>%
        rvest::html_nodes("#content") %>%
        rvest::html_node("a") %>% rvest::html_text()

      all_tables <- match_page %>%
        rvest::html_nodes(".table_container")


      if(stat_type == "summary") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "summary$"))] %>%
          rvest::html_nodes("table")

      } else if(stat_type == "passing") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "passing$"))] %>%
          rvest::html_nodes("table")

      } else if(stat_type == "passing_types") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "passing_types"))] %>%
          rvest::html_nodes("table")

      } else if(stat_type == "defense") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "defense$"))] %>%
          rvest::html_nodes("table")

      } else if(stat_type == "possession") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "possession$"))] %>%
          rvest::html_nodes("table")


      } else if(stat_type == "misc") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "misc$"))] %>%
          rvest::html_nodes("table")

      } else if(stat_type == "keeper") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "keeper_stats"))] %>%
          rvest::html_nodes("table")

      }

      if(length(stat_df) != 0) {

        if(!stat_type %in% c("shots")) {
          home_stat <- .extract_team_players(
            match_page = match_page,
            xml_elements = stat_df,
            team_idx = 1,
            home_away = "Home"
          )
          away_stat <- .extract_team_players(
            match_page = match_page,
            xml_elements = stat_df,
            team_idx = 2,
            home_away = "Away"
          )
          stat_df_output <- dplyr::bind_rows(home_stat, away_stat)

          if(any(grepl("Nation", colnames(stat_df_output)))) {
            if(!stat_type %in% c("keeper", "shots")) {
              if(team_or_player == "team") {
                stat_df_output <- stat_df_output %>%
                  dplyr::filter(stringr::str_detect(.data[["Player"]], " Players")) %>%
                  dplyr::select(-.data[["Player"]], -.data[["Player_Num"]], -.data[["Nation"]], -.data[["Pos"]], -.data[["Age"]])
              } else {
                stat_df_output <- stat_df_output %>%
                  dplyr::filter(!stringr::str_detect(.data[["Player"]], " Players"))
              }

            }
          } else {
            if(!stat_type %in% c("keeper", "shots")) {
              if(team_or_player == "team") {
                stat_df_output <- stat_df_output %>%
                  dplyr::filter(stringr::str_detect(.data[["Player"]], " Players")) %>%
                  dplyr::select(-.data[["Player"]], -.data[["Player_Num"]], -.data[["Pos"]], -.data[["Age"]])
              } else {
                stat_df_output <- stat_df_output %>%
                  dplyr::filter(!stringr::str_detect(.data[["Player"]], " Players"))
              }

            }
          }


          stat_df_output <- dplyr::bind_cols(match_report, stat_df_output)

          if(nrow(stat_df_output) == 0) {
            print(glue::glue("NOTE: Stat Type '{stat_type}' is not found for this match. Check {match_url} to see if it exists."))
          }

        } else if(stat_type == "shots") {

        }
      } else {
        print(glue::glue("NOTE: Stat Type '{stat_type}' is not found for this match. Check {match_url} to see if it exists."))
        stat_df_output <- data.frame()
      }

    } else {
      print(glue::glue("Stats data not available for {match_url}"))
      stat_df_output <- data.frame()
    }

    return(stat_df_output)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(match_url))

  suppressWarnings(
    final_df <- match_url %>%
      purrr::map_df(get_each_match_statistic)
  )

  return(final_df)
}





#' Get advanced match stats
#'
#' Returns data frame of selected statistics for each match, for either whole team or individual players.
#' Multiple URLs can be passed to the function, but only one `stat_type` can be selected
#'
#' @param match_url the three character country code for all countries
#' @param stat_type the type of team statistics the user requires
#' @param team_or_player result either summarised for each team, or individual players
#' @param time_pause the wait time (in seconds) between page loads
#'
#' The statistic type options (stat_type) include:
#'
#' \emph{"summary"}, \emph{"passing"}, \emph{"passing"_types},
#' \emph{"defense" }, \emph{"possession"}, \emph{"misc"}, \emph{"keeper"}
#'
#' @return returns a dataframe of a selected team statistic type for a selected match(es)
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
#' urls <- get_match_urls(country = "AUS", gender = "F", season_end_year = 2021, tier = "1st")
#'
#' df <- get_advanced_match_stats(match_url=urls,stat_type="possession",team_or_player="player")
#' })
#' }

get_advanced_match_stats <- function(match_url, stat_type, team_or_player, time_pause=3) {

  .Deprecated("fb_advanced_match_stats")

  main_url <- "https://fbref.com"

  time_wait <- time_pause

  get_each_match_statistic <- function(match_url, time_pause=time_wait) {
    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    match_page <- tryCatch(xml2::read_html(match_url), error = function(e) NA)

    if(!is.na(match_page)) {
      match_report <- .get_match_report_page(match_page = match_page)

      league_url <- match_page %>%
        rvest::html_nodes("#content") %>%
        rvest::html_node("a") %>%
        rvest::html_attr("href") %>% paste0(main_url, .)

      all_tables <- match_page %>%
        rvest::html_nodes(".table_container")


      if(stat_type == "summary") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "summary$"))] %>%
          rvest::html_nodes("table")

      } else if(stat_type == "passing") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "passing$"))] %>%
          rvest::html_nodes("table")

      } else if(stat_type == "passing_types") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "passing_types"))] %>%
          rvest::html_nodes("table")

      } else if(stat_type == "defense") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "defense$"))] %>%
          rvest::html_nodes("table")

      } else if(stat_type == "possession") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "possession$"))] %>%
          rvest::html_nodes("table")


      } else if(stat_type == "misc") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "misc$"))] %>%
          rvest::html_nodes("table")

      } else if(stat_type == "keeper") {
        stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "keeper_stats"))] %>%
          rvest::html_nodes("table")

      }
      # shots stat type is not yet built in to the function
      # else if(stat_type == "shots") {
      #   stat_df <- all_tables[which(stringr::str_detect(all_tables %>% rvest::html_attr("id"), "all_shots"))] %>% .[1] %>%
      #     rvest::html_table()
      #
      # }



      if(length(stat_df) != 0) {

        if(!stat_type %in% c("shots")) {
          Team <- match_page %>%
            rvest::html_nodes("div:nth-child(1) div strong a") %>%
            rvest::html_text() %>% .[1]

          home_stat <- stat_df[1] %>%
            rvest::html_table() %>% data.frame() %>%
            .clean_match_advanced_stats_data()

          Home_Away <- "Home"
          home_stat <- cbind(Team, Home_Away, home_stat)

          Team <- match_page %>%
            rvest::html_nodes("div:nth-child(2) div strong a") %>%
            rvest::html_text() %>% .[2]

          away_stat <- stat_df[2] %>%
            rvest::html_table() %>% data.frame() %>%
            .clean_match_advanced_stats_data()

          Home_Away <- "Away"
          away_stat <- cbind(Team, Home_Away, away_stat)

          stat_df_output <- dplyr::bind_rows(home_stat, away_stat)

          if(any(grepl("Nation", colnames(stat_df_output)))) {
            if(!stat_type %in% c("keeper", "shots")) {
              if(team_or_player == "team") {
                stat_df_output <- stat_df_output %>%
                  dplyr::filter(stringr::str_detect(.data[["Player"]], " Players")) %>%
                  dplyr::select(-.data[["Player"]], -.data[["Player_Num"]], -.data[["Nation"]], -.data[["Pos"]], -.data[["Age"]])
              } else {
                stat_df_output <- stat_df_output %>%
                  dplyr::filter(!stringr::str_detect(.data[["Player"]], " Players"))
              }

            }
          } else {
            if(!stat_type %in% c("keeper", "shots")) {
              if(team_or_player == "team") {
                stat_df_output <- stat_df_output %>%
                  dplyr::filter(stringr::str_detect(.data[["Player"]], " Players")) %>%
                  dplyr::select(-.data[["Player"]], -.data[["Player_Num"]], -.data[["Pos"]], -.data[["Age"]])
              } else {
                stat_df_output <- stat_df_output %>%
                  dplyr::filter(!stringr::str_detect(.data[["Player"]], " Players"))
              }

            }
          }


          stat_df_output <- cbind(match_report, stat_df_output)
        } else if(stat_type == "shots") {

        }
      } else {
        print(glue::glue("NOTE: Stat Type '{stat_type}' is not found for this match. Check {match_url} to see if it exists."))
        stat_df_output <- data.frame()
      }

    } else {
      print(glue::glue("Stats data not available for {match_url}"))
      stat_df_output <- data.frame()
    }

    return(stat_df_output)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(match_url))

  suppressWarnings(
    final_df <- match_url %>%
      purrr::map_df(get_each_match_statistic) )

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  seasons <- seasons %>%
    dplyr::filter(.data[["seasons_urls"]] %in% final_df$League_URL) %>%
    dplyr::select(League=.data[["competition_name"]], Gender=.data[["gender"]], Country=.data[["country"]], Season=.data[["seasons"]], League_URL=.data[["seasons_urls"]])

  final_df <- seasons %>%
    dplyr::left_join(final_df, by = "League_URL") %>%
    dplyr::select(-.data[["League_URL"]]) %>% dplyr::distinct(.keep_all = T)

  return(final_df)
}
