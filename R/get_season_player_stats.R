#' Get fbref Player Season Statistics
#'
#' Returns the historical season stats for a selected player(s) and stat type
#'
#' @param player_url the URL(s) of the player(s) (can come from fb_player_urls())
#' @param stat_type the type of statistic required
#' @param national the category of the required stats (club/country)
#' @param time_pause the wait time (in seconds) between page loads
#'
#' The statistic type options (stat_type) include:
#'
#' \emph{"standard"}, \emph{"shooting"}, \emph{"passing"},
#' \emph{"passing_types"}, \emph{"gca"}, \emph{"defense"}, \emph{"possession"}
#' \emph{"playing_time"}, \emph{"misc"}, \emph{"keeper"}, \emph{"keeper_adv"}
#'
#' @return returns a dataframe of a player's historical season stats
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#'   fb_player_season_stats("https://fbref.com/en/players/3bb7b8b4/Ederson",
#'     stat_type = "standard"
#'   )
#'
#'   multiple_playing_time <- fb_player_season_stats(
#'     player_url = c(
#'       "https://fbref.com/en/players/d70ce98e/Lionel-Messi",
#'       "https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo"
#'     ),
#'     stat_type = "playing_time"
#'   )
#' })
#' }

fb_player_season_stats <- function(player_url, stat_type, national = FALSE, time_pause = 3) {
  main_url <- "https://fbref.com"

  time_wait <- time_pause
  national_bool <- national

  get_each_player_stuff <- function(player_url, stat_type, national = national_bool, time_pause = time_wait) {

    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    stat_types <- c("standard", "shooting", "passing", "passing_types", "gca", "defense", "possession", "playing_time", "misc", "keeper", "keeper_adv")

    if (!stat_type %in% stat_types) stop("check stat type")

    player_page <- .load_page(player_url)

    player_name <- player_page %>%
      rvest::html_node("h1") %>%
      rvest::html_text() %>%
      stringr::str_squish()

    comps_filters <- player_page %>%
      rvest::html_nodes(".filter") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      .[!is.na(.)]

    if (national) {
      stats_string <- "National-Team"
    } else {
      stats_string <- "All-Compe"
    }

    stat_df <- data.frame()

    if (any(grepl(stats_string, comps_filters))) {
      competition_url <- player_page %>%
        rvest::html_nodes(".filter") %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") %>%
        .[grep(stats_string, .)] %>%
        paste0(main_url, .)

      Sys.sleep(time_pause)

      competition_page <- .load_page(competition_url)

      expanded_table_elements <- competition_page %>%
        rvest::html_nodes(".table_container") %>%
        rvest::html_nodes("table")

      expanded_table_idx <- c()

      for (i in 1:length(expanded_table_elements)) {
        idx <- xml2::xml_attrs(expanded_table_elements[[i]])[["id"]]
        expanded_table_idx <- c(expanded_table_idx, idx)
      }

      idx <- grep("_expanded", expanded_table_idx)

      if (length(idx) == 0) {
        if (national) {
          competition_string <- "_nat_tm"
        } else {
          competition_string <- "_dom_lg"
        }

        idx <- grep(competition_string, expanded_table_idx)

        expanded_table_idx <- expanded_table_idx[idx]

        expanded_table_elements <- expanded_table_elements[idx]

        stat_df <- tryCatch(
          .clean_player_season_stats(expanded_table_elements[which(stringr::str_detect(expanded_table_idx, paste0("stats_", stat_type, competition_string)))]),
          error = function(e) data.frame()
        )

      } else {
        expanded_table_idx <- expanded_table_idx[idx]

        expanded_table_elements <- expanded_table_elements[idx]

        stat_df <- tryCatch(
          .clean_player_season_stats(expanded_table_elements[which(stringr::str_detect(expanded_table_idx, paste0("stats_", stat_type, "_expanded")))]),
          error = function(e) data.frame()
        )
      }
    }

    if (nrow(stat_df) == 0) {
      error_info <- "{stat_type} data not available for: {player_url}"
      if(national){
        print(glue::glue("National ", error_info))
      } else {
        print(glue::glue(error_info))
      }
    } else {

      stat_df <- stat_df %>%
        filter(!is.na(Age))

      stat_df <- stat_df %>%
        dplyr::mutate(
          player_name = player_name,
          player_url = player_url
        ) %>%
        dplyr::select(player_name, player_url, dplyr::everything())
    }

    return(stat_df)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(player_url))

  # run for all players selected
  all_players <- purrr::map2_df(player_url, stat_type, get_each_player_stuff)
}
