#' Get fbref Team's Player Season Statistics
#'
#' Returns the team's players season stats for a selected team(s) and stat type
#'
#' @param team_urls the URL(s) of the teams(s) (can come from fb_teams_urls())
#' @param stat_type the type of statistic required
#' @param time_pause the wait time (in seconds) between page loads
#'
#'The statistic type options (stat_type) include:
#'
#' \emph{"standard"}, \emph{"shooting"}, \emph{"passing"},
#' \emph{"passing_types"}, \emph{"gca"}, \emph{"defense"}, \emph{"possession"}
#' \emph{"playing_time"}, \emph{"misc"}, \emph{"keeper"}, \emph{"keeper_adv"}
#'
#' @return returns a dataframe of all players of a team's season stats
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' fb_team_player_stats("https://fbref.com/en/squads/d6a369a2/Fleetwood-Town-Stats",
#'                        stat_type = 'standard')
#'
#' league_url <- fb_league_urls(country = "ENG", gender = "M",
#'                                              season_end_year = 2022, tier = "3rd")
#' team_urls <- fb_teams_urls(league_url)
#' multiple_playing_time <- fb_team_player_stats(team_urls,
#'                          stat_type = "playing_time")
#' })
#' }

fb_team_player_stats <- function(team_urls, stat_type, time_pause=3) {

  stat_types <- c("standard", "shooting", "passing", "passing_types", "gca", "defense", "possession", "playing_time", "misc", "keeper", "keeper_adv")

  if(!stat_type %in% stat_types) stop("check stat type")

  # .pkg_message("Scraping {team_or_player} season '{stat_type}' stats")

  main_url <- "https://fbref.com"

  time_wait <- time_pause

  get_each_team_players <- function(team_url, time_pause=time_wait) {

    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    page <- .load_page(team_url)

    team_season <- page %>% rvest::html_nodes("h1") %>% rvest::html_nodes("span") %>% .[1] %>% rvest::html_text()
    season <- team_season %>% stringr::str_extract(., "(\\d+)-(\\d+)")
    Squad <- team_season %>% gsub("(\\d+)-(\\d+)", "", .) %>% gsub("\\sStats", "", .) %>% stringr::str_squish()
    league <- page %>% rvest::html_nodes("h1") %>% rvest::html_nodes("span") %>% .[2] %>% rvest::html_text() %>% gsub("\\(", "", .) %>% gsub("\\)", "", .)

    tabs <- page %>% rvest::html_nodes(".table_container")
    tab_names <- page %>% rvest::html_nodes("#content") %>% rvest::html_nodes(".table_wrapper") %>% rvest::html_attr("id")

    tab_idx <- grep(paste0("all_stats_", stat_type, "$"), tab_names)

    if(length(tab_idx) == 0) {
      stop(glue::glue("Stat: {stat_type} not available for {Squad}"))
      tab <- data.frame()
    } else {
      tab <- tabs[tab_idx] %>% rvest::html_node("table") %>% rvest::html_table() %>% data.frame()

      var_names <- tab[1,] %>% as.character()
      new_names <- paste(var_names, names(tab), sep = "_")

      new_names <- new_names %>%
        gsub("\\..[0-9]", "", .) %>%
        gsub("\\.[0-9]", "", .) %>%
        gsub("\\.", "_", .) %>%
        gsub("_Var", "", .) %>%
        gsub("#", "Player_Num", .) %>%
        gsub("%", "_percent", .) %>%
        gsub("_Performance", "", .) %>%
        gsub("_Penalty", "", .) %>%
        gsub("1/3", "Final_Third", .) %>%
        gsub("\\+/-", "Plus_Minus", .) %>%
        gsub("/", "_per_", .) %>%
        gsub("-", "_minus_", .) %>%
        gsub("90s", "Mins_Per_90", .) %>%
        gsub("__", "_", .)

      names(tab) <- new_names
      tab <- tab[-1,]

      remove_rows <- min(grep("Squad ", tab$Player)):nrow(tab)
      tab <- tab[-remove_rows, ]
      tab$Matches <- NULL

      if(any(grepl("Nation", colnames(tab)))) {
        tab$Nation <- gsub(".*? ", "", tab$Nation)
      }

      non_num_vars <- c("Player", "Nation", "Pos", "Age")
      cols_to_transform <- names(tab)[!names(tab) %in% non_num_vars]

      tab <- tab %>%
        dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub(",", "", x)}) %>%
        dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub("+", "", x)}) %>%
        dplyr::mutate_at(.vars = cols_to_transform, .funs = as.numeric)

      # this is a new fix and more robust to occasions when some players don't have a URL
      tab_player_urls <- tabs[tab_idx] %>% rvest::html_node("table")
      url_rows <- tab_player_urls %>% rvest::html_elements("tbody tr")

      player_urls <- c()
      for(i in url_rows) {
        each_url <- i %>% rvest::html_elements("th a") %>% rvest::html_attr("href") %>%
          paste0(main_url, .)
        # the below will catch when there is no player URL and it returns NA
        if(each_url == "https://fbref.com") {
          each_url <- NA_character_
        }
        player_urls <- c(player_urls, each_url)
      }


      tab <- tab %>%
        dplyr::mutate(Season = season,
                      Squad=Squad,
                      Comp=league,
                      PlayerURL = player_urls) %>%
        dplyr::select(.data[["Season"]], .data[["Squad"]], .data[["Comp"]], dplyr::everything())
    }

    return(tab)

  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(team_urls))

  all_stats_df <- team_urls %>%
    purrr::map_df(get_each_team_players)

  return(all_stats_df)

}
