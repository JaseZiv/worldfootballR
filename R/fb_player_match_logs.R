#' Get fbref Player Match Logs
#'
#' Returns all match logs for a selected player, season and stat type
#'
#' @param player_url the URL of the player (can come from fb_player_urls())
#' @param season_end_year the year the season(s) concludes
#' @param stat_type the type of statistic required
#' @param time_pause the wait time (in seconds) between page loads
#'
#'The statistic type options (stat_type) include:
#'
#' \emph{"summary"}, \emph{"keepers"}, \emph{"passing"}, \emph{"passing_types"},
#' \emph{"gca"}, \emph{"defense"}, \emph{"possession"}, \emph{"misc"}
#'
#' @return returns a dataframe of a player's match logs for a season
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fb_player_match_logs("https://fbref.com/en/players/3bb7b8b4/Ederson",
#' season_end_year = 2021, stat_type = 'summary')
#' }

fb_player_match_logs <- function(player_url, season_end_year, stat_type, time_pause=2) {

  stat_types <- c("summary", "keepers", "passing", "passing_types", "gca", "defense", "possession", "misc")
  if(!stat_type %in% stat_types) stop("check stat type")


  main_url <- "https://fbref.com"

  # put sleep in as per new user agreement on FBref
  Sys.sleep(time_pause)

  player_page <- xml2::read_html(player_url)

  player_name <- player_page %>% rvest::html_node("h1") %>% rvest::html_text() %>% stringr::str_squish()

  main_cats <- player_page %>% rvest::html_nodes("#inner_nav") %>% rvest::html_nodes(".full.hasmore")
  span_names <- main_cats %>% rvest::html_nodes("span") %>% rvest::html_text()


  main_cats <- main_cats[grep("Match Logs", span_names)]

  match_log_names <- main_cats %>% rvest::html_nodes("p") %>% rvest::html_text()

  match_log_names <- match_log_names %>% gsub("Match Logs \\(", "", .) %>% gsub("\\)", "", .)

  log_level1 <- main_cats %>% rvest::html_nodes("ul")

  # create a dataframe of all stat types, seasons and their URLs
  # this is to combat the way the html is laid out on the site
  all_logs <- data.frame()

  for(i in 1:length(log_level1)) {
    log_name <- match_log_names[i]
    log_urls <- log_level1[i] %>% rvest::html_nodes("li") %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
    season <- log_level1[i] %>% rvest::html_nodes("li") %>% rvest::html_nodes("a") %>% rvest::html_text()

    each_log_df <- cbind(log_name=log_name, log_urls=log_urls, season=season) %>% data.frame()

    all_logs <- rbind(all_logs, each_log_df)
  }

  all_logs <- all_logs %>%
    dplyr::mutate(season_end = gsub(".*-", "", .data$season),
           stat = dplyr::case_when(
             log_name == "Summary" ~ "summary",
             log_name == "Goalkeeping" ~ "keepers",
             log_name == "Passing" ~ "passing",
             log_name == "Pass Types" ~ "passing_types",
             log_name == "Goal and Shot Creation" ~ "gca",
             log_name == "Defensive Actions" ~ "defense",
             log_name == "Possession" ~ "possession",
             log_name == "Miscellaneous Stats" ~ "misc",
             TRUE ~ NA_character_
           ))

  log_url <- all_logs %>%
    dplyr::filter(.data$stat == stat_type,
                  .data$season_end == season_end_year) %>%
    dplyr::pull(log_urls)

  if(length(log_url) == 0) stop(glue::glue("check stat type: {stat_type} or season end: {season_end_year} exists"))


  season <- all_logs %>%
    dplyr::filter(.data$stat == stat_type,
                  .data$season_end == season_end_year) %>%
    dplyr::pull(season)

  # Get match logs for stat -------------------------------------------------

  Sys.sleep(1)
  stat_page <- xml2::read_html(paste0(main_url, log_url))

  tab <- stat_page %>% rvest::html_nodes(".table_container") %>% rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame()

  tab <- .clean_table_names(tab)

  tab <- tab %>%
    dplyr::filter(.data$Date != "") %>%
    dplyr::mutate(Squad = sub("^.*?([A-Z])", "\\1", .data$Squad),
           Opponent = sub("^.*?([A-Z])", "\\1", .data$Opponent),
           Player = player_name,
           Season = season) %>%
    dplyr::select(.data$Player, .data$Season, dplyr::everything(), -.data$`Match Report`)



  non_num_vars <- c("Player", "Season", "Date", "Day", "Comp", "Round", "Venue", "Result", "Squad", "Opponent", "Start", "Pos")
  cols_to_transform <- names(tab)[!names(tab) %in% non_num_vars]

  suppressWarnings(
    tab <- tab %>%
    dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub(",", "", x)}) %>%
    dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub("+", "", x)}) %>%
    dplyr::mutate_at(.vars = cols_to_transform, .funs = as.numeric)
  )

  return(tab)

}


