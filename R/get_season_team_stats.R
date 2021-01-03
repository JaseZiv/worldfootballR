#' Get season team stats
#'
#' Returns different team season statistics results for a given league season and stat type
#'
#' @param country the three character country code
#' @param gender gender of competition, either "M" or "F"
#' @param season_end_year the year the season concludes, in quotes, ie "2021"
#' @param stat_type the type of team statistics the user requires
#'
#' The statistic type options (stat_type) include:
#'
#' \emph{"league_table"}, \emph{"league_table_home_away}", \emph{"standard"},
#' \emph{"keeper"}, \emph{"keeper_adv"}, \emph{"shooting"}, \emph{"passing"},
#' \emph{"goal_shot_creation"}, \emph{"defense" }, \emph{"possession"},
#' \emph{"playing_time"}, \emph{"misc"}
#'
#' @return returns a dataframe of a selected team statistic type for a selected league season
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_team_statistics(country = "ITA", gender = "M", season_end_year = "2020", stat_type = defense)
#' }




get_season_team_stats <- function(country, gender, season_end_year,
                                stat_type = c("league_table", "league_table_home_away", "standard", "keeper", "keeper_adv",
                                              "shooting", "passing", "goal_shot_creation", "defense", "possession", "playing_time",
                                              "misc")) {

  select_season <- .get_league_season_url(country=country, gender=gender, season=season_end_year)

  league_standings <- xml2::read_html(select_season) %>% rvest::html_nodes("table")


  each_stat_table_url <- xml2::read_html(select_season) %>%
    rvest::html_nodes(".table_wrapper") %>%
    rvest::html_nodes(".section_heading_text") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>% .[!is.na(.)] %>% unique() %>%
    paste0("https://fbref.com", .)


  if(stat_type == "league_table") {
    stat_df <- league_standings[1] %>% rvest::html_table() %>% data.frame()

  } else if(stat_type == "league_table_home_away") {
    stat_df <- league_standings[2] %>% rvest::html_table() %>% data.frame()

    var_names <- stat_df[1,] %>% as.character()

    new_names <- paste(var_names, names(stat_df), sep = "_")

    new_names <- new_names %>%
      gsub("\\..*", "", .) %>%
      gsub("_Var", "", .) %>%
      gsub("/", "_per_", .)

    names(stat_df) <- new_names
    stat_df <- stat_df[-1,]

    cols_to_transform <- stat_df %>%
      dplyr::select(-.data$Squad) %>% names()

    stat_df <- stat_df %>%
      dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub(",", "", x)}) %>%
      dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub("+", "", x)}) %>%
      dplyr::mutate_at(.vars = cols_to_transform, .funs = as.numeric)

  } else if(stat_type == "standard") {
    stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_standard"))]

    stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

  } else if(stat_type == "keeper") {
    stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_keeper$"))]

    stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

  } else if(stat_type == "keeper_adv") {
    stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_keeper_adv"))]

    stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

  } else if(stat_type == "shooting") {
    stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_shooting$"))]

    stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

  } else if(stat_type == "passing") {
    stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_passing$"))]

    stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

  } else if(stat_type == "goal_shot_creation") {
    stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_gca$"))]

    stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

  } else if(stat_type == "defense") {
    stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_defense$"))]

    stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

  } else if(stat_type == "possession") {
    stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_possession$"))]

    stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

  } else if(stat_type == "playing_time") {
    stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_playing_time$"))]

    stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

  } else if(stat_type == "misc") {
    stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_misc$"))]

    stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)
  }

  names(stat_df) <- gsub("\\+", "_plus_", names(stat_df))

  stat_df <- stat_df %>%
    dplyr::mutate(Team_or_Opponent = ifelse(!stringr::str_detect(.data$Squad, "vs "), "team", "opponent")) %>%
    dplyr::filter(.data$Team_or_Opponent == "team") %>%
    dplyr::bind_rows(
      stat_df %>%
        dplyr::filter(.data$Team_or_Opponent == "opponent")
    ) %>%
    dplyr::select(.data$Squad, .data$Team_or_Opponent, dplyr::everything())

  return(stat_df)
}
