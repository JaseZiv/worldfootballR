#' Get season team stats
#'
#' Returns different team season statistics results for a given league season and stat type
#'
#' @param country the three character country code for all countries
#' @param gender gender of competition, either "M", "F" or both
#' @param season_end_year the year the season(s) concludes
#' @param stat_type the type of team statistics the user requires
#'
#' The statistic type options (stat_type) include:
#'
#' \emph{"league_table"}, \emph{"league_table_home_away}", \emph{"standard"},
#' \emph{"keeper"}, \emph{"keeper_adv"}, \emph{"shooting"}, \emph{"passing"},
#' \emph{"passing_types"}, \emph{"goal_shot_creation"}, \emph{"defense" },
#' \emph{"possession"}, \emph{"playing_time"}, \emph{"misc"}
#'
#' @return returns a dataframe of a selected team statistic type for a selected league season
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_team_statistics(country = "ITA", gender = "M", season_end_year = c(2017:2021), stat_type = "defense")
#' }

get_season_team_stats <- function(country, gender, season_end_year,
                                  stat_type = c("league_table", "league_table_home_away", "standard", "keeper", "keeper_adv",
                                                "shooting", "passing", "passing_types", "goal_shot_creation", "defense", "possession",
                                                "playing_time", "misc")) {

  print(glue::glue("Scraping season {stat_type} stats"))

  country_abbr <- country
  gender_M_F <- gender
  season_end_year_num <- season_end_year


  main_url <- "https://fbref.com"

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/league_seasons/all_tier1_season_URLs.csv")

  seasons <- seasons %>%
    dplyr::filter(country %in% country_abbr,
                  gender %in% gender_M_F,
                  season_end_year %in% season_end_year_num)

  seasons_urls <- seasons %>%
    dplyr::pull(seasons_urls)


  get_each_stats_type <- function(season_url) {

    season_stats_page <- xml2::read_html(season_url)

    league_standings <- season_stats_page %>% rvest::html_nodes("table")


    each_stat_table_url <- season_stats_page %>%
      rvest::html_nodes(".table_wrapper") %>%
      rvest::html_nodes(".section_heading_text") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>% .[!is.na(.)] %>% unique() %>%
      paste0("https://fbref.com", .)

    tryCatch(
      if(stat_type == "league_table") {
        stats_url <- NA
        stat_df <- league_standings[1] %>% rvest::html_table() %>% data.frame()

      } else if(stat_type == "league_table_home_away") {
        stats_url <- NA
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

      } else if(stat_type == "passing_types") {
        stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_passing_types"))]

        stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

      } else if(stat_type == "goal_shot_creation") {
        stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_gca$"))]

        stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

      } else if(stat_type == "defense") {
        stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_defense$"))]

        stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

      } else if(stat_type == "possession") {
        stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_possession$"))]

        stat_df <- tryCatch(.clean_advanced_stat_table(advanced_stat_url = stats_url), error = function(e) data.frame())

      } else if(stat_type == "playing_time") {
        stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_playing_time$"))]

        stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)

      } else if(stat_type == "misc") {
        stats_url <- each_stat_table_url[which(stringr::str_detect(each_stat_table_url, "#all_stats_misc$"))]

        stat_df <- .clean_advanced_stat_table(advanced_stat_url = stats_url)
      }, error = function(e)  {stat_df <- data.frame()} )

    if(length(stats_url) == 0) {
      stat_df <- data.frame()
      print(glue::glue("NOTE: Stat Type '{stat_type}' is not found for this league season. Check {season_url} to see if it exists."))

    } else {
      # stopifnot("Data not available, see message above" = length(stats_url) > 0)
      names(stat_df) <- gsub("\\+", "_plus_", names(stat_df))


      stat_df <- stat_df %>%
        dplyr::mutate(Team_or_Opponent = ifelse(!stringr::str_detect(.data$Squad, "vs "), "team", "opponent")) %>%
        dplyr::filter(.data$Team_or_Opponent == "team") %>%
        dplyr::bind_rows(
          stat_df %>%
            dplyr::mutate(Team_or_Opponent = ifelse(!stringr::str_detect(.data$Squad, "vs "), "team", "opponent")) %>%
            dplyr::filter(.data$Team_or_Opponent == "opponent")
        ) %>%
        dplyr::mutate(season_url = season_url) %>%
        dplyr::select(.data$season_url, .data$Squad, .data$Team_or_Opponent, dplyr::everything())

      stat_df <- seasons %>%
        dplyr::select(Competition_Name=.data$competition_name, Gender=.data$gender, Country=.data$country, Season_End_Year=.data$season_end_year, .data$seasons_urls) %>%
        dplyr::left_join(stat_df, by = c("seasons_urls" = "season_url")) %>%
        dplyr::select(-.data$seasons_urls) %>%
        dplyr::filter(!is.na(.data$Squad)) %>%
        dplyr::arrange(.data$Country, .data$Competition_Name, .data$Gender, .data$Season_End_Year, dplyr::desc(.data$Team_or_Opponent), .data$Squad)
    }


    return(stat_df)

  }

  all_stats_df <- seasons_urls %>%
    purrr::map_df(get_each_stats_type)

  return(all_stats_df)

}
