#' Get Tier 1 competitions
#'
#' Returns a df of the top leagues around the world
#'
#' @return returns a dataframe with the tier 1 competitions from around the world
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'

.get_tier1_competitions <- function() {
  main_url <- "https://fbref.com"
  # read page to all competitions
  all_comps_url <- xml2::read_html("https://fbref.com/en/comps/")
  # this just gets the Tier 1 club comps - this will need to be modified if more comps are required
  comps <- all_comps_url %>% rvest::html_nodes("#all_comps_1_fa_club_league_senior")
  # get the urls for each competition, then paste fbref url
  competition_urls <- comps %>% rvest::html_node("tbody") %>% rvest::html_nodes("th a") %>% rvest::html_attr("href")
  competition_urls <- paste0(main_url, competition_urls)
  # scrape the table that contains the competitons
  competitions <- comps %>% rvest::html_nodes(".sortable") %>% rvest::html_table() %>% data.frame()
  # add the competition url column
  competitions <- cbind(competitions, competition_urls)
  # remove the two character country code for the flag, and only leave the 3 character code
  competitions$Country <- gsub(".*? ", "", competitions$Country)

  return(competitions)
}



#' Get URL of league season
#'
#' Returns a URL for the selected league seasons
#'
#' @param country the three character country code
#' @param gender gender of competition, either "M" or "F"
#' @param season the year the season concludes, in quotes, ie "2021"

#' @return a URL for the selected league seasons
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
.get_league_season_url <- function(country, gender, season) {
  main_url <- "https://fbref.com"

  competitions <- .get_tier1_competitions()

  league_url <- competitions %>%
    dplyr::filter(toupper(.data$Country) == toupper(country),
                  toupper(.data$Gender) == toupper(gender)) %>%
    dplyr::pull(.data$competition_urls)

  stopifnot("Country or Gender not found" =
              toupper(country) %in% competitions$Country,
            toupper(gender) %in% competitions$Gender)

  league_url <- xml2::read_html(league_url)

  seasons <- league_url %>%
    rvest::html_nodes("th a") %>%
    rvest::html_text() %>%
    gsub(".*-", "", .)

  stopifnot("Season not found or needs to be wrapped in quotes" =
              season %in% seasons)

  seasons_urls <- league_url %>%
    rvest::html_nodes("th a") %>%
    rvest::html_attr("href") %>%
    paste0(main_url, .)

  select_season <- cbind(seasons, seasons_urls) %>% data.frame() %>%
    dplyr::filter(seasons == season) %>%
    dplyr::pull(seasons_urls)

  return(select_season)

}




#' Clean advanced statistic tables
#'
#' Returns cleaned dataframe for each of the team statistic tables
#'
#' @param advanced_stat_url url of the html table on the league season page
#'
#' @return a data frame for the selected league seasons advanced statistic
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
.clean_advanced_stat_table <- function(advanced_stat_url) {

  stat_df <- xml2::read_html(advanced_stat_url) %>%
    rvest::html_table() %>%
    data.frame()

  var_names <- stat_df[1,] %>% as.character()

  new_names <- paste(var_names, names(stat_df), sep = "_")

  new_names <- new_names %>%
    gsub("\\..*", "", .) %>%
    gsub("_Var", "", .) %>%
    gsub("# Pl", "Num_Players", .) %>%
    gsub("%", "_percent", .) %>%
    gsub("_Performance", "", .) %>%
    gsub("_Penalty", "", .) %>%
    gsub("1/3", "Final_Third", .) %>%
    gsub("/", "_per_", .) %>%
    gsub("-", "_minus_", .) %>%
    gsub("90s", "Mins_Per_90", .)

  names(stat_df) <- new_names
  stat_df <- stat_df[-1,]

  cols_to_transform <- stat_df %>%
    dplyr::select(-.data$Squad) %>% names()

  stat_df <- stat_df %>%
    dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub(",", "", x)}) %>%
    dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub("+", "", x)}) %>%
    dplyr::mutate_at(.vars = cols_to_transform, .funs = as.numeric)

  return(stat_df)
}



#' Clean each match advanced statistic tables
#'
#' Returns cleaned data frame for each of the team statistic tables for each selected match
#'
#' @param df_in a raw match stats data frame
#'
#' @return a cleaned data frame for the selected match advanced statistic
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'

.clean_match_advanced_stats_data <- function(df_in) {

  var_names <- df_in[1,] %>% as.character()

  new_names <- paste(var_names, names(df_in), sep = "_")

  new_names <- new_names %>%
    gsub("\\..*", "", .) %>%
    gsub("_Var", "", .) %>%
    gsub("#", "Player_Num", .) %>%
    gsub("%", "_percent", .) %>%
    gsub("_Performance", "", .) %>%
    gsub("_Penalty", "", .) %>%
    gsub("1/3", "Final_Third", .) %>%
    gsub("/", "_per_", .) %>%
    gsub("-", "_minus_", .) %>%
    gsub("90s", "Mins_Per_90", .)

  names(df_in) <- new_names
  df_in <- df_in[-1,]

  df_in$Nation <- gsub(".*? ", "", df_in$Nation)

  # cols_to_transform <- df_in %>%
  #   dplyr::select(-.data$Player, -.data$Nation, -.data$Pos, -.data$Age) %>% names()

  non_num_vars <- c("Player", "Nation", "Pos", "Age")
  cols_to_transform <- names(df_in)[!names(df_in) %in% non_num_vars]

  df_in <- df_in %>%
    dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub(",", "", x)}) %>%
    dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub("+", "", x)}) %>%
    dplyr::mutate_at(.vars = cols_to_transform, .funs = as.numeric)

  return(df_in)
}


#' Convert formatted valuations to numeric
#'
#' Returns a numeric data type for player valuations
#'
#' @param euro_value raw valuation from transfermarkt.com
#'
#' @return a cleaned numeric data value for market and/or transfer valuation
#'
#' @importFrom magrittr %>%
#'
.convert_value_to_numeric <- function(euro_value) {
  clean_val <- gsub("[^\x20-\x7E]", "", euro_value) %>% tolower()
  if(grepl("m", clean_val)) {
    clean_val <- suppressWarnings(gsub("m", "", clean_val) %>% as.numeric() * 1000000)
  } else if(grepl("th.", clean_val)) {
    clean_val <- suppressWarnings(gsub("th.", "", clean_val) %>% as.numeric() * 1000)
  } else {
    clean_val <- suppressWarnings(as.numeric(clean_val) * 1)
  }
  return(clean_val)
}
