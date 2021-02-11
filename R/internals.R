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
    # gsub("_Penalty", "", .) %>%
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
