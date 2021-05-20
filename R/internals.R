#' Clean advanced statistic tables
#'
#' Returns cleaned dataframe for each of the team statistic tables used by get_season_team_stats()
#'
#' @param input_table_element element of the html table on the league season page
#'
#' @return a data frame for the selected league seasons advanced statistic
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
.clean_advanced_stat_table <- function(input_table_element) {


  stat_df <- input_table_element %>%
    rvest::html_table() %>%
    data.frame()

  var_names <- stat_df[1,] %>% as.character()

  new_names <- paste(var_names, names(stat_df), sep = "_")

  new_names <- new_names %>%
    gsub("\\..[0-9]", "", .) %>%
    gsub("\\.[0-9]", "", .) %>%
    gsub("\\.", "_", .) %>%
    gsub("_Var", "", .) %>%
    gsub("# Pl", "Num_Players", .) %>%
    gsub("%", "_percent", .) %>%
    gsub("_Performance", "", .) %>%
    # gsub("_Penalty", "", .) %>%
    gsub("1/3", "Final_Third", .) %>%
    gsub("\\+/-", "Plus_Minus", .) %>%
    gsub("/", "_per_", .) %>%
    gsub("-", "_minus_", .) %>%
    gsub("90s", "Mins_Per_90", .) %>%
    gsub("__", "_", .)

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

#' Clean player season statistic tables
#'
#' Returns cleaned dataframe for each of the player statistic tables used by fb_player_season_stats()
#'
#' @param input_table_element element of the html table on the player page
#'
#' @return a data frame for the selected player's advanced statistic
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
.clean_player_season_stats <- function(input_table_element) {


  stat_df <- input_table_element %>%
    rvest::html_table() %>%
    data.frame()

  var_names <- stat_df[1,] %>% as.character()

  new_names <- paste(var_names, names(stat_df), sep = "_")

  new_names <- new_names %>%
    gsub("\\..[0-9]", "", .) %>%
    gsub("\\.[0-9]", "", .) %>%
    gsub("\\.", "_", .) %>%
    gsub("_Var", "", .) %>%
    gsub("_Playing", "", .) %>%
    gsub("%", "_percent", .) %>%
    gsub("_Performance", "", .) %>%
    # gsub("_Penalty", "", .) %>%
    gsub("1/3", "Final_Third", .) %>%
    gsub("\\+/-", "Plus_Minus", .) %>%
    gsub("/", "_per_", .) %>%
    gsub("-", "_minus_", .) %>%
    gsub("90s", "Mins_Per_90", .) %>%
    gsub("__", "_", .)

  names(stat_df) <- new_names
  stat_df <- stat_df[-1,]

  stat_df <- stat_df %>% dplyr::select(-.data$Matches)

  remove_rows <- min(grep("Season", stat_df$Season)):nrow(stat_df)

  stat_df <- stat_df[-remove_rows, ]

  if(any(grepl("LgRank", names(stat_df)))){
    cols_to_transform <- stat_df %>%
      dplyr::select(-.data$Season, -.data$Squad, -.data$Country, -.data$Comp, -.data$LgRank) %>% names()
  } else {
    cols_to_transform <- stat_df %>%
      dplyr::select(-.data$Season, -.data$Squad, -.data$Country, -.data$Comp) %>% names()
  }

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


#' Clean stat table column names
#'
#' Returns cleaned column names for stats tables
#'
#' @param df_in a raw match stats data frame
#'
#' @return a data frame with cleaned names
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
.clean_table_names <- function(df_in) {
  var_names <- df_in[1,] %>% as.character()

  new_names <- paste(var_names, names(df_in), sep = "_")

  new_names <- new_names %>%
    gsub("\\..[0-9]", "", .) %>%
    gsub("\\.[0-9]", "", .) %>%
    gsub("\\.", "_", .) %>%
    gsub("_Var", "", .) %>%
    gsub("#", "Num_", .) %>%
    gsub("%", "_percent", .) %>%
    gsub("_Performance", "", .) %>%
    gsub("_Penalty", "", .) %>%
    gsub("1/3", "Final_Third", .) %>%
    gsub("\\+/-", "Plus_Minus", .) %>%
    gsub("/", "_per_", .) %>%
    gsub("-", "_minus_", .) %>%
    gsub("90s", "Mins_Per_90", .) %>%
    gsub("__", "_", .)

  names(df_in) <- new_names
  df_in <- df_in[-1,]

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
