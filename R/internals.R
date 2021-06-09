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

  if(any(grepl("Nation", colnames(df_in)))) {
    df_in$Nation <- gsub(".*? ", "", df_in$Nation)
  }

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


#' Clean Understat JSON data
#'
#' Returns a cleaned Understat data frame
#'
#' @param page_url understat.com page URL
#' @param script_name html JSON script name
#'
#' @return a cleaned Understat data frame
#'
#' @importFrom magrittr %>%
#'
.get_clean_understat_json <- function(page_url, script_name) {
  main_url <- "https://understat.com/"
  page <-  tryCatch( xml2::read_html(page_url), error = function(e) NA)

  if(!is.na(page)) {
    # locate script tags
    clean_json <- page %>% rvest::html_nodes("script") %>% as.character()
    clean_json <- clean_json[grep(script_name, clean_json)] %>% stringi::stri_unescape_unicode()
    clean_json <- qdapRegex::rm_square(clean_json, extract = TRUE, include.markers = TRUE) %>% unlist() %>% stringr::str_subset("\\[\\]", negate = TRUE)

    out_df <- lapply(clean_json, jsonlite::fromJSON) %>% do.call("rbind", .)
    # some outputs don't come with the season present, so add it in if not
    if(!any(grepl("season", colnames(out_df)))) {
      season <- page %>% rvest::html_nodes(xpath = '//*[@name="season"]') %>%
        rvest::html_nodes("option") %>% rvest::html_attr("value") %>% .[1] %>% as.numeric()
      out_df <- cbind(season, out_df)
    }

  } else {
    out_df <- data.frame()
  }

  out_df <- do.call(data.frame, out_df)

  return(out_df)

}


#' Understat shots location helper function
#'
#' Returns a cleaned Understat shooting location  data frame
#'
#' @param type_url can be season, team, match, player URL
#'
#' @return a cleaned Understat shooting location data frame
#'
#' @importFrom magrittr %>%
#' @importFrom stats runif
#'
.understat_shooting <- function(type_url) {
  main_url <- "https://understat.com/"
  # need to get the game IDs first, filtering out matches not yet played as these URLs will error
  games <-  .get_clean_understat_json(page_url = type_url, script_name = "datesData") %>%
    dplyr::filter(.data$isResult)
  # then create a chr vector of match URLs
  match_urls <- paste0(main_url, "match/", games$id)

  # start scrape:
  shots_data <- data.frame()

  for(each_match in match_urls) {
    Sys.sleep(round(runif(1, 1, 2)))
    tryCatch(df <- .get_clean_understat_json(page_url = each_match, script_name = "shotsData"), error = function(e) data.frame())
    if(nrow(df) == 0) {
      print(glue::glue("Shots data for match_url {each_match} not available"))
    }
    shots_data <- rbind(shots_data, df)

  }
  return(shots_data)
}


#' Clean date fields
#'
#' Returns a date format in YYYY-MM-DD from 'mmm d, yyyy'
#'
#' @param dirty_dates formatted date value
#'
#' @return a cleaned date
#'
#' @importFrom magrittr %>%
#'
.tm_fix_dates <- function(dirty_dates) {

  fix_date <- function(dirty_date) {
    if(is.na(dirty_date)) {
      clean_date <- NA_character_
    } else {
      split_string <- strsplit(dirty_date, split = " ") %>% unlist() %>% gsub(",", "", .)
      if(length(split_string) != 3) {
        clean_date <- NA_character_
      } else {
        tryCatch({clean_date <- lubridate::ymd(paste(split_string[3], split_string[1], split_string[2], sep = "-")) %>%
          as.character()}, error = function(e) {country_name <- NA_character_})
      }
    }

    return(clean_date)
  }
  clean_dates <- dirty_dates %>% purrr::map_chr(fix_date)

  return(clean_dates)
}
