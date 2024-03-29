#' Get match team stats for webpage
#'
#' Internal function to return match team stats for a selected match, with input being a webpage, not url
#'
#' @param match_page the fbref.com URL for the required match
#'
#' @return returns a dataframe with the match team stats for a selected match
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @noRd
#'
.get_team_match_stats <- function(match_page) {

  Game_URL <- match_page %>% rvest::html_nodes(".langs") %>% rvest::html_node(".en") %>% rvest::html_attr("href")
  each_game_page <- tryCatch(match_page, error = function(e) NA)

  if(!is.na(each_game_page)) {
    team_stats <- each_game_page %>% rvest::html_nodes("#team_stats") %>% rvest::html_nodes("tr") %>% rvest::html_text()
    team_stats_extra <- each_game_page %>% rvest::html_nodes("#team_stats_extra") %>% rvest::html_text()

    tryCatch( {League <- each_game_page %>% rvest::html_nodes("h1+ div a:nth-child(1)") %>% rvest::html_text()}, error = function(e) {League <- NA})
    tryCatch( {Match_Date <- each_game_page %>% rvest::html_nodes(".venuetime") %>% rvest::html_attr("data-venue-date")}, error = function(e) {Match_Date <- NA})
    tryCatch( {Matchweek <- each_game_page %>% rvest::html_nodes("h1+ div") %>% rvest::html_text()}, error = function(e) {Matchweek <- NA})

    tryCatch( {Home_Team <- each_game_page %>% rvest::html_nodes("div+ strong a") %>% rvest::html_text() %>% .[1]}, error = function(e) {Home_Team <- NA})
    tryCatch( {Home_Formation <- each_game_page %>% rvest::html_nodes(".lineup#a") %>% rvest::html_nodes("th") %>% rvest::html_text() %>% .[1] %>% gsub(".*\\(", "", .) %>% gsub("\\)", "", .)}, error = function(e) {Home_Formation <- NA})
    tryCatch( {Home_Possession <- team_stats %>% .[3] %>% gsub("[\n\t]", "", .) %>% strsplit(., '%') %>%  unlist(.) %>% .[1]}, error = function(e) {Home_Possession <- NA})
    tryCatch( {Home_Success_Pass <- team_stats %>% .[5] %>% gsub("[\n\t]", "", .) %>% regmatches(., regexpr('.+(?=%\\d{1,3})', ., perl = TRUE)) %>% regmatches(., regexpr('.+(?= of)', ., perl = TRUE))}, error = function(e) {Home_Success_Pass <- NA})
    tryCatch( {Home_Pass <- team_stats %>% .[5] %>% gsub("[\n\t]", "", .) %>% regmatches(., regexpr('.+(?=%\\d{1,3})', ., perl = TRUE)) %>% regmatches(., regexpr('(?<=of )\\d{1,4}', ., perl = TRUE))}, error = function(e) {Home_Pass <- NA})
    tryCatch( {Home_Passing_Accuracy <- team_stats %>% .[5] %>% gsub("[\n\t]", "", .) %>% regmatches(., regexpr('.+(?=%\\d{1,3})', ., perl = TRUE)) %>% regmatches(., regexpr('\\d{1,3}$', ., perl = TRUE))}, error = function(e) {Home_Passing_Accuracy <- NA})
    tryCatch( {Home_Fouls <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Fouls)', ., perl = TRUE))}, error = function(e) {Home_Fouls <- NA})
    tryCatch( {Home_Corners <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Corners)', ., perl = TRUE))}, error = function(e) {Home_Corners <- NA})
    tryCatch( {Home_Crosses <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Crosses)', ., perl = TRUE))}, error = function(e) {Home_Crosses <- NA})
    tryCatch( {Home_Touches <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Touches)', ., perl = TRUE))}, error = function(e) {Home_Touches<- NA})
    tryCatch( {Home_Tackles <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Tackles)', ., perl = TRUE))}, error = function(e) {Home_Tackles <- NA})
    tryCatch( {Home_Interceptions <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Interceptions)', ., perl = TRUE))}, error = function(e) {Home_Interceptions <- NA})
    tryCatch( {Home_Aerials_Won <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Aerials Won)', ., perl = TRUE))}, error = function(e) {Home_Aerials_Won <- NA})
    tryCatch( {Home_Clearances <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Clearances)', ., perl = TRUE))}, error = function(e) {Home_Clearances <- NA})
    tryCatch( {Home_Offsides <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Offsides)', ., perl = TRUE))}, error = function(e) {Home_Offsides <- NA})
    tryCatch( {Home_Goal_Kicks <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Goal Kicks)', ., perl = TRUE))}, error = function(e) {Home_Goal_Kicks <- NA})
    tryCatch( {Home_Throw_Ins <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Throw Ins)', ., perl = TRUE))}, error = function(e) {Home_Throw_Ins <- NA})
    tryCatch( {Home_Long_Balls <- team_stats_extra %>% regmatches(., regexpr('\\d{1,4}(?=Long Balls)', ., perl = TRUE))}, error = function(e) {Home_Long_Balls <- NA})
    tryCatch( {Home_Yellow_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% .[1] %>% rvest::html_nodes("span.yellow_card, span.yellow_red_card") %>% length()}, error = function(e) {Home_Yellow_Cards <- 0})
    tryCatch( {Home_Red_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% .[1] %>% rvest::html_nodes("span.red_card, span.yellow_red_card") %>% length()}, error = function(e) {Home_Red_Cards <- 0})

    tryCatch( {Away_Team <- each_game_page %>% rvest::html_nodes("div+ strong a") %>% rvest::html_text() %>% .[2]}, error = function(e) {Away_Team <- NA})
    tryCatch( {Away_Formation <- each_game_page %>% rvest::html_nodes(".lineup#b") %>% rvest::html_nodes("th") %>% rvest::html_text() %>% .[1] %>% gsub(".*\\(", "", .) %>% gsub("\\)", "", .)}, error = function(e) {Away_Formation <- NA})
    tryCatch( {Away_Possession <- team_stats %>% .[3] %>% gsub("[\n\t]", "", .) %>% strsplit(., '%') %>%  unlist(.) %>% .[2]}, error = function(e) {Away_Possession <- NA})
    tryCatch( {Away_Success_Pass <- team_stats %>% .[5] %>% gsub("[\n\t]", "", .) %>% regmatches(., regexpr('(?<=%).+', ., perl = TRUE)) %>% regmatches(., regexpr('\\d{1,4}(?= of)', ., perl = TRUE))}, error = function(e) {Away_Success_Pass <- NA})
    tryCatch( {Away_Pass <- team_stats %>% .[5] %>% gsub("[\n\t]", "", .) %>% regmatches(., regexpr('(?<=%).+', ., perl = TRUE)) %>% regmatches(., regexpr('(?<=of )\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Pass <- NA})
    tryCatch( {Away_Passing_Accuracy <- team_stats %>% .[5] %>% gsub("[\n\t]", "", .) %>% regmatches(., regexpr('(?<=%).+', ., perl = TRUE)) %>% regmatches(., regexpr('\\d{1,3}(?=%)', ., perl = TRUE))}, error = function(e) {Away_Passing_Accuracy <- NA})
    tryCatch( {Away_Fouls <- team_stats_extra %>% regmatches(., regexpr('(?<=Fouls)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Fouls <- NA})
    tryCatch( {Away_Corners <- team_stats_extra %>% regmatches(., regexpr('(?<=Corners)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Corners <- NA})
    tryCatch( {Away_Crosses <- team_stats_extra %>% regmatches(., regexpr('(?<=Crosses)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Crosses <- NA})
    tryCatch( {Away_Touches <- team_stats_extra %>% regmatches(., regexpr('(?<=Touches)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Touches <- NA})
    tryCatch( {Away_Tackles <- team_stats_extra %>% regmatches(., regexpr('(?<=Tackles)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Tackles <- NA})
    tryCatch( {Away_Interceptions <- team_stats_extra %>% regmatches(., regexpr('(?<=Interceptions)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Interceptions <- NA})
    tryCatch( {Away_Aerials_Won <- team_stats_extra %>% regmatches(., regexpr('(?<=Aerials Won)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Aerials_Won <- NA})
    tryCatch( {Away_Clearances <- team_stats_extra %>% regmatches(., regexpr('(?<=Clearances)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Clearances <- NA})
    tryCatch( {Away_Offsides <- team_stats_extra %>% regmatches(., regexpr('(?<=Offsides)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Offsides <- NA})
    tryCatch( {Away_Goal_Kicks <- team_stats_extra %>% regmatches(., regexpr('(?<=Goal Kicks)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Goal_Kicks <- NA})
    tryCatch( {Away_Throw_Ins <- team_stats_extra %>% regmatches(., regexpr('(?<=Throw Ins)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Throw_Ins <- NA})
    tryCatch( {Away_Long_Balls <- team_stats_extra %>% regmatches(., regexpr('(?<=Long Balls)\\d{1,4}', ., perl = TRUE))}, error = function(e) {Away_Long_Balls <- NA})
    tryCatch( {Away_Yellow_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% .[2] %>% rvest::html_nodes("span.yellow_card, span.yellow_red_card") %>% length()}, error = function(e) {Away_Yellow_Cards <- 0})
    tryCatch( {Away_Red_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% .[2] %>% rvest::html_nodes("span.red_card, span.yellow_red_card") %>% length()}, error = function(e) {Away_Red_Cards <- 0})

    suppressWarnings(each_game <- cbind(League, Match_Date, Matchweek, Home_Team, Home_Formation, Home_Possession, Home_Success_Pass, Home_Pass, Home_Passing_Accuracy, Home_Fouls, Home_Corners, Home_Crosses, Home_Touches, Home_Tackles,
                                        Home_Interceptions, Home_Aerials_Won, Home_Clearances, Home_Offsides, Home_Goal_Kicks, Home_Throw_Ins, Home_Long_Balls, Home_Yellow_Cards, Home_Red_Cards,
                                        Away_Team, Away_Formation, Away_Possession, Away_Success_Pass, Away_Pass, Away_Passing_Accuracy, Away_Fouls, Away_Corners, Away_Crosses, Away_Touches, Away_Tackles, Away_Interceptions, Away_Aerials_Won,
                                        Away_Clearances, Away_Offsides, Away_Goal_Kicks, Away_Throw_Ins, Away_Long_Balls, Away_Yellow_Cards, Away_Red_Cards, Game_URL) %>%
                       dplyr::as_tibble() %>%
                       dplyr::mutate(Home_Possession = as.numeric(.data[["Home_Possession"]]),
                                     Home_Success_Pass = as.numeric(.data[["Home_Success_Pass"]]),
                                     Home_Pass = as.numeric(.data[["Home_Pass"]]),
                                     Home_Passing_Accuracy = as.numeric(.data[["Home_Passing_Accuracy"]]),
                                     Home_Fouls = as.numeric(.data[["Home_Fouls"]]),
                                     Home_Corners = as.numeric(.data[["Home_Corners"]]),
                                     Home_Crosses = as.numeric(.data[["Home_Crosses"]]),
                                     Home_Touches = as.numeric(.data[["Home_Touches"]]),
                                     Home_Tackles = as.numeric(.data[["Home_Tackles"]]),
                                     Home_Interceptions = as.numeric(.data[["Home_Interceptions"]]),
                                     Home_Aerials_Won = as.numeric(.data[["Home_Aerials_Won"]]),
                                     Home_Clearances = as.numeric(.data[["Home_Clearances"]]),
                                     Home_Offsides = as.numeric(.data[["Home_Offsides"]]),
                                     Home_Goal_Kicks = as.numeric(.data[["Home_Goal_Kicks"]]),
                                     Home_Throw_Ins = as.numeric(.data[["Home_Throw_Ins"]]),
                                     Home_Long_Balls = as.numeric(.data[["Home_Long_Balls"]]),
                                     Away_Possession = as.numeric(.data[["Away_Possession"]]),
                                     Away_Success_Pass = as.numeric(.data[["Away_Success_Pass"]]),
                                     Away_Pass = as.numeric(.data[["Away_Pass"]]),
                                     Away_Passing_Accuracy = as.numeric(.data[["Away_Passing_Accuracy"]]),
                                     Away_Fouls = as.numeric(.data[["Away_Fouls"]]),
                                     Away_Corners = as.numeric(.data[["Away_Corners"]]),
                                     Away_Crosses = as.numeric(.data[["Away_Crosses"]]),
                                     Away_Touches = as.numeric(.data[["Away_Touches"]]),
                                     Away_Tackles = as.numeric(.data[["Away_Tackles"]]),
                                     Away_Interceptions = as.numeric(.data[["Away_Interceptions"]]),
                                     Away_Aerials_Won = as.numeric(.data[["Away_Aerials_Won"]]),
                                     Away_Clearances = as.numeric(.data[["Away_Clearances"]]),
                                     Away_Offsides = as.numeric(.data[["Away_Offsides"]]),
                                     Away_Goal_Kicks = as.numeric(.data[["Away_Goal_Kicks"]]),
                                     Away_Throw_Ins = as.numeric(.data[["Away_Throw_Ins"]]),
                                     Away_Long_Balls = as.numeric(.data[["Away_Long_Balls"]]))
    )


  } else {
    print(glue::glue("{Game_URL} is not available"))
    each_game <- data.frame()
  }

  return(each_game)
}

#' Get FBref match team stats
#'
#' Returns match team stats for selected matches.
#'
#' @param match_url the fbref.com URL for the required match
#' @param time_pause the wait time (in seconds) between page loads
#'
#' @return returns a dataframe with the match team stats for a selected match
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' match <- fb_match_urls(country = "AUS", gender = "F", season_end_year = 2021, tier = "1st")[1]
#' df <- fb_team_match_stats(match_url = match)
#' })
#' }

fb_team_match_stats <- function(match_url, time_pause=3) {

  time_wait <- time_pause

  each_match_report <- function(match_url, time_pause=time_wait) {
    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    match_page <- tryCatch(.load_page(match_url), error = function(e) NA)

    if(!is.na(match_page)) {
      each_game <- .get_team_match_stats(match_page)
    } else {
      print(glue::glue("{match_url} is not available"))
      each_game <- data.frame()
    }
    return(each_game)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(match_url))

  all_games <- match_url %>%
    purrr::map_df(each_match_report)

  return(all_games)
}
