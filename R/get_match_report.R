#' Get match report for webpage
#'
#' Internal function to return match report details for a selected match, with input being a webpage, not url
#'
#' @param match_page the fbref.com URL for the required match
#'
#' @return returns a dataframe with the match details for a selected match
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'

.get_match_report_page <- function(match_page) {

  # seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  Game_URL <- match_page %>% rvest::html_nodes(".langs") %>% rvest::html_node(".en") %>% rvest::html_attr("href")
  each_game_page <- tryCatch(match_page, error = function(e) NA)
  # tryCatch( {each_game_page <- xml2::read_html(match_url)}, error = function(e) {each_game_page <- NA})

  if(!is.na(each_game_page)) {
    game <- each_game_page %>% rvest::html_nodes("h1") %>% rvest::html_text()

    # tryCatch( {League <- each_game_page %>% rvest::html_nodes("h1+ div a") %>% rvest::html_text()}, error = function(e) {League <- NA})
    tryCatch( {League_URL <- each_game_page %>% rvest::html_nodes("h1+ div a") %>% rvest::html_attr("href") %>% paste0("https://fbref.com", .)}, error = function(e) {League <- NA})
    tryCatch( {Match_Date <- each_game_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[3]}, error = function(e) {Match_Date <- NA})
    tryCatch( {Matchweek <- each_game_page %>% rvest::html_nodes("h1+ div") %>% rvest::html_text()}, error = function(e) {Matchweek <- NA})

    tryCatch( {Home_Team <- each_game_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[1]}, error = function(e) {Home_Team <- NA})
    tryCatch( {Home_Formation <- each_game_page %>% rvest::html_nodes(".lineup#a") %>% rvest::html_nodes("th") %>% rvest::html_text() %>% .[1] %>% gsub(".*\\(", "", .) %>% gsub("\\)", "", .)}, error = function(e) {Home_Formation <- NA})
    tryCatch( {Home_Score <- each_game_page %>% rvest::html_nodes(".scores") %>% rvest::html_nodes(".score") %>% rvest::html_text() %>% .[1]}, error = function(e) {Home_Score <- NA})
    tryCatch( {Home_xG <- each_game_page %>% rvest::html_nodes(".scores") %>% rvest::html_nodes(".score_xg") %>% rvest::html_text() %>% .[1]}, error = function(e) {Home_xG <- NA})
    tryCatch( {Home_Goals <- each_game_page %>% rvest::html_nodes("#a") %>% .[[1]] %>% rvest::html_text()}, error = function(e) {Home_Goals <- NA}) %>%
      stringr::str_remove_all("\n") %>% stringr::str_remove_all("\t")
    tryCatch( {Home_Yellow_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% .[1] %>% rvest::html_nodes("span.yellow_card, span.yellow_red_card") %>% length()}, error = function(e) {Home_Yellow_Cards <- 0})
    tryCatch( {Home_Red_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% .[1] %>% rvest::html_nodes("span.red_card, span.yellow_red_card") %>% length()}, error = function(e) {Home_Red_Cards <- 0})

    tryCatch( {Away_Team <- each_game_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[2]}, error = function(e) {Away_Team <- NA})
    tryCatch( {Away_Formation <- each_game_page %>% rvest::html_nodes(".lineup#b") %>% rvest::html_nodes("th") %>% rvest::html_text() %>% .[1] %>% gsub(".*\\(", "", .) %>% gsub("\\)", "", .)}, error = function(e) {Away_Formation <- NA})
    tryCatch( {Away_Score <- each_game_page %>% rvest::html_nodes(".scores") %>% rvest::html_nodes(".score") %>%rvest:: html_text() %>% .[2]}, error = function(e) {Away_Score <- NA})
    tryCatch( {Away_xG <- each_game_page %>% rvest::html_nodes(".scores") %>% rvest::html_nodes(".score_xg") %>% rvest::html_text() %>% .[2]}, error = function(e) {Away_xG <- NA})
    tryCatch( {Away_Goals <- each_game_page %>% rvest::html_nodes("#b") %>% .[[1]] %>% rvest::html_text()}, error = function(e) {Away_Goals <- NA}) %>%
      stringr::str_remove_all("\n") %>% stringr::str_remove_all("\t")
    tryCatch( {Away_Yellow_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% .[2] %>% rvest::html_nodes("span.yellow_card, span.yellow_red_card") %>% length()}, error = function(e) {Away_Yellow_Cards <- 0})
    tryCatch( {Away_Red_Cards <- each_game_page %>% rvest::html_nodes(".cards") %>% .[2] %>% rvest::html_nodes("span.red_card, span.yellow_red_card") %>% length()}, error = function(e) {Away_Red_Cards <- 0})

    suppressWarnings(each_game <- cbind(League_URL, Match_Date, Matchweek, Home_Team, Home_Formation, Home_Score, Home_xG, Home_Goals, Home_Yellow_Cards, Home_Red_Cards, Away_Team, Away_Formation, Away_Score, Away_xG, Away_Goals, Away_Yellow_Cards, Away_Red_Cards, Game_URL) %>%
                       dplyr::as_tibble() %>%
                       dplyr::mutate(Home_Score = as.numeric(.data$Home_Score),
                                     Home_xG = as.numeric(.data$Home_xG),
                                     Away_Score = as.numeric(.data$Away_Score),
                                     Away_xG = as.numeric(.data$Away_xG))
    )


    # seasons <- seasons %>%
    #   dplyr::filter(.data$seasons_urls %in% each_game$League_URL) %>%
    #   dplyr::select(League=.data$competition_name, Gender=.data$gender, Country=.data$country, Season=.data$seasons)

    # each_game <- cbind(seasons, each_game) %>%
    #   dplyr::select(-.data$League_URL)
  } else {
    print(glue::glue("{Game_URL} is not available"))
    each_game <- data.frame()
  }

  return(each_game)
}



#' Get match report
#'
#' Returns match report details for selected matches
#'
#' @param match_url the fbref.com URL for the required match
#' @param time_pause the wait time (in seconds) between page loads
#'
#' @return returns a dataframe with the match details for a selected match
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' match <- get_match_urls(country = "AUS", gender = "F", season_end_year = 2021, tier = "1st")[1]
#' df <- get_match_report(match_url = match)
#' }

get_match_report <- function(match_url, time_pause=2) {

  time_wait <- time_pause

  each_match_report <- function(match_url, time_pause=time_wait) {
    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    match_page <- tryCatch(xml2::read_html(match_url), error = function(e) NA)

    if(!is.na(match_page)) {
      each_game <- .get_match_report_page(match_page)
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

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  seasons <- seasons %>%
    dplyr::filter(.data$seasons_urls %in% all_games$League_URL) %>%
    dplyr::select(League=.data$competition_name, Gender=.data$gender, Country=.data$country, Season=.data$seasons, League_URL=.data$seasons_urls)

  all_games <- seasons %>%
    dplyr::left_join(all_games, by = "League_URL") %>%
    dplyr::select(-.data$League_URL) %>% dplyr::distinct(.keep_all = T)

  return(all_games)
}
