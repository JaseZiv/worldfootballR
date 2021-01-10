#' Get match report
#'
#' Returns match report details for a selected match
#'
#' @param match_url the fbref.com URL for the required match
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
#' match <- "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League"
#' get_match_report(match_url = match)
#' }

get_match_report <- function(match_url) {

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/league_seasons/all_tier1_season_URLs.csv")

  get_each_match_report <- function(match_url) {
    Game_URL <- match_url
    each_game_page <- NA
    tryCatch( {each_game_page <- xml2::read_html(match_url)}, error = function(e) {each_game_page <- NA})

    if(is.na(each_game_page)) stop("Invalid matchday URL")

    game <- each_game_page %>% rvest::html_nodes("h1") %>% rvest::html_text()

    print(glue::glue("Scraping {game}"))

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

    tryCatch( {Away_Team <- each_game_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[2]}, error = function(e) {Away_Team <- NA})
    tryCatch( {Away_Formation <- each_game_page %>% rvest::html_nodes(".lineup#b") %>% rvest::html_nodes("th") %>% rvest::html_text() %>% .[1] %>% gsub(".*\\(", "", .) %>% gsub("\\)", "", .)}, error = function(e) {Away_Formation <- NA})
    tryCatch( {Away_Score <- each_game_page %>% rvest::html_nodes(".scores") %>% rvest::html_nodes(".score") %>%rvest:: html_text() %>% .[2]}, error = function(e) {Away_Score <- NA})
    tryCatch( {Away_xG <- each_game_page %>% rvest::html_nodes(".scores") %>% rvest::html_nodes(".score_xg") %>% rvest::html_text() %>% .[2]}, error = function(e) {Away_xG <- NA})
    tryCatch( {Away_Goals <- each_game_page %>% rvest::html_nodes("#b") %>% .[[1]] %>% rvest::html_text()}, error = function(e) {Away_Goals <- NA}) %>%
      stringr::str_remove_all("\n") %>% stringr::str_remove_all("\t")


    each_game <- cbind(League_URL, Match_Date, Matchweek, Home_Team, Home_Formation, Home_Score, Home_xG, Home_Goals, Away_Team, Away_Formation, Away_Score, Away_xG, Away_Goals, Game_URL) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(Home_Score = as.numeric(.data$Home_Score),
                    Home_xG = as.numeric(.data$Home_xG),
                    Away_Score = as.numeric(.data$Away_Score),
                    Away_xG = as.numeric(.data$Away_xG))

    seasons <- seasons %>%
      dplyr::filter(.data$seasons_urls %in% each_game$League_URL) %>%
      dplyr::select(League=.data$competition_name, Gender=.data$gender, Country=.data$country, Season=.data$seasons)

    each_game <- cbind(seasons, each_game) %>%
      dplyr::select(-.data$League_URL)


    return(each_game)
  }

  all_games <- match_url %>%
    purrr::map_df(get_each_match_report)

  return(all_games)
}
