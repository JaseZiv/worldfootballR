#' Get match lineups
#'
#' Returns lineups for home and away teams for a selected match
#'
#' @param match_url the fbref.com URL for the required match
#'
#' @return returns a dataframe with the team lineups for a selected match
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' match <- "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League"
#' get_match_lineups(match_url = match)
#' }

get_match_lineups <- function(match_url) {
  match_page <- xml2::read_html(match_url)

  game <- match_page %>% rvest::html_nodes("h1") %>% rvest::html_text()

  cat(glue::glue("Scraping lineups for {game}"))

  lineups <- match_page %>% rvest::html_nodes(".lineup") %>% rvest::html_nodes("table")

  home <- 1
  away <- 2

  get_each_lineup <- function(home_away) {
    lineup <- lineups[home_away] %>% rvest::html_table() %>% data.frame()
    formation <- names(lineup)[1] %>% gsub(".*\\.\\.", "", .) %>% stringr::str_extract_all(., "[[:digit:]]") %>% unlist() %>% paste(collapse = "-")
    tryCatch( {team <- match_page %>% rvest::html_nodes("div:nth-child(1) div strong a") %>% rvest::html_text() %>% .[home_away]}, error = function(e) {team <- NA})

    bench_index <- which(lineup[,1] == "Bench")

    lineup <- lineup[1:(bench_index-1),] %>% dplyr::mutate(Starting = "Pitch") %>%
      dplyr::bind_rows(
        lineup[(bench_index+1):nrow(lineup),] %>% dplyr::mutate(Starting = "Bench")
      )

    lineup <- lineup %>%
      dplyr::mutate(Matchday = game,
             Team = team,
             Formation = formation)

    names(lineup) <- c("Player_Num", "Player_Name", "Starting", "Matchday", "Team", "Formation")

    lineup <- lineup %>%
      dplyr::select(.data$Matchday, .data$Team, .data$Formation, .data$Player_Num, .data$Player_Name, .data$Starting)

    return(lineup)
  }

  all_lineup <- c(home, away) %>%
    purrr::map_df(get_each_lineup)

  return(all_lineup)
}
