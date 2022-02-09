#' Get fotmob match results by date
#'
#' Returns match results for all matches played on the selected date from fotmob.com
#'
#' @param dates a vector of string-formatted dates in "Ymd" format, e.g. "20210926". An attempt is
#' made to coerce the input to the necessary format if a date is passed in.
#'
#' @return returns a dataframe of match results
#'
#' @importFrom purrr map_dfr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#'
#' results <- fotmob_get_matches_by_date(date = c("20210925", "20210926"))
#' results %>%
#'   dplyr::select(primaryId, ccode, league_name = name, matches) %>%
#'   tidyr::unnest_longer(matches)
#' }
#'
fotmob_get_matches_by_date <- function(dates) {
  purrr::map_dfr(dates, .fotmob_get_matches_by_single_date)
}

#' @importFrom lubridate is.Date ymd
#' @importFrom stringr str_remove_all
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom purrr possibly
.fotmob_get_matches_by_single_date <- function(date) {
  print(glue::glue('Scraping match results data from fotmob for "{date}".'))
  main_url <- "https://www.fotmob.com/"

  is_date <- lubridate::is.Date(date)
  if(is_date) {
    date <- lubridate::ymd(date)
  }
  date <- stringr::str_remove_all(as.character(date), "-")
  url <- paste0(main_url, "matches?date=", date)
  f <- function(url) {
    resp <- jsonlite::fromJSON(url)
    resp$leagues
  }
  fp <- purrr::possibly(f, otherwise = data.frame())
  return(fp(url))
}

#' Get fotmob match details by match id
#'
#' Returns match details from fotmob.com
#'
#' @param match_ids a vector of strings or numbers representing matches
#'
#' @return returns a dataframe of match shots
#'
#' @importFrom purrr map_dfr
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#' results <- fotmob_get_matches_by_date(date = "20210926")
#' match_ids <- results %>%
#'   dplyr::select(primaryId, ccode, league_name = name, matches) %>%
#'   dplyr::filter(league_name == "Premier League", ccode == "ENG") %>%
#'   tidyr::unnest_longer(matches) %>%
#'   dplyr::pull(matches) %>%
#'   dplyr::pull(id)
#' match_ids # 3609987 3609979
#' details <- fotmob_get_match_details(match_ids)
#' }
fotmob_get_match_details <- function(match_ids) {
  purrr::map_dfr(match_ids, .fotmob_get_single_match_details)
}

#' @importFrom glue glue
#' @importFrom purrr possibly
#' @importFrom dplyr bind_cols mutate case_when
#' @importFrom rlang .data
.fotmob_get_single_match_details <- function(match_id) {
  print(glue::glue("Scraping match data from fotmob for match {match_id}."))
  main_url <- "https://www.fotmob.com/"
  url <- paste0(main_url, "matchDetails?matchId=", match_id)

  f <- function(url) {

    general <- .extract_fotmob_match_general(url)
    resp <- general$resp
    general_scalars <- general$scalars
    general_teams <- general$teams
    shots <- resp$content$shotmap$shots
    has_shots <- length(shots) > 0

    df <- dplyr::bind_cols(
      general_scalars,
      general_teams
    )

    if(has_shots) {
      df <-
        dplyr::bind_cols(
          df,
          shots
        ) %>%
        dplyr::mutate(
          team = dplyr::case_when(
            teamId == general$homeTeam$id ~ .data$homeTeam,
            teamId == general$awayTeam$id ~ .data$awayTeam,
            TRUE ~ NA_character_
          )
        )
    }
    df
  }

  fp <- purrr::possibly(f, otherwise = data.frame())
  return(fp(url))
}

#' @importFrom jsonlite fromJSON
.extract_fotmob_match_general <- function(url) {
  resp <- jsonlite::fromJSON(url)
  general <- resp$general
  scalars <- data.frame(
    stringsAsFactors = FALSE,
    matchId = general$matchId,
    matchRound = ifelse(is.null(general$matchRound), "", general$matchRound),
    leagueId = general$leagueId,
    leagueName = general$leagueName,
    leagueRoundName = general$leagueRoundName,
    parentLeagueId = general$parentLeagueId,
    parentLeagueSeason = general$parentLeagueSeason,
    matchTimeUTC = general$matchTimeUTC
  )
  teams <- data.frame(
    stringsAsFactors = FALSE,
    homeTeamId = unlist(general$homeTeam$id),
    homeTeam = unlist(general$homeTeam$name),
    homeTeamColor = unlist(general$teamColors$home),
    awayTeamId = unlist(general$awayTeam$id),
    awayTeam = unlist(general$awayTeam$name),
    awayTeamColor = unlist(general$teamColors$away)
  )
  list(
    resp = resp,
    scalars = general_scalars,
    teams = general_teams
  )
}

