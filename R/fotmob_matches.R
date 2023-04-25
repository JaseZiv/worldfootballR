.extract_fotmob_match_general <- function(url) {
  resp <- safely_get_content(url)$result
  general <- resp$general
  scalars <- data.frame(
    stringsAsFactors = FALSE,
    match_id = general$matchId, ## don't technically need this since `.fotmob_get_single_match_details` is wrapped with `.wrap_fotmob_match_id_f`
    match_round = ifelse(is.null(general$matchRound), "", general$matchRound),
    league_id = general$leagueId,
    league_name = general$leagueName,
    league_round_name = general$leagueRoundName,
    parent_league_id = general$parentLeagueId,
    parent_league_season = general$parentLeagueSeason,
    match_time_utc = general$matchTimeUTC
  )
  colors <- general$teamColor
  teams <- data.frame(
    stringsAsFactors = FALSE,
    home_team_id = unlist(general$homeTeam$id),
    home_team = unlist(general$homeTeam$name),
    home_team_color = colors[1, "color"],
    away_team_id = unlist(general$awayTeam$id),
    away_team = unlist(general$awayTeam$name),
    away_team_color = colors[2, "color"]
  )
  list(
    resp = resp,
    scalars = scalars,
    teams = teams
  )
}

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
#' \donttest{
#' try({
#' library(dplyr)
#' library(tidyr)
#'
#' results <- fotmob_get_matches_by_date(date = c("20210925", "20210926"))
#' results %>%
#'   dplyr::select(primary_id, ccode, league_name = name, match_id)
#' })
#' }
#'
fotmob_get_matches_by_date <- function(dates) {
  purrr::map_dfr(dates, .fotmob_get_matches_by_single_date)
}

#' @importFrom lubridate is.Date ymd
#' @importFrom stringr str_remove_all
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom purrr possibly
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr unnest
#' @importFrom dplyr rename select
#' @importFrom tidyselect vars_select_helpers
#' @importFrom magrittr %>%
.fotmob_get_matches_by_single_date <- function(date) {
  # CRAN feedback was to remove this from the existing functions so I have for now
  # print(glue::glue('Scraping match results data from fotmob for "{date}".'))

  main_url <- "https://www.fotmob.com/api/"

  f <- function(date) {

    orig_date <- date
    is_date <- lubridate::is.Date(date)
    if(is_date) {
      date <- lubridate::ymd(date)
    }

    date <- stringr::str_remove_all(as.character(date), "-")

    url <- paste0(main_url, "matches?date=", date)

    resp <- safely_get_content(url)$result

    res <- resp$leagues
    if(is.null(res)) {
      stop(sprintf('Couldn\'t find match data for `date = "%s"`.', orig_date))
      return(res)
    }

    res %>%
      janitor::clean_names() %>%
      tibble::as_tibble() %>%
      dplyr::rename(match = .data[["matches"]]) %>%
      tidyr::unnest(.data[["match"]], names_sep = "_") %>%
      dplyr::rename(home = .data[["match_home"]], away = .data[["match_away"]]) %>%
      tidyr::unnest(c(.data[["home"]], .data[["away"]], .data[["match_status"]]), names_sep = "_") %>%
      dplyr::select(-tidyselect::vars_select_helpers$where(is.list)) %>%
      janitor::clean_names()
  }
  fp <- purrr::possibly(
    f,
    otherwise = tibble::tibble(),
    quiet = FALSE
  )
  fp(date)
}

#' Get fotmob match details by match id
#'
#' Returns match details from fotmob.com
#'
#' @param match_ids a vector of strings or numbers representing matches
#'
#' @return returns a dataframe of match shots
#'
#' @examples
#' \donttest{
#' try({
#' library(dplyr)
#' library(tidyr)
#' results <- fotmob_get_matches_by_date(date = "20210926")
#' match_ids <- results %>%
#'   dplyr::select(primary_id, ccode, league_name = name, match_id) %>%
#'   dplyr::filter(league_name == "Premier League", ccode == "ENG") %>%
#'   dplyr::pull(match_id)
#' match_ids # 3609987 3609979
#' details <- fotmob_get_match_details(match_ids)
#' })
#' }
#' @export
fotmob_get_match_details <- function(match_ids) {
  .wrap_fotmob_match_f(match_ids, .fotmob_get_single_match_details)
}

#' @importFrom glue glue
#' @importFrom purrr possibly
#' @importFrom dplyr bind_cols mutate case_when
#' @importFrom janitor clean_names
#' @importFrom rlang .data
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr unnest unnest_wider
.fotmob_get_single_match_details <- function(match_id) {
  # CRAN feedback was to remove this from the existing functions so I have for now
  # print(glue::glue("Scraping match data from fotmob for match {match_id}."))
  main_url <- "https://www.fotmob.com/api/"
  url <- paste0(main_url, "matchDetails?matchId=", match_id)

  f <- function(url) {

    general <- .extract_fotmob_match_general(url)
    df <- dplyr::bind_cols(
      general$scalars,
      general$teams
    )
    df <- tibble::as_tibble(df)
    shots <- general$resp$content$shotmap$shots
    has_shots <- length(shots) > 0

    if(isTRUE(has_shots)) {
      shots <- janitor::clean_names(shots)
      df$shots <- list(shots)
      df <- df %>%
        tidyr::unnest(.data[["shots"]])  %>%
        tidyr::unnest_wider(.data[["on_goal_shot"]], names_sep = "_")  %>%
        janitor::clean_names()
    } else {
      df$shots <- NULL
    }
    df
  }

  fp <- purrr::possibly(
    f,
    quiet = FALSE,
    otherwise = tibble::tibble()
  )
  fp(url)
}

#' Get fotmob match top team stats by match id
#'
#' Returns match top team stats from fotmob.com
#'
#' @param match_ids a vector of strings or numbers representing matches
#'
#' @return returns a dataframe of match top team stats
#'
#' @examples
#' \donttest{
#' try({
#' library(dplyr)
#' library(tidyr)
#' results <- fotmob_get_matches_by_date(date = "20210926")
#' match_ids <- results %>%
#'   dplyr::select(primary_id, ccode, league_name = name, match_id) %>%
#'   dplyr::filter(league_name == "Premier League", ccode == "ENG") %>%
#'   dplyr::pull(match_id)
#' match_ids # 3609987 3609979
#' details <- fotmob_get_match_team_stats(match_ids)
#' })
#' }
#' @export
fotmob_get_match_team_stats <- function(match_ids) {
  .wrap_fotmob_match_f(match_ids, .fotmob_get_single_match_team_stats)
}

#' @importFrom purrr possibly
#' @importFrom dplyr bind_cols mutate across rename
#' @importFrom janitor clean_names
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest unnest_wider hoist
.fotmob_get_single_match_team_stats <- function(match_id) {

  main_url <- "https://www.fotmob.com/api/"
  url <- paste0(main_url, "matchDetails?matchId=", match_id)

  f <- function(url) {

    general <- .extract_fotmob_match_general(url)
    df <- dplyr::bind_cols(
      general$scalars,
      general$teams
    )

    stats <- general$resp$content$stats$stats %>% janitor::clean_names()
    has_stats <- length(stats) > 0
    df <- tibble::as_tibble(df)
    if(isTRUE(has_stats)) {
      wide_stats <- stats  %>%
        tidyr::unnest_wider(stats, names_sep = '_')  %>%
        tidyr::unnest(vars_select_helpers$where(is.list))
      clean_stats <- wide_stats  %>%
        tidyr::hoist(
          .data[["stats_stats"]],
          "home_value" = 1
        )  %>%
        dplyr::rename("away_value" = .data[["stats_stats"]])  %>%
        dplyr::mutate(
          dplyr::across(c(.data[["home_value"]], .data[["away_value"]]), as.character)
        )  %>%
        tidyr::unnest(c(.data[["home_value"]], .data[["away_value"]]))
      df <- dplyr::bind_cols(df, clean_stats)
    }
    df
  }

  fp <- purrr::possibly(
    f,
    quiet = FALSE,
    otherwise = tibble::tibble()
  )
  fp(url)
}

#' Get fotmob match info by match id
#'
#' Returns match info from fotmob.com
#'
#' @param match_ids a vector of strings or numbers representing matches
#'
#' @return returns a dataframe of match info
#'
#' @examples
#' \donttest{
#' try({
#' library(dplyr)
#' library(tidyr)
#' results <- fotmob_get_matches_by_date(date = "20210926")
#' match_ids <- results %>%
#'   dplyr::select(primary_id, ccode, league_name = name, match_id) %>%
#'   dplyr::filter(league_name == "Premier League", ccode == "ENG") %>%
#'   dplyr::pull(match_id)
#' match_ids # 3609987 3609979
#' details <- fotmob_get_match_info(match_ids)
#' })
#' }
#' @export
fotmob_get_match_info <- function(match_ids) {
  .wrap_fotmob_match_f(match_ids, .fotmob_get_single_match_info)
}

#' @importFrom purrr possibly
#' @importFrom dplyr bind_cols
#' @importFrom janitor clean_names
#' @importFrom rlang .data
#' @importFrom tibble tibble enframe
#' @importFrom tidyr pivot_wider unnest unnest_wider
.fotmob_get_single_match_info <- function(match_id) {
  main_url <- "https://www.fotmob.com/api/"

  f <- function(match_id) {

    url <- paste0(main_url, "matchDetails?matchId=", match_id)

    general <- .extract_fotmob_match_general(url)
    df <- dplyr::bind_cols(
      general$scalars,
      general$teams
    )

    info <- general$resp$content$matchFacts$infoBox

    unnested_info <- info %>%
      tibble::enframe() %>%
      tidyr::pivot_wider(
        names_from = .data[["name"]],
        values_from = .data[["value"]]
      ) %>%
      janitor::clean_names() %>%
      tidyr::unnest_wider(
        c(tidyselect::vars_select_helpers$where(is.list), -tidyselect::vars_select_helpers$any_of("attendance")),
        names_sep = "_"
      ) %>%
      tidyr::unnest(
        tidyselect::vars_select_helpers$any_of("attendance")
      ) %>%
      janitor::clean_names()

    if (nrow(df) != 1) {
      stop(sprintf("Could not find match info for `match_id = %s`", match_id))
      return(tibble::tibble())
    }

    dplyr::bind_cols(df, unnested_info)
  }

  fp <- purrr::possibly(
    f,
    quiet = FALSE,
    otherwise = tibble::tibble()
  )
  fp(match_id)
}


#' Get fotmob match momentum
#'
#' Returns match momentum from fotmob.com. Only available for 2022/23 season and beyond.
#'
#' @inheritParams fotmob_get_match_info
#'
#' @return returns a dataframe of match momentum
#' @importFrom rlang arg_match
#' @examples
#' \donttest{
#' try({
#' match_id <- 3901251
#' fotmob_get_match_info(match_id)
#' })
#' }
#' @export
fotmob_get_match_momentum <- function(match_ids) {
  .wrap_fotmob_match_f(match_ids, .fotmob_get_single_match_momentum)
}

#' @importFrom rlang .data .env
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr unnest
#' @importFrom dplyr bind_rows
#' @importFrom purrr possibly
#' @importFrom janitor clean_names
.fotmob_get_single_match_momentum <- function(match_id, type) {

  main_url <- "https://www.fotmob.com/api/"
  url <- paste0(main_url, "matchDetails?matchId=", match_id)
  resp <- safely_get_content(url)
  if (!is.null(resp$error)) {
    stop(
      sprintf("Error with `match_id = %s` (%s). Error:\n", match_id, url, resp$error)
    )
  }
  not_has_momentum <- length(resp$result$content$momentum) == 1 & isFALSE(resp$result$content$momentum)
  if (isTRUE(not_has_momentum)) {
    stop(
      sprintf("No momentum data for `match_id = %s.", match_id)
    )
  }
  momentum <- resp$result$content$momentum
  main <- tidyr::unnest(dplyr::bind_rows(momentum$main), "data")
  main$type <- "main"
  alt <-tidyr::unnest(dplyr::bind_rows(momentum$alternateModels), "data")
  alt$type <- "alternateModels"
  res <- dplyr::bind_rows(
    main,
    alt
  )
  res <- janitor::clean_names(res)
  tibble::as_tibble(res)
}
