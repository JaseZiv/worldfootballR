
.load_fotmob_leagues <- function() {
  read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/3c6ff713a08a0ef5f9355b8eba791a899fe68189/raw-data/fotmob-leagues/all_leagues.csv", stringsAsFactors = F)
}

#' @importFrom memoise memoise
.mem_load_fotmob_leagues <- memoise::memoise(.load_fotmob_leagues)

#' @importFrom memoise memoise
#' @importFrom purrr transpose map_dfr
#' @importFrom dplyr filter
.fotmob_get_league_ids <- function(league_id = NULL, country = NULL, league_name = NULL) {
  leagues <- .mem_load_fotmob_leagues()
  has_country <- !is.null(country)
  has_league_name <- !is.null(league_name)
  has_league_id <- !is.null(league_id)
  if(!has_league_id & !(has_country & has_league_name)) {
    stop(
      'Must provide `league_id` or both of `country` and `league_name`.'
    )
  }

  has_country_and_league_name <- has_country & has_league_name
  league_urls <- if(has_country_and_league_name) {
    n_country <- length(country)
    n_league_name <- length(league_name)
    if(n_country != n_league_name) {
      stop(
        sprintf(
          'If providing `country` and `league_name`, length of each must be the same (%s != %s).',
          n_country,
          n_league_name
        )
      )
    }

    pairs <- list(
      country = country,
      league_name = league_name
    ) %>%
      purrr::transpose()

    purrr::map_dfr(
      pairs,
      ~dplyr::filter(
        leagues,
        .data$ccode == .x$country, .data$name == .x$league_name
      )
    )
  } else {
    leagues %>%
      dplyr::filter(.data$id %in% league_id)
  }

  n_league_urls <- nrow(league_urls)
  if(n_league_urls == 0) {
    stop(
      'Could not find any leagues matching specified parameters.',
    )
  }

  n_params <- ifelse(
    has_country_and_league_name,
    n_country,
    length(league_id)
  )

  if(n_league_urls < n_params) {
    warning(
      sprintf(
        'Found less leagues than specified (%s < %s).',
        n_league_urls,
        n_params
      )
    )
  } else if (n_league_urls > n_params) {
    warning(
      sprintf(
        'Found more leagues than specified (%s > %s).',
        n_league_urls,
        n_params
      )
    )
  }
  league_urls$id
}

#' @importFrom jsonlite fromJSON
.fotmob_get_league_resp <- function(league_id) {
  main_url <- "https://www.fotmob.com/leagues?id="
  url <- sprintf("%s%s", main_url, league_id)
  jsonlite::fromJSON(url)
}

#' Get fotmob match results by league
#'
#' Returns match results for all matches played on the selected date from fotmob.com
#'
#' @param country Three character country code. Can be one or multiple. If provided, `league_name` must also be provided (of the same length)
#' @param league_name League names. If provided, `country` must also be provided (of the same length)
#' @param league_id Fotmob ID for the league. Only used if `country` and `league_name` are not specified.
#'
#' @return returns a dataframe of league matches
#'
#' @importFrom purrr possibly map_dfr
#' @importFrom tibble tibble
#' @importFrom rlang maybe_missing
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#'
#' # one league
#' fotmob_get_league_matches(
#'   country = "ENG",
#'   league_name = "Premier League"
#' )
#'
#' # one league, by id
#' fotmob_get_league_matches(
#'   league_id = 47
#' )
#'
#' # multiple leagues (could also use ids)
#' league_matches <- fotmob_get_league_matches(
#'   country =     c("ENG",            "ESP"   ),
#'   league_name = c("Premier League", "LaLiga")
#' )
#'
#' # probably the data that you care about
#' league_matches %>%
#'   dplyr::select(match_id = id, home, away) %>%
#'   tidyr::unnest_wider(c(home, away), names_sep = "_")
#' }
fotmob_get_league_matches <- function(country, league_name, league_id) {

  ids <- .fotmob_get_league_ids(
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL)
  )

  fp <- purrr::possibly(
    .fotmob_get_league_matches,
    quiet = FALSE,
    otherwise = tibble::tibble()
  )
  purrr::map_dfr(
    ids,
    .fotmob_get_league_matches
  )
}

#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
.fotmob_get_league_matches <- function(...) {
  resp <- .fotmob_get_league_resp(...)
  resp$fixtures %>%
    janitor::clean_names() %>%
    tibble::as_tibble()
}

#' Get standings from fotmob
#'
#' Returns league standings from fotmob.com. 3 types are returned: all, home, away
#'
#' @inheritParams fotmob_get_league_matches
#'
#' @return returns a dataframe of league standings
#'
#' @importFrom purrr possibly map_dfr
#' @importFrom tibble tibble
#' @importFrom rlang maybe_missing
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tidyr)
#'
#' # one league
#' fotmob_get_league_tables(
#'   country = "ENG",
#'   league_name = "Premier League"
#' )
#'
#' # one league, by id
#' fotmob_get_league_tables(
#'   league_id = 47
#' )
#'
#' # multiple leagues (could also use ids)
#' league_tables <- fotmob_get_league_tables(
#'   country =     c("ENG",            "ESP"   ),
#'   league_name = c("Premier League", "LaLiga")
#' )
#'
#' # look at tables if only away matches are considered
#' league_tables %>%
#'   dplyr::filter(table_type == "away")
#' }
fotmob_get_league_tables <- function(country, league_name, league_id) {

  ids <- .fotmob_get_league_ids(
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL)
  )

  fp <- purrr::possibly(
    .fotmob_get_league_tables,
    quiet = FALSE,
    otherwise = tibble::tibble()
  )
  purrr::map_dfr(
    ids,
    fp
  )
}

#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer unnest_longer unnest
.fotmob_get_league_tables <- function(...) {
  resp <- .fotmob_get_league_resp(...)
  table <- resp$tableData$table %>%
    janitor::clean_names() %>%
    tibble::as_tibble()
  table %>%
    tidyr::pivot_longer(
      colnames(.),
      names_to = "table_type",
      values_to = "table"
    ) %>%
    tidyr::unnest_longer(
      .data$table
    ) %>%
    tidyr::unnest(
      .data$table
    )
}
