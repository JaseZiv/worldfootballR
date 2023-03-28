
## TODO: Cache this.
.fotmob_load_csv <- function(suffix) {
  read.csv(
    sprintf("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/%s", suffix),
    stringsAsFactors = FALSE
  )
}

#' @importFrom purrr transpose map_dfr
#' @importFrom dplyr filter
.fotmob_get_league_urls <- function(
    leagues,
    league_id = NULL,
    country = NULL,
    league_name = NULL
) {
  has_country <- !is.null(country)
  has_league_name <- !is.null(league_name)
  has_league_id <- !is.null(league_id)
  if(!has_league_id & !(has_country & has_league_name)) {
    stop(
      "Must provide `league_id` or both of `country` and `league_name`."
    )
  }

  has_country_and_league_name <- has_country & has_league_name
  urls <- if(has_country_and_league_name) {
    n_country <- length(country)
    n_league_name <- length(league_name)
    if(n_country != n_league_name) {
      stop(
        sprintf(
          "If providing `country` and `league_name`, length of each must be the same (%s != %s).",
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
        .data[["ccode"]] == .x$country, .data[["name"]] == .x$league_name
      )
    )
  } else {
    leagues %>%
      dplyr::filter(.data[["id"]] %in% league_id)
  }

  n_urls <- nrow(urls)
  if(n_urls == 0) {
    stop(
      "Could not find any leagues matching specified parameters."
    )
  }

  n_params <- ifelse(
    has_country_and_league_name,
    n_country,
    length(league_id)
  )

  if(n_urls < n_params) {
    warning(
      sprintf(
        "Found less leagues than specified (%s < %s).",
        n_urls,
        n_params
      )
    )
  } else if (n_urls > n_params) {
    warning(
      sprintf(
        "Found more leagues than specified (%s > %s).",
        n_urls,
        n_params
      )
    )
  }
  urls
}


#' Get fotmob league ids
#'
#' Returns a dataframe of the league ids available on fotmob
#'
#' @param cached Whether to load the dataframe from the \href{https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/fotmob-leagues/all_leagues.csv}{data CSV}. This is faster and most likely what you want to do, unless you identify a league that's being tracked by fotmob that's not in this pre-saved CSV.
#'
#' @importFrom httr POST content
#' @importFrom purrr map_dfr
#' @importFrom tibble enframe
#' @importFrom dplyr rename
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom janitor clean_names
#' @export
fotmob_get_league_ids <- function(cached = TRUE) {
  if(cached) {
    return(.fotmob_load_csv("fotmob-leagues/all_leagues.csv"))
  }

  resp <- httr::POST("https://www.fotmob.com/api/allLeagues")
  cont <- resp %>% httr::content()

  .extract_leagues <- function(x) {
    cont[[x]] %>%
      tibble::enframe() %>%
      dplyr::select(.data[["value"]]) %>%
      tidyr::unnest_wider(.data[["value"]]) %>%
      tidyr::unnest_longer(.data[["leagues"]]) %>%
      dplyr::rename(
        country = .data[["name"]]
      ) %>%
      tidyr::unnest_wider(.data[["leagues"]]) %>%
      janitor::clean_names()
  }

  purrr::map_dfr(
    c(
      "international",
      "countries"
    ),
    .extract_leagues
  )
}


.fotmob_get_league_ids <- function(cached = TRUE, ...) {
  leagues <- fotmob_get_league_ids(cached = cached)
  .fotmob_get_league_urls(
    leagues = leagues,
    ...
  )
}

#' @importFrom stringr str_replace
.fotmob_get_league_resp_from_build_id <- function(page_url, stats = FALSE) {
  build_id <- .fotmob_get_build_id()
  url <- sprintf("https://www.fotmob.com/_next/data/%s%s.json", build_id, page_url)
  if(stats) {
    url <- stringr::str_replace(url, "overview", "stats")
  }
  resp <- safely_get_content(url)
  if (is.null(resp$error)) {
    return(resp$result)
  }
  stop(sprintf("Error in `.fotmob_get_league_resp_from_build_id`:\n%s:", resp$error))
}

#' @importFrom purrr safely
#' @importFrom httr parse_url build_url
#' @importFrom rlang inform
#' @importFrom glue glue
.fotmob_get_league_resp <- function(league_id, page_url, season = NULL, fallback = TRUE) {
  url <- httr::parse_url("https://www.fotmob.com/api/leagues")
  url$query <- list(
    "id" = league_id,
    "season" = season
  )
  url <- httr::build_url(url)
  resp <- safely_get_content(url)
  if(!is.null(resp$result)) {
    return(resp$result)
  }

  first_url <- url
  if(fallback) {
    if (!is.null(season)) {
      rlang::inform(
        glue::glue('`season` ignored in call to "{page_url}".')
      )
    }
    resp <- .fotmob_get_league_resp_from_build_id(page_url)
    if(!is.null(resp$result)) {
      return(resp$result)
    }
  }

  stop(
    sprintf("Could not identify the league endpoint at either %s or %s. Stopping with the following error from jsonlite::fromJSON:\n", first_url, url, res$error)
  )
}

#' Get fotmob match results by league
#'
#' Returns match status given a league and season
#'
#' @param country Three character country code. Can be one or multiple. If provided, `league_name` must also be provided (of the same length)
#' @param league_name League names. If provided, `country` must also be provided (of the same length).
#' @param league_id Fotmob ID for the league. Only used if `country` and `league_name` are not specified.
#' @param season Season, e.g. `"2021/2022"`. Can be one or multiple. If left as `NULL` (default), data for the latest season available will be pulled.
#' @inheritParams fotmob_get_league_ids
#'
#' @return returns a dataframe of league matches
#'
#' @importFrom purrr possibly pmap_dfr
#' @importFrom tibble tibble
#' @importFrom rlang maybe_missing
#' @importFrom tidyr crossing
#'
#' @export
#'
#' @examples
#' \donttest{
#' try({
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
#' # can specify past seasons
#' fotmob_get_league_matches(
#'   country = "GER",
#'   league_name = "1. Bundesliga",
#'   season = "2020/2021"
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
#' })
#' }
fotmob_get_league_matches <- function(country, league_name, league_id, season = NULL, cached = TRUE) {

  urls <- .fotmob_get_league_ids(
    cached = cached,
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL)
  )

  # Need to coerce to `NA_character` since crossing doesn't like `NULL`
  season <- ifelse(is.null(season), NA_character_, season)
  urls <- tidyr::crossing(
    urls,
    "season" = season
  )

  purrr::pmap_dfr(
    list(
      urls$id,
      urls$page_url,
      urls$season
    ),
    .fotmob_get_league_matches
  )
}

#' @importFrom glue glue glue_collapse
#' @importFrom rlang inform
.fotmob_message_for_season <- function(resp, season = NULL) {

  if (is.null(season)) {
    rlang::inform(
      glue::glue('Defaulting `season` to latest ("{resp$allAvailableSeasons[1]}").'),
      .frequency = "once",
      .frequency_id = ".fotmob_get_league_(matches|tables)"
    )
  } else {
    if (isFALSE(season %in% resp$allAvailableSeasons)) {
      stop(
        glue::glue(
          "`season` should be one of the following:\n{glue::glue_collapse(resp$allAvailableSeasons, '\n')}"
        )
      )
    }
  }

}

#' @importFrom purrr map_lgl
all_len1 <- function(x) {
  is.list(x) & all(
    purrr::map_lgl(
      x,
      ~length(.x) == 1L
    )
  )
}

#' @importFrom tidyr unnest
#' @importFrom tidyselect vars_select_helpers
unnest_where_all_len1 <- function(df) {
  df %>%
    tidyr::unnest(tidyselect::vars_select_helpers$where(all_len1))
}

#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_rows
.fotmob_get_league_matches <- function(league_id, page_url, season = NULL) {
  # And now coerce NA back to NULL
  season <- switch(!is.na(season), season, NULL)
  resp <- .fotmob_get_league_resp(
    league_id = league_id,
    page_url = page_url,
    season = season
  )

  .fotmob_message_for_season(resp, season)
  matches <- resp$matches$allMatches
  matches %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON() %>%
    unnest_where_all_len1() %>%
    janitor::clean_names()
}

#' Get standings from fotmob
#'
#' Returns league standings from fotmob.com. 3 types are returned: all, home, away
#'
#' @inheritParams fotmob_get_league_matches
#'
#' @return returns a dataframe of league standings
#'
#' @importFrom purrr possibly map2_dfr
#' @importFrom tibble tibble
#' @importFrom rlang maybe_missing
#' @importFrom tidyr crossing
#'
#' @export
#'
#' @examples
#' \donttest{
#' try({
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
#' # one league, past season
#' fotmob_get_league_tables(
#'   country = "GER",
#'   league_name = "1. Bundesliga",
#'   season = "2020/2021"
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
#' })
#' }
fotmob_get_league_tables <- function(country, league_name, league_id, season = NULL, cached = TRUE) {

  urls <- .fotmob_get_league_ids(
    cached = cached,
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL)
  )

  season <- ifelse(is.null(season), NA_character_, season)
  urls <- tidyr::crossing(
    urls,
    "season" = season
  )

  purrr::pmap_dfr(
    list(
      urls$id,
      urls$page_url,
      urls$season
    ),
    .fotmob_get_league_tables
  )
}

#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
#' @importFrom dplyr select all_of bind_rows rename mutate
#' @importFrom tidyr pivot_longer unnest_longer unnest
.fotmob_get_league_tables <- function(league_id, page_url, season = NULL) {

  season <- switch(!is.na(season), season, NULL)
  resp <- .fotmob_get_league_resp(
    league_id = league_id,
    page_url = page_url,
    season = season
  )
  .fotmob_message_for_season(resp, season)

  table_init <- jsonlite::fromJSON(jsonlite::toJSON(resp$table))
  table_init <- dplyr::bind_rows(table_init$data)
  # TODO:
  # - Use purrr::flatten_chr(resp$table$data$tableFilterTypes) instead of hard-coding `cols`?
  # - Extract "form" as well?
  cols <- c("all", "home", "away")
  table <- if("table" %in% names(table_init)) {
    table_init$table %>% dplyr::select(dplyr::all_of(cols))
  } else if("tables" %in% names(table_init)) {
    tables <- dplyr::bind_rows(table_init$tables)
    tables$all <- tables$table$all
    tables$home <- tables$table$home
    tables$away <- tables$table$away
    tables %>%
      dplyr::rename(
        group_id = .data[["leagueId"]],
        group_page_url = .data[["pageUrl"]],
        group_name = .data[["leagueName"]]
      ) %>%
      dplyr::select(
        -c(.data[["table"]], .data[["legend"]])
      )
  } else {
    stop(
      "Expected to find `table` or `tables` element but did not."
    )
  }
  table <- table %>%
    janitor::clean_names() %>%
    tibble::as_tibble()

  browser()
  res <- table %>%
    tidyr::pivot_longer(
      dplyr::all_of(cols),
      names_to = "table_type",
      values_to = "table"
    ) %>%
    tidyr::unnest_longer(
      .data[["table"]]
    ) %>%
    tidyr::unnest(
      .data[["table"]],
      names_sep = "_"
    ) %>%
    janitor::clean_names() %>%
    tibble::as_tibble()

  res %>%
    dplyr::mutate(
      league_id = !!league_id,
      page_url = !!page_url,
      .before = 1
    ) %>%
    unnest_where_all_len1()
}
