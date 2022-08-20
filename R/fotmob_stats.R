
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @importFrom janitor clean_names
#' @importFrom purrr safely
#' @importFrom stringr str_detect str_replace_all
.fotmob_get_single_season_stats <- function(league_id, season_id, stat) {

  if (stringr::str_detect(season_id, '-')) {
    season_id <- stringr::str_replace_all(season_id, '-', '\\/')
  }
  url <- sprintf(
    "https://data.fotmob.com/stats/%s/season/%s/%s.json",
    league_id,
    season_id,
    stat
  )

  ## This still print out HTTP error 403 even with `quiet = TRUE`?!?
  resp <- .safely_from_json(url)
  if(!is.null(resp$error)) {
    warning(
      sprintf(
        'Issue with data at `url = "%s".\n%s', url, resp$error
      )
    )
    return(tibble::tibble())
  }

  resp$result %>%
    tibble::as_tibble() %>%
    dplyr::select(.data$TopLists) %>%
    tidyr::unnest(.data$TopLists) %>%
    dplyr::select(.data$StatList) %>%
    tidyr::unnest(.data$StatList) %>%
    janitor::clean_names()
}

.upper1 <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


.extract_seasons_and_stats_from_options <- function(options) {
  labels <- options %>% rvest::html_text2()
  rgx <- "(^.*)(\\s)(20[012].*$)"
  league_name <- labels %>% stringr::str_replace(rgx, "\\1")
  season_name <- labels %>% stringr::str_replace(rgx, "\\3")
  league_name <- ifelse(season_name == league_name, NA_character_, league_name)
  is_season <- season_name %>% stringr::str_detect("^2")
  tibble::tibble(
    league_name = league_name,
    option_type = ifelse(is_season, "season", "stat"),
    name = season_name,
    id = options %>% rvest::html_attr("value")
  )
}

#' @importFrom tibble tibble as_tibble
#' @importFrom rvest read_html html_elements html_attr
#' @importFrom stringr str_replace str_replace str_detect
#' @importFrom rlang maybe_missing
#' @importFrom dplyr distinct arrange bind_rows
#' @importFrom purrr imap_lgl keep map_dfr
.fotmob_get_stat_and_season_options <- function(
    country,
    league_name,
    league_id,
    team_or_player,
    page_url,
    cached
) {

  tables <- fotmob_get_league_tables(
    cached = cached,
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL)
  )

  url <- sprintf(
    "https://www.fotmob.com%s/%ss",
    stringr::str_replace(tables$page_url[1], "overview", "stats"),
    team_or_player
  )
  page <- url %>% rvest::read_html()

  see_all_button <- page %>% rvest::html_elements(".SeeAllButton")
  has_see_all_button <- length(see_all_button) > 0

  options <- page |> rvest::html_elements("option")
  has_options <- length(options) > 0

  if (has_see_all_button) {

    hrefs <- see_all_button %>% rvest::html_attr("href")
    next_url <- sprintf(
      "https://www.fotmob.com%s",
      hrefs[1]
    )
    next_page <- next_url %>% rvest::read_html()
    options <- next_page %>% rvest::html_elements("option")
    .extract_seasons_and_stats_from_options(options)
    } else if (has_options) {

    values <- options |> rvest::html_attr("value")

    parts <- values |>
      purrr::keep(~stringr::str_detect(.x, "season")) |>
      stringr::str_split("/")

    if(length(parts) == 0) {
      rlang::abort(glue::glue("Could not parse season ids from {url}."))
    }

    if(length(parts[[1]]) < 5) {
      rlang::abort(glue::glue("Season ids not stored in expected format at {url}."))
    }

    season_ids <- parts |>
      purrr::map_chr(~purrr::pluck(.x, 4))

    ## protect against the current season being in the offseason, unless there is no other season.
    season_id <- ifelse(length(season_ids) > 1, season_ids[2], season_ids[1])
    next_url <- sprintf(
      "https://www.fotmob.com/leagues/%s/stats/season/%s/%ss/saves_team",
      tables$league_id[1],
      season_id,
      team_or_player
    )

    next_page <- next_url |> rvest::read_html()
    next_options <- next_page |> rvest::html_elements("option")
    .extract_seasons_and_stats_from_options(next_options)

  } else {
    resp <- .fotmob_get_league_resp_from_build_id(page_url, stats = TRUE)
    if(is.null(resp$result)) {
      stop(
        sprintf("Can't find season stats data. Failed with the following error:\n", resp$error)
      )
    }
    stats <- .fotmob_extract_data_from_page_props(resp$result)$stats
    seasons <- stats$seasonStatLinks$Name

    label <- sprintf("%ss", .upper1(team_or_player))
    valid_seasons <- setNames(seasons, seasons) %>%
      purrr::imap_lgl(
        ~label %in% stats[[.x]]$tabs
      ) %>%
      purrr::keep(isTRUE) %>%
      names()

    extract_options <- function(season) {
      all_stats_df <- stats[[season]][[sprintf("%ss", team_or_player)]] %>%
        dplyr::distinct(.data$header, .data$name, .data$fetchAllUrl)
      season_id <- all_stats_df$fetchAllUrl %>% dirname() %>% basename() %>% unique()
      stats_df <- all_stats_df %>% dplyr::distinct(.data$header, .data$name)
      dplyr::bind_rows(
        tibble::tibble(
          league_name = NA_character_,
          option_type = "season",
          name = season,
          id = season_id[1]
        ),
        tibble::tibble(
          league_name = NA_character_,
          option_type = "stat",
          name = stats_df$header,
          id = stats_df$name
        )
      )
    }
    valid_seasons %>%
      purrr::map_dfr(extract_options) %>%
      dplyr::distinct() %>%
      dplyr::arrange(.data$option_type, .data$name)
  }
}

#' Get season statistics from fotmob
#'
#' Returns team or player season-long statistics standings from fotmob.com.
#'
#' @inheritParams fotmob_get_league_matches
#' @param season_name Season names in the format `"2021/2022"`. Multiple allowed. If multiple leagues are specified, season stats are retrieved for each league.
#' @param stat_league_name Same format as `league_name`. If not provided explicitly, then it takes on the same value as `league_name`. If provided explicitly, should be of the same length as `league_name` (or `league_id` if `league_name` is not provided).
#'
#' Note that not Fotmob currently only goes back as far as `"2016/2017"`. Some leagues may not have data for that far back.
#'
#' @param team_or_player return statistics for either \code{"team"} or \code{"player"}. Can only be one or the other.
#' @param stat_name the type of statistic. Can be more than one.
#' `stat_name` must be one of the following for \code{"player"}:
#'
#' \itemize{
#' \item{Accurate long balls per 90}
#' \item{Accurate passes per 90}
#' \item{Assists}
#' \item{Big chances created}
#' \item{Big chances missed}
#' \item{Blocks per 90}
#' \item{Chances created}
#' \item{Clean sheets}
#' \item{Clearances per 90}
#' \item{Expected assist (xA)}
#' \item{Expected assist (xA) per 90}
#' \item{Expected goals (xG)}
#' \item{Expected goals (xG) per 90}
#' \item{Expected goals on target (xGOT)}
#' \item{FotMob rating}
#' \item{Fouls committed per 90}
#' \item{Goals + Assists}
#' \item{Goals conceded per 90}
#' \item{Goals per 90}
#' \item{Goals prevented}
#' \item{Interceptions per 90}
#' \item{Penalties conceded}
#' \item{Penalties won}
#' \item{Possession won final 3rd per 90}
#' \item{Red cards}
#' \item{Save percentage}
#' \item{Saves per 90}
#' \item{Shots on target per 90}
#' \item{Shots per 90}
#' \item{Successful dribbles per 90}
#' \item{Successful tackles per 90}
#' \item{Top scorer}
#' \item{xG + xA per 90}
#' \item{Yellow cards}
#' }
#'
#' For \code{"team"}, `stat_name` must be one of the following:
#' \itemize{
#' \item{Accurate crosses per match}
#' \item{Accurate long balls per match}
#' \item{Accurate passes per match}
#' \item{Average possession}
#' \item{Big chances created}
#' \item{Big chances missed}
#' \item{Clean sheets}
#' \item{Clearances per match}
#' \item{Expected goals}
#' \item{FotMob rating}
#' \item{Fouls per match}
#' \item{Goals conceded per match}
#' \item{Goals per match}
#' \item{Interceptions per match}
#' \item{Penalties awarded}
#' \item{Penalties conceded}
#' \item{Possession won final 3rd per match}
#' \item{Red cards}
#' \item{Saves per match}
#' \item{Shots on target per match}
#' \item{Successful tackles per match}
#' \item{xG conceded}
#' \item{Yellow cards}
#' }
#'
#' Fotmob has changed these stat names over time, so this list may be out-dated. If you try an invalid stat name, you should see an error message indicating which ones are available.
#'
#' @return returns a dataframe of team or player stats
#'
#' @importFrom purrr map_dfr map2_dfr pmap_dfr possibly
#' @importFrom rlang maybe_missing .data
#' @importFrom dplyr filter select
#' @importFrom tibble tibble
#' @importFrom glue glue_collapse
#'
#' @export
#' @examples
#' \donttest{
#' try({
#' epl_team_xg_2021 <- fotmob_get_season_stats(
#'   country = "ENG",
#'   league_name = "Premier League",
#'   season = "2020/2021",
#'   stat_name = "Expected goals",
#'   team_or_player = "team"
#' )
#' })
#' }
fotmob_get_season_stats <- function(
    country,
    league_name,
    league_id,
    season_name,
    team_or_player = c("team", "player"),
    stat_name,
    stat_league_name = league_name,
    cached = TRUE
) {

  match.arg(team_or_player, several.ok = FALSE)
  stopifnot("`season_name` cannot be NULL.`" = !is.null(season_name))

  urls <- .fotmob_get_league_ids(
    cached = cached,
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL)
  )
  stat_league_name <- rlang::maybe_missing(stat_league_name, urls$name)

  n_league_name <- length(urls$name)
  n_stat_league_name <- length(stat_league_name)
  ## this check only comes into play if the user explicitly specifies `stat_league_name`
  if(n_league_name != n_stat_league_name) {
    stop(
      sprintf(
        "`league_name` and `stat_league_name` must have the same length (%s != %s)", n_league_name, n_stat_league_name
      )
    )
  }
  urls$stat_league_name <- stat_league_name

  fp <- purrr::possibly(
    .fotmob_get_single_league_single_season_stats,
    otherwise = tibble::tibble(),
    quiet = FALSE
  )

  ## Note that this is written in this awkward fashion (instead of expanding on stat, season_name, AND league_id)
  ##   so that we can re-use the season options for a given league without having to re-scrape it every time
  ##   (if we have multiple stats or seasons for a given league).
  g <- function(stat_name, season_name, league_id) {

    url <- urls %>% dplyr::filter(.data$id == !!league_id)
    country <-  url$ccode
    league_name <- url$name
    page_url <- url$page_url
    options <- .fotmob_get_stat_and_season_options(
      cached = cached,
      country = country,
      league_name = league_name,
      league_id = league_id,
      page_url = page_url,
      team_or_player = team_or_player
    )

    stat_options <- options %>%
      dplyr::filter(.data$option_type == "stat") %>%
      dplyr::select(stat_name = .data$name, stat = .data$id)

    filt_stat_options <- stat_options %>%
      dplyr::filter(.data$stat_name == !!stat_name)

    if(nrow(filt_stat_options) == 0) {
      stop(
        glue::glue('`"{stat_name}"` is not a valid `stat_name` for `league_name = "{league_name}"` (`league_id = {league_id}`). Try one of the following:\n{glue::glue_collapse(stat_options$stat_name, "\n")}')
      )
    }

    season_options <- options %>%
      dplyr::filter(.data$option_type == "season") %>%
      dplyr::select(.data$league_name, season_name = .data$name, season_id = .data$id)

    if(nrow(season_options) == 0) {
      stop(
        glue::glue(
          "No seasons with stats found for league. Try one of the following:\n{glue::glue_collapse(stat_options$stat_name, '\n')}"
        )
      )
    }

    season_options$league_name <- ifelse(
      is.na(season_options$league_name),
      league_name,
      season_options$league_name
    )

    purrr::map_dfr(
      season_name,
      ~fp(
        country = country,
        league_name = league_name,
        league_id = league_id,
        stat_name = stat_name,
        stat = filt_stat_options %>%
          dplyr::filter(.data$stat_name == !!stat_name) %>%
          dplyr::pull(.data$stat),
        stat_league_name = url$stat_league_name,
        season_name = .x,
        season_options = season_options
      )
    )
  }

  params <- expand.grid(
    stat_name = stat_name,
    season_name = season_name,
    stringsAsFactors = FALSE
  )

  purrr::map2_dfr(
    params$stat_name,
    params$season_name,
    ~purrr::pmap_dfr(
      list(
        .x,
        .y,
        urls$id
      ),
      ~g(..1, ..2, ..3)
    )
  )

}

#' @importFrom glue glue glue_collapse
#' @importFrom dplyr filter mutate
#' @importFrom tibble tibble
#' @importFrom rlang .data
.fotmob_get_single_league_single_season_stats <- function(
    country,
    league_name,
    league_id,
    stat_name,
    stat,
    stat_league_name,
    season_name,
    season_options
) {

  filt_season_options <- season_options %>%
    dplyr::filter(
      .data$league_name == !!stat_league_name,
      .data$season_name == !!season_name
    )

  n_season_options <- nrow(filt_season_options)

  print_season_league_name_error <- function(stem) {
    glue::glue(
      '`season_name` = "{season_name}", `stat_league_name` = "{stat_league_name}" {stem}. Try one of the following `stat_league_name`, `season_name` pairs:\n{glue::glue_collapse(sprintf("%s, %s", season_options$league_name, season_options$season_name), "\n")}'
    )
  }

  if(n_season_options == 0) {
    stop(
      print_season_league_name_error("not found")
    )
  }

  if(n_season_options > 1) {
    stop(
      print_season_league_name_error("match more than 1 result")
    )
  }

  fp <- purrr::possibly(
    .fotmob_get_single_season_stats,
    quiet = FALSE,
    otherwise = tibble::tibble()
  )

  res <- fp(
    league_id = league_id,
    season_id = filt_season_options$season_id,
    stat = stat
  )

  res %>%
    dplyr::mutate(
      country = country,
      league_name = league_name,
      league_id = league_id,
      season_name = season_name,
      season_id = filt_season_options$season_id,
      stat_league_name = stat_league_name,
      stat_name = stat_name,
      stat = stat,
      .before = 1
    )

}
