
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @importFrom janitor clean_names
#' @importFrom purrr safely
.fotmob_get_single_season_stats <- function(league_id, season_id, stat) {
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

#' @importFrom tibble tibble
#' @importFrom rvest read_html html_elements html_attr
#' @importFrom stringr str_replace str_replace str_detect
#' @importFrom rlang maybe_missing
.fotmob_get_stat_and_season_options <- function(
  country,
  league_name,
  league_id,
  team_or_player,
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
  hrefs <- page %>% rvest::html_elements(".SeeAllButton") %>% rvest::html_attr("href")
  next_url <- sprintf(
    "https://www.fotmob.com%s",
    hrefs[1]
  )
  next_page <- next_url %>% rvest::read_html()
  option_elements <- next_page %>% rvest::html_elements("option")
  labels <- option_elements %>% rvest::html_text2()
  rgx <- "(^.*)(\\s)(20[012].*$)"
  league_name <- labels %>% stringr::str_replace(rgx, "\\1")
  season_name <- labels %>% stringr::str_replace(rgx, "\\3")
  league_name <- ifelse(season_name == league_name, NA_character_, league_name)
  is_season <- season_name %>% stringr::str_detect("^2")
  tibble::tibble(
    league_name = league_name,
    option_type = ifelse(is_season, "season", "stat"),
    name = season_name,
    id = option_elements %>% rvest::html_attr("value")
  )
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
#' `stat_name` may be one of the following, although it may not be available for both \code{"team"} or \code{"player"}:
#' \itemize{
#' \item{"Accurate long balls per 90"}
#' \item{"Accurate passes per 90"}
#' \item{"Assists}
#' \item{"Big chances created"}
#' \item{"Big chances missed"}
#' \item{"Blocks per 90"}
#' \item{"Chances created}
#' \item{"Clean sheets"}
#' \item{"Clearances per 90"}
#' \item{"Expected assist (xA)"}
#' \item{"Expected assist (xA) per 90"}
#' \item{"Expected goals (xG)"}
#' \item{"Expected goals (xG) per 90"}
#' \item{"Expected goals on target (xGOT)"}
#' \item{"FotMob rating"}
#' \item{"Fouls committed per 90"}
#' \item{"Goals + Assists"}
#' \item{"Goals conceded per 90"}
#' \item{"Goals per 90"}
#' \item{"Goals prevented"}
#' \item{"Interceptions per 90"}
#' \item{"Penalties conceded"}
#' \item{"Penalties won"}
#' \item{"Possession won final 3rd per 90"}
#' \item{"Red cards"}
#' \item{"Saves per 90"}
#' \item{"Shots on target per 90"}
#' \item{"Shots per 90"}
#' \item{"Successful dribbles per 90"}
#' \item{"Successful tackles per 90"}
#' \item{"Top scorer"}
#' \item{"xG + xA per 90"}
#' \item{"Yellow cards"}
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
#' \dontrun{
#' epl_team_xg_2021 <- fotmob_get_season_stats(
#'   country = "ENG",
#'   league_name = "Premier League",
#'   season = "2020/2021",
#'   stat_type = "Expected goals",
#'   team_or_player = "team"
#' )
#'
#' ## fotmob doesn't has data for 2016/2017 for some leagues, but not all.
#' ##   you'll see a warning about this and receive an empty dataframe
#' get_epl_season_stats(
#'   season = "2016/2017"
#' )
#'
#' ## Note that the `stat_type` name is slightly different.
#' epl_player_xg_2021 <- get_epl_season_stats(
#'   country = "ENG",
#'   league_name = "Premier League",
#'   season = "2020/2021",
#'   stat_type = "Expected goals (xG)",
#'   team_or_player = "player"
#' )
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
    options <- .fotmob_get_stat_and_season_options(
      cached = cached,
      country = country,
      league_name = league_name,
      league_id = league_id,
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
      '`season_name` = "{season_name}", `stat_league_name` = "{stat_league_name}" {stem}. Try one of the following:\n{glue::glue_collapse(sprintf("%s %s", season_options$league_name, season_options$season_name), "\n")}'
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
