
## Probably best to just have this always available, until caching is implemented.
.fotmob_stat_types <- .fotmob_load_csv(
  "fotmob-stats/stat_types.csv"
)

#' @importFrom dplyr filter
.fotmob_validate_stat_type <- function(team_or_player, stat_type) {

  stopifnot(
    "`team_or_player` must be specified." = !is.null(team_or_player),
    "`team_or_player` must have length 1" = length(team_or_player) == 1
  )

  entity <- match.arg(team_or_player, c("player", "team"))

  stopifnot(
    "`stat_type` must be specified." = !is.null(stat_type)
  )

  filt_stat_choices <- .fotmob_stat_types %>%
    dplyr::filter(.data$entity == !!team_or_player)

  n_stats <- length(stat_type)
  res <- filt_stat_choices %>% dplyr::filter(.data$stat %in% !!stat_type)
  n_res <- nrow(res)

  if(n_res == 0) {
    stop(
      "Could not find any matching `stat_type`."
    )
  }
  res
}

#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @importFrom janitor clean_names
#' @importFrom purrr safely
.fotmob_get_single_season_stats <- function(league_id, season_id, full_stat, stat) {
  url <- sprintf(
    "https://data.fotmob.com/stats/%s/season/%s/%s.json",
    league_id,
    season_id,
    full_stat
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
#' @importFrom stringr str_replace str_replace
#' @importFrom rlang maybe_missing
.fotmob_get_season_options <- function(
  country,
  league_name,
  league_id,
  team_or_player,
  cached = TRUE
) {
  tables <- fotmob_get_league_tables(
    cached = TRUE,
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL)
  )
  team_id <- tables$id[1]
  url <- sprintf(
    "https://www.fotmob.com%s/%ss",
    stringr::str_replace(tables$page_url[1], "overview", "stats"),
    team_or_player
  )
  page <- url %>% rvest::read_html()
  option_elements <- page %>% rvest::html_elements("option")
  labels <- option_elements %>% rvest::html_text2()
  rgx <- "(^.*)(\\s)(20[012].*$)"
  tibble::tibble(
    league_name = labels %>% stringr::str_replace(rgx, "\\1"),
    season_name = labels %>% stringr::str_replace(rgx, "\\3"),
    season_id = option_elements %>% rvest::html_attr("value")
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
#' @param stat_type the type of statistic. Can be more than one.
#' For \code{entity = "player"}, must be one of the following:
#' \itemize{
#' \item{"accurate_pass"}
#' \item{"big_chance_created"}
#' \item{"big_chance_missed"}
#' \item{"clean_sheet"}
#' \item{"effective_clearance"}
#' \item{"xg"}
#' \item{"xg_conceded"}
#' \item{"assist"}
#' \item{"goals"}
#' \item{"mins_played_goal"}
#' \item{"ontarget_scoring_att"}
#' \item{"penalty_won"}
#' \item{"poss_won_att_3rd"}
#' \item{"rating"}
#' \item{"red_card"}
#' \item{"saves"}
#' \item{"total_att_assist"}
#' \item{"won_contest"}
#' \item{"won_tackle"}
#' \item{"yellow_card"}
#' }
#' For `entity = "team"` all of the same stats are available, with the exception of:
#' \itemize{
#' \item{"goals"}
#' \item{"assist"}
#' \item{"won_contest"}
#' }
#' Additional stats available for \code{"team"} are:
#' \itemize{
#' \item{"goals_conceded_per_match"}
#' \item{"goals_per_match"}
#' \item{"possession_percentage"}
#' }
#'
#' @return returns a dataframe of team or player stats
#'
#' @importFrom purrr map_dfr map2_dfr pmap_dfr possibly
#' @importFrom rlang maybe_missing .data
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#'
#' @export
#' @examples
#' \dontrun{
#' epl_team_xg_2021 <- fotmob_get_season_stats(
#'   country = "ENG",
#'   league_name = "Premier League",
#'   season = "2020/2021",
#'   stat_type = "xg",
#'   team_or_player = "team"
#' )
#'
#' ## fotmob doesn't has data for 2016/2017 for some leagues, but not all.
#' ##   you'll see a warning about this and receive an empty dataframe
#' get_epl_season_stats(
#'   season = "2016/2017"
#' )
#'
#' epl_player_xg_2021 <- get_epl_season_stats(
#'   country = "ENG",
#'   league_name = "Premier League",
#'   season = "2020/2021",
#'   stat_type = "xg",
#'   team_or_player = "player"
#' )
#' }
fotmob_get_season_stats <- function(
  country,
  league_name,
  league_id,
  season_name,
  team_or_player = c("team", "player"),
  stat_type,
  stat_league_name = league_name,
  cached = TRUE
) {

  match.arg(team_or_player, several.ok = FALSE)
  stopifnot("`season_name` cannot be NULL.`" = !is.null(season_name))

  stat_type_df <- .fotmob_validate_stat_type(
    team_or_player = rlang::maybe_missing(team_or_player, NULL),
    stat_type = rlang::maybe_missing(stat_type, NULL)
  )

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

  fp <- purrr::possibly(.fotmob_get_single_league_single_season_stats, otherwise = tibble::tibble(), quiet = FALSE)

  ## Note that this is written in this awkward fashion (instead of expanding on stat, season_name, AND league_id)
  ##   so that we can re-use the season options for a given league without having to re-scrape it every time
  ##   (if we have multiple stats or seasons for a given league).
  g <- function(stat, season_name, league_id) {

    url <- urls %>% dplyr::filter(.data$id == !!league_id)
    country <-  url$ccode
    league_name <- url$name
    season_options <- .fotmob_get_season_options(
      cached = cached,
      country = country,
      league_name = league_name,
      league_id = league_id,
      team_or_player = team_or_player
    )

    if(nrow(season_options) == 0) {
      stop(
        sprintf("No seasons with stats found for league.")
      )
    }

    purrr::map_dfr(
      season_name,
      ~fp(
        country = country,
        league_name = league_name,
        league_id = league_id,
        stat = stat,
        full_stat = stat_type_df %>% dplyr::filter(.data$stat == !!stat) %>% dplyr::pull(.data$full_stat),
        stat_league_name = url$stat_league_name,
        season_name = .x,
        season_options = season_options
      )
    )
  }

  params <- expand.grid(
    stat = stat_type_df$stat,
    season_name = season_name,
    stringsAsFactors = FALSE
  )

  purrr::map2_dfr(
    params$stat,
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
  stat,
  full_stat,
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
  if(n_season_options == 0) {
    stop(
      glue::glue(
        '`season_name` = "{season_name}", `stat_league_name` = "{stat_league_name}" not found. Try one of the following:\n{glue::glue_collapse(sprintf("%s %s", season_options$league_name, season_options$season_name), "\n")}
        ',
      )
    )
  }

  if(n_season_options > 1) {
    stop(
      glue::glue(
        '`season_name` = "{season_name}", `stat_league_name` = "{stat_league_name}" match more than 1 result ({n_season_options}):\n{glue::glue_collapse(sprintf("%s %s", season_options$league_name, season_options$season_name), "\n")}'
      )
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
    full_stat = full_stat,
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
      stat_type = stat,
      .before = 1
    )

}
