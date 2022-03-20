
## TODO: Cache this
.fotmob_get_season_ids <- function(...) {
  .fotmob_get_league_or_season_ids(
    url = "https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/466d399903d2dfc2c0e37058dc2e3f33f1ac74d1/raw-data/fotmob-leagues/season_ids.csv",
    ...
  )
}

## Probably best to just have this always available, until caching is implemented.
.fotmob_stat_types <- .load_fotmob_csv(
  "https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/bdc5a8f8d4b31b477621e309ddb06fae9d35e022/raw-data/fotmob-stats/stat_types.csv"
)

#' @importFrom dplyr filter
.fotmob_validate_stat_type <- function(team_or_player, stat_type) {

  stopifnot(
    "`team_or_player` must be specified." = !is.null(team_or_player),
    "`team_or_player` must have length 1" = length(team_or_player) == 1
  )

  entity <- match.arg(team_or_player, c("player", "team"))

  stopifnot(
    "`stat_type` must be specified." = !is.null(stat_type),
    "`stat_type` must have length 1" = length(stat_type) == 1
  )

  filt_stat_choices <- .fotmob_stat_types %>%
    dplyr::filter(.data$entity == !!team_or_player)

  n_stats <- length(stat_type)
  res <- filt_stat_choices %>% dplyr::filter(.data$stat == !!stat_type)
  n_res <- nrow(res)

  if(n_res == 0) {
    stop(
      "Could not find any matching `stat_type`."
    )
  }
  res
}

#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr select bind_cols
#' @importFrom tidyr unnest
#' @importFrom janitor clean_names
#' @importFrom purrr safely
.fotmob_get_single_season_stats <- function(df, stat_type_df) {
  url <- sprintf(
    "https://data.fotmob.com/stats/%s/season/%s/%s.json",
    df$id,
    df$season_id,
    stat_type_df$full_stat
  )

  ## This still print out HTTP error 403 even with `quiet = TRUE`?!?
  fs <- purrr::safely(jsonlite::fromJSON, otherwise = NULL, quiet = TRUE)
  resp <- fs(url)
  if(!is.null(resp$error)) {
    warning(
      sprintf(
        'Issue with data at `url = "%s".\n%s', url, resp$error
      )
    )
    return(tibble::tibble())
  }
  res <- resp$result %>%
    tibble::as_tibble() %>%
    dplyr::select(.data$TopLists) %>%
    tidyr::unnest(.data$TopLists) %>%
    dplyr::select(.data$StatList) %>%
    tidyr::unnest(.data$StatList) %>%
    janitor::clean_names()

  dplyr::bind_cols(
    df,
    tibble::tibble(stat_type = stat_type_df$stat),
    res
  )
}

#' Get season statistics from fotmob
#'
#' Returns team or player season-long statistics standings from fotmob.com.
#'
#' @inheritParams fotmob_get_league_matches
#' @param season_name Season names in the format `"2021/2022"`. Multiple allowed. If multiple leagues are specified, season stats are retrieved for each league.
#'
#' Note that not Fotmob currently only goes back as far as `"2016/2017"`. Some leagues may not have data for that far back.
#'
#' @param team_or_player return statistics for either \code{"team"} or \code{"player"}. Can only be one or the other.
#' @param stat_type the type of statistic. Can only be one.
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
#' @return returns a dataframe of league standings
#'
#' @importFrom purrr possibly map2_dfr
#' @importFrom tibble tibble
#' @importFrom rlang maybe_missing
#' @importFrom dplyr filter distinct pull mutate row_number group_split
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
  team_or_player,
  stat_type
) {

  ## There's already warnings in this function for non-matching leagues, so we don't need to warn about that here.
  urls <- .fotmob_get_season_ids(
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL)
  )

  stat_type_df <- .fotmob_validate_stat_type(
    team_or_player = rlang::maybe_missing(team_or_player, NULL),
    stat_type = rlang::maybe_missing(stat_type, NULL)
  )

  n_urls <- nrow(urls)
  filt_urls <- urls %>%
    dplyr::filter(
      .data$season_name %in% !!season_name
    )

  if(nrow(filt_urls) == 0) {
    stop(
      sprintf(
        "No league-`season_name` pairs matching parameters. Perhaps try the following:\n- %s",
        glue::glue_collapse(
          glue::glue("{urls$id} - {urls$name} - {urls$country} - {urls$season_name}"),
          sep = "\n- "
        )
      )
    )
  }

  actual_season_names <- filt_urls %>%
    dplyr::distinct(.data$season_name) %>%
    dplyr::pull(.data$season_name)

  diff_season_names <- setdiff(season_name, actual_season_names)
  if(length(diff_season_names) > 0) {
    warning(
      sprintf(
        "Couldn't find the following `season_name`s:\n- %s",
        glue::glue_collapse(glue::glue("{diff_season_names}"), sep = "\n- ")
      )
    )
  }

  ## TODO: It's possible that there's a `season_name` for one league but not for another...
  ##  It would be nice to warn about that.
  fp <- purrr::possibly(
    .fotmob_get_single_season_stats,
    quiet = FALSE,
    otherwise = tibble::tibble()
  )

  ## This feels a little clunky, but this was the most elegant way that I could think of to
  ##   keep around meta info about the league and season (in `filt_urls`) to join with the results.
  filt_urls %>%
    dplyr::mutate(rn = dplyr::row_number()) %>%
    dplyr::group_split(.data$rn, .keep = FALSE) %>%
    purrr::map_dfr(
      ~fp(
        .x,
        stat_type_df = stat_type_df
      )
    )

}
