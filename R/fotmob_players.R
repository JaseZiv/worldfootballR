#' @importFrom purrr pluck
.pp <- function(p, ..., n, .na, .f) {
  res <- purrr::pluck(p, ...)
  if(is.null(res) | length(res) == 0) {
    return(rep(.na, n))
  }
  dots <- list(...)
  .f(res)
}
.ppc <- function(...) .pp(..., .na = NA_character_, .f = as.character)
.ppi <- function(...) .pp(..., .na = NA_integer_, .f = as.integer)
.ppl <- function(...) .pp(..., .na = NA, .f = as.logical)
#' @importFrom purrr pluck
.pp2 <- function(p, ...) {
  res <- purrr::pluck(p, ...)
  if(is.null(res) | length(res) == 0) {
    return(NULL)
  }
  res
}

#' @importFrom tibble tibble
.parse_stat <- function(x, nm) {
  if (all(is.na(x$key))) {
    return(tibble::tibble())
  }
  tibble::tibble(
    col = nm,
    value = as.character(x$value)
  )
}

#' @importFrom jsonlite fromJSON toJSON
#' @importFrom purrr map_dfr discard
#' @importFrom tidyr pivot_wider
#' @importFrom janitor clean_names
.clean_stats <- function(x) {
  s <- x[['stats']]
  if(is.null(s)) {
    return(data.frame(dummy = 1))
  }
  ## re-parse json since it's in a super-awckward format
  j <- jsonlite::fromJSON(jsonlite::toJSON(s), simplifyDataFrame = FALSE)

  res <- purrr::map_dfr(
    j,
    function(ji) {
      retained_ji <- purrr::discard(
        ji,
        ~length(.x) == 0
      )
      purrr::imap_dfr(
        retained_ji,
        .parse_stat
      )
    }
  )

  tidyr::pivot_wider(
    res,
    names_from = .data[["col"]],
    values_from = .data[["value"]]
  ) %>%
    janitor::clean_names()
}

#' @importFrom purrr map_dfr
#' @importFrom dplyr select any_of
#' @importFrom tibble tibble
.clean_positions <- function(p) {
  ## index represents F/M/D/G (1/2/3/4), row index represents player (usually up to 4)
  player_ids <- as.character(p[["id"]])
  n <- length(player_ids)
  ## for some reason jsonlite tries to combine stuff row-wise when it really should just bind things column-wise
  ps <- p[["stats"]]
  stats <- if(is.null(ps)) {
    NULL
  } else {

    pss <- purrr::map_dfr(ps, .clean_stats)
    if(any(colnames(pss) == "dummy")) {
      pss <- dplyr::select(pss, -dplyr::any_of("dummy"))
    }
    pss
  }


  rows <- tibble::tibble(
    "id" = player_ids,
    "using_opta_id" = .ppl(p, "usingOptaId", n = n),
    "first_name" = .ppc(p, "name", "firstName", n = n),
    "last_name" = .ppc(p, "name", "lastName", n = n),
    "image_url" = .ppc(p, "imageUrl", n = n),
    "page_url" = .ppc(p, "pageUrl", n = n),
    "shirt" = .ppc(p, "shirt", n = n) ,
    "is_home_team" = .ppl(p, "isHomeTeam", n = n),
    "time_subbed_on" = .ppi(p, "timeSubbedOn", n = n),
    "time_subbed_off" = .ppi(p, "timeSubbedOff", n = n),
    "usual_position" = .ppi(p, "usualPosition", n = n),
    "position_row" = .ppi(p, "positionRow", n = n),
    "role" = .ppc(p, "role", n = n),
    "is_captain" = .ppl(p, "isCaptain", n = n),
    "subbed_out" = .ppi(p, "events", "subbedOut", n = n),
    "g" = .ppi(p, "events", "g", n = n),
    "rating_num" = .ppc(p, "rating", "num", n = n),
    "rating_bgcolor" = .ppc(p, "rating", "bgcolor", n = n),
    "is_top_rating" = .ppl(p, "rating", "isTop", "isTopRating", n = n),
    "is_match_finished" = .ppl(p, "rating", "isTop", "isMatchFinished", n = n),
    "fantasy_score_num" = .ppc(p, "fantasyScore", "num", n = n),
    "fantasy_score_bgcolor" = .ppc(p, "fantasyScore", "bgcolor", n = n),
    "home_team_id" = .ppi(p, "teamData", "home", "id", n = n),
    "home_team_color" = .ppc(p, "teamData", "home", "color", n = n),
    "away_team_id" = .ppi(p, "teamData", "away", "id", n = n),
    "away_team_color" = .ppc(p, "teamData", "away", "color", n = n)
  )
  if (nrow(stats) > 1) {
    # browser()
    rows$stats <- vector(mode = "list", length = nrow(stats))
    for (i in 1:nrow(rows)) {
      rows[i, ]$stats <- list(stats[1, ])
    }
  } else {
    rows$stats <- list(stats)
}
  rows$shotmap <- if(!is.null(.pp2(p, "shotmap", 1))) .pp2(p, "shotmap") else NULL
  rows
}

#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate across
#' @importFrom rlang .data
#' @importFrom stats setNames
.wrap_fotmob_match_f <- function(match_ids, f) {
  purrr::map_dfr(
    stats::setNames(match_ids, match_ids),
    f,
    .id = "match_id"
  ) %>%
    dplyr::mutate(
      dplyr::across(.data[["match_id"]], as.integer)
    )
}

#' Get fotmob match player details by match id
#'
#' Returns match details from fotmob.com
#'
#' @param match_ids a vector of strings or numbers representing matches
#'
#' @return returns a dataframe of match players
#'
#' @examples
#' \donttest{
#' try({
#' library(dplyr)
#' library(tidyr)
#' ## single match
#' players <- fotmob_get_match_players(3610132)
#' salah_id <- "292462"
#' players %>%
#'   dplyr::filter(id == salah_id) %>%
#'   dplyr::select(player_id = id, stats) %>%
#'   tidyr::unnest(stats)
#'
#' ## multiple matches
#' fotmob_get_match_players(c(3609987, 3609979))
#' })
#' }
#' @export
fotmob_get_match_players <- function(match_ids) {
  .wrap_fotmob_match_f(match_ids, .fotmob_get_single_match_players)
}

#' @importFrom tibble as_tibble
#' @importFrom purrr pluck map_dfr map2_dfr possibly
#' @importFrom dplyr mutate bind_rows
#' @importFrom tidyr unnest_wider
#' @importFrom rlang .data
#' @importFrom tibble as_tibble tibble
.fotmob_get_single_match_players <- function(match_id) {
  # CRAN feedback was to remove this from the existing functions so I have for now
  # print(glue::glue("Scraping match data from fotmob for match {match_id}."))
  main_url <- "https://www.fotmob.com/api/"
  url <- paste0(main_url, "matchDetails?matchId=", match_id)

  f <- function(url) {
    resp <- safely_get_content(url)$result

    table <- resp$content$table
    lineup <- resp$content$lineup$lineup
    starters <- lineup$players
    bench <- lineup$bench
    stopifnot(length(starters) == 2) ## 2 teams
    stopifnot(length(bench) == 2)

    .add_team_info <- function(p, i) {
      res <- .clean_positions(p)
      dplyr::mutate(
        res,
        "team_id" = lineup$teamId[i],
        "team_name" = lineup$teamName[i],
        .before = 1
      )
    }

    ## Overwrite the existing variables since the away team value is "bad".
    ##   See https://github.com/JaseZiv/worldfootballR/issues/93
    ## For non-domestic leagues, the table element will not have a teams element
    ##   See https://github.com/JaseZiv/worldfootballR/issues/111
    .coerce_team_id <- function(df, side) {
      idx <- ifelse(side == "home", 1, 2)
      team_col <- sprintf("%s_team_id", side)
      df[[team_col]] <- ifelse(
        is.logical(table) | !("teams" %in% names(table)),
        df[[team_col]],
        table$teams[idx]
      )
      df
    }

    clean_starters <- purrr::map2_dfr(
      starters,
      seq_along(starters),
      ~purrr::map2_dfr(
        .x, .y,
        .add_team_info
      )
    ) %>%
      dplyr::mutate(
        is_starter = TRUE
      )

    clean_bench <- purrr::map2_dfr(
      bench,
      seq_along(bench),
      .add_team_info
    ) %>%
      dplyr::mutate(
        is_starter = FALSE
      )

    res <- dplyr::bind_rows(
      clean_starters,
      clean_bench
    ) %>%
      tibble::as_tibble()

    res <- .coerce_team_id(res, "home")
    res <- .coerce_team_id(res, "away")
    tidyr::unnest_wider(dplyr::slice(clean_starters, 2), tidyselect::vars_select_helpers$all_of("stats"), names_sep = "_")
  }

  fp <- purrr::possibly(
    f,
    quiet = FALSE,
    otherwise = tibble::tibble()
  )
  fp(url)
}
