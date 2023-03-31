
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

#' @importFrom glue glue
#' @importFrom tibble as_tibble tibble
#' @importFrom purrr pluck map_dfr map2_dfr possibly
#' @importFrom dplyr bind_cols select filter distinct any_of
#' @importFrom tidyr pivot_longer pivot_wider unnest_wider
#' @importFrom rlang .data
#' @importFrom janitor make_clean_names
#' @importFrom tibble as_tibble tibble
#' @importFrom stringr str_detect
.fotmob_get_single_match_players <- function(match_id) {
  # CRAN feedback was to remove this from the existing functions so I have for now
  # print(glue::glue("Scraping match data from fotmob for match {match_id}."))
  main_url <- "https://www.fotmob.com/api/"
  url <- paste0(main_url, "matchDetails?matchId=", match_id)

  f <- function(url) {
    resp <- safely_get_content(url)
    if (!is.null(resp$error)) {
      stop(sprintf("Error in `.fotmob_get_single_match_players`:\n%s", resp$error))
    }

    res <- resp$result
    table <- res$content$table
    lineup <- res$content$lineup$lineup
    starters <- lineup$players
    bench <- lineup$bench
    stopifnot(length(starters) == 2) ## 2 teams
    stopifnot(length(bench) == 2)

    .clean_positions <- function(p) {

      ## index represents F/M/D/G (1/2/3/4), row index represents player (usually up to 4)
      player_ids <- as.character(p[["id"]])
      n <- length(player_ids)
      ## for some reason jsonlite tries to combine stuff row-wise when it really should just bind things column-wise
      ps <- p[["stats"]]

      .clean_stats <- function(x) {
        if(is.null(x)) {
          return(data.frame(dummy = 1))
        }
        xt <- t(x)
        xt[xt == "NULL"] <- NA_real_
        rn <- rownames(xt)
        suppressWarnings(
          xt2 <- matrix(
            as.character(xt),
            ncol = ncol(xt)
          ) %>%
            tibble::as_tibble()
        )
        xt2 %>%
          dplyr::bind_cols(
            col = janitor::make_clean_names(rn)
          ) %>%
          tidyr::pivot_longer(
            -.data[["col"]]
          ) %>%
          dplyr::select(
            -.data[["name"]]
          ) %>%
          dplyr::filter(stringr::str_detect(.data[["col"]], "^stats_"), !is.na(.data[["value"]])) %>%
          dplyr::distinct(.data[["col"]], .data[["value"]]) %>%
          tidyr::pivot_wider(
            names_from = "col",
            values_from = "value"
          ) %>%
          as.data.frame()
      }

      stats <- if(is.null(ps)) {
        list(NULL)
      } else {

        pss <- ps %>% purrr::map_dfr(.clean_stats)
        if(any(colnames(pss) == "dummy")) {
          pss <- pss %>% dplyr::select(-dplyr::any_of("dummy"))
        }
        pss
      }

      pp <- function(..., .na, .f) {
        res <- purrr::pluck(p, ...)
        if(is.null(res) | length(res) == 0) {
          return(rep(.na, n))
        }
        dots <- list(...)
        .f(res)
      }
      ppc <- function(...) pp(..., .na = NA_character_, .f = as.character)
      ppi <- function(...) pp(..., .na = NA_integer_, .f = as.integer)
      ppl <- function(...) pp(..., .na = NA, .f = as.logical)
      pp2 <- function(...) {
        res <- purrr::pluck(p, ...)
        if(is.null(res) | length(res) == 0) {
          return(NULL)
        }
        res
      }

      rows <- tibble::tibble(
        "id" = player_ids,
        "using_opta_id" = ppl("usingOptaId"),
        "first_name" = ppc("name", "firstName"),
        "last_name" = ppc("name", "lastName"),
        "image_url" = ppc("imageUrl"),
        "page_url" = ppc("pageUrl"),
        "shirt" = ppc("shirt") ,
        "is_home_team" = ppl("isHomeTeam"),
        "time_subbed_on" = ppi("timeSubbedOn"),
        "time_subbed_off" = ppi("timeSubbedOff"),
        "usual_position" = ppi("usualPosition"),
        "position_row" = ppi("positionRow"),
        "role" = ppc("role"),
        "is_captain" = ppl("isCaptain"),
        "subbed_out" = ppi("events", "subbedOut"),
        "g" = ppi("events", "g"),
        "rating_num" = ppc("rating", "num"),
        "rating_bgcolor" = ppc("rating", "bgcolor"),
        "is_top_rating" = ppl("rating", "isTop", "isTopRating"),
        "is_match_finished" = ppl("rating", "isTop", "isMatchFinished"),
        "fantasy_score_num" = ppc("fantasyScore", "num"),
        "fantasy_score_bgcolor" = ppc("fantasyScore", "bgcolor"),
        "home_team_id" = ppi("teamData", "home", "id"),
        "home_team_color" = ppc("teamData", "home", "color"),
        "away_team_id" = ppi("teamData", "away", "id"),
        "away_team_color" = ppc("teamData", "away", "color")
      )
      rows$stats <- stats
      rows$shotmap <- if(!is.null(pp2("shotmap", 1))) pp2("shotmap") else NULL
      rows
    }

    add_team_info <- function(p, i) {
      res <- .clean_positions(p)
      res$team_id <- lineup$teamId[i]
      res$team_name <- lineup$teamName[i]
      res %>%
        dplyr::relocate(
          .data[["team_id"]],
          .data[["team_name"]],
          .before = 1
        )
    }

    res <- dplyr::bind_rows(
      purrr::map2_dfr(
        starters, seq_along(starters),
        ~purrr::map2_dfr(
          .x, .y,
          add_team_info
        )
      ) %>%
        dplyr::mutate(
          is_starter = TRUE
        ),
      purrr::map2_dfr(
        bench, seq_along(bench),
        add_team_info
      ) %>%
        dplyr::mutate(
          is_starter = FALSE
        )
    ) %>%
      tibble::as_tibble()
    ## Overwrite the existing variables since the away team value is "bad".
    ##   See https://github.com/JaseZiv/worldfootballR/issues/93
    ## For non-domestic leagues, the table element will not have a teams element
    ##   See https://github.com/JaseZiv/worldfootballR/issues/111
    coerce_team_id <- function(df, side) {
      idx <- ifelse(side == "home", 1, 2)
      team_col <- sprintf("%s_team_id", side)
      df[[team_col]] <- ifelse(
        is.logical(table) | !("teams" %in% names(table)),
        df[[team_col]],
        table$teams[idx]
      )
      df
    }
    res <- coerce_team_id(res, "home")
    res <- coerce_team_id(res, "away")
    tidyr::unnest_wider(res, .data[["stats"]])
  }

  fp <- purrr::possibly(
    f,
    quiet = FALSE,
    otherwise = tibble::tibble()
  )
  fp(url)
}
