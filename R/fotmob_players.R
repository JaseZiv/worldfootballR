
#' Get fotmob match player details by match id
#'
#' Returns match details from fotmob.com
#'
#' @param match_ids a vector of strings or numbers representing matches
#'
#' @return returns a dataframe of match players
#'
#' @importFrom purrr map_dfr
#'
#' @examples
#' \dontrun{
#' players <- fotmob_get_match_players(c(3609987, 3609979))
#' }
fotmob_get_match_players <- function(match_ids) {
  purrr::map_dfr(match_ids, .fotmob_get_single_match_players)
}

#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble tibble
#' @importFrom purrr map_dfr possibly
#' @importFrom dplyr bind_cols select filter distinct
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data
#' @importFrom janitor make_clean_names
.fotmob_get_single_match_players <- function(match_id) {
  print(glue::glue("Scraping match data from fotmob for match {match_id}."))
  main_url <- "https://www.fotmob.com/"
  url <- paste0(main_url, "matchDetails?matchId=", match_id)

  f <- function(url) {
    resp <- jsonlite::fromJSON(url)

    lineup <- resp$content$lineup$lineup
    starters <- lineup$players
    bench <- lineup$bench
    stopifnot(length(starters) == 2) ## 2 teams
    stopifnot(length(bench) == 2) ## 2 teams

    .clean_positions <- function(p) {

      ## index represents F/M/D/G (1/2/3/4), row index represents player (usually up to 4)
      player_ids <- as.character(p[["id"]])
      n <- length(player_ids)
      ## for some reason jsonlite tries to combine stuff row-wise when it really should just bind things column-wise
      ps <- p[["stats"]]

      .clean_stats <- function(x) {
        xt <- t(x)
        xt[xt == "NULL"] <- NA_real_
        rn <- rownames(xt)
        suppressWarnings(
          xt2 <- matrix(
            as.numeric(gsub("%", "", xt)),
            ncol = ncol(xt)
          )
        )
        xt2 %>%
          tibble::as_tibble() %>%
          dplyr::bind_cols(
            col = janitor::make_clean_names(rn)
          ) %>%
          tidyr::pivot_longer(
            -.data$col
          ) %>%
          dplyr::select(
            -.data$name
          ) %>%
          dplyr::filter(!is.na(.data$value)) %>%
          dplyr::distinct(.data$col, .data$value) %>%
          tidyr::pivot_wider(
            names_from = "col",
            values_from = "value"
          )
      }
      stats <- ps %>% purrr::map_dfr(.clean_stats)

      pp <- function(..., .na, .f) {
        res <- pluck(p, ...)
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
        res <- pluck(p, ...)
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
      ) %>%
        dplyr::mutate(
          stats = stats,
          shotmap = if(!is.null(pp2("shotmap", 1))) pp2("shotmap") else NULL
        )
      rows
    }

    res <- dplyr::bind_rows(
      purrr::map_dfr(
        starters,
        ~purrr::map_dfr(
          .x,
          .clean_positions
        )
      ) %>%
        dplyr::mutate(
          is_starter = TRUE
        ),
      purrr::map_dfr(
        bench,
        .clean_positions
      ) %>%
        dplyr::mutate(
          is_starter = FALSE
        )
    )
  }

  fp <- purrr::possibly(f, otherwise = data.frame())
  return(fp(url))
}
