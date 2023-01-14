#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate
#' @importFrom readr type_convert
.fb_single_league_stats <- function(url, team_or_player) {

  session <- worldfootballr_chromote_session(url)
  tables <- worldfootballr_html_table(session)

  idx <- if(team_or_player == "player") { 3 } else { 1:2 }

  purrr::map_dfr(
    idx,
    ~{
      table <- tables[[.x]]

      renamed_table <- .rename_fb_cols(table)

      clean_table <- if (team_or_player == "player") {
        renamed_table[renamed_table$Rk != "Rk", ]
      } else {
        renamed_table %>%
          dplyr::mutate(
            "Team_or_Opponent" = ifelse(.x == 1, "team", "opponent"),
            .before = 1
          )

      }

      suppressMessages(
        readr::type_convert(
          clean_table,
          guess_integer = TRUE,
          na = "",
          trim_ws = TRUE
        )
      )
    }
  )

}

#' Get FBref Team or Player Season Statistics for an Entire League
#'
#' Get the season stats for all teams / players in a selected league
#'
#' @inheritParams fb_match_urls
#' @inheritParams fb_advanced_match_stats
#' @param stat_type the type of statistic required. Must be one of the following:
#' \itemize{
#' \item{standard}
#' \item{keepers}
#' \item{keepersadv}
#' \item{shooting}
#' \item{passing}
#' \item{passing_types}
#' \item{gca}
#' \item{defense}
#' \item{possession}
#' \item{playing_time}
#' \item{misc}
#' }
#'
#' @return a dataframe of season stats for all teams / players in a league
#'
#' @importFrom rlang arg_match0 .data .env
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull transmute
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom tidyr crossing
#' @importFrom purrr possibly map_dfr
#' @importFrom progress progress_bar
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' fb_season_team_stats(
#'   country = "ENG",
#'   gender = "M",
#'   season_end_year = 2022,
#'   tier = "1st",
#'   stat_type = "shooting",
#'   team_or_player = "player"
#' )
#' })
#' }
fb_league_stats <- function(country, gender, season_end_year, tier = "1st", non_dom_league_url = NA, stat_type, team_or_player, time_pause=3) {

  stopifnot("`stat_type` must have length 1", length(stat_type) == 1)
  rlang::arg_match0(
    stat_type,
    c(
      "standard",
      "keepers",
      "keepersadv",
      "shooting",
      "passing",
      "passing_types",
      "gca",
      "defense",
      "possession",
      "playing_time",
      "misc"
    )
  )

  stopifnot("`team_or_player` must have length 1", length(team_or_player) == 1)
  rlang::arg_match0(
    team_or_player,
    c("team", "player")
  )

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  league_urls <- seasons %>%
    dplyr::filter(
      stringr::str_detect(.data[["competition_type"]], "Leagues"),
      .data[["country"]] %in% .env[["country"]],
      .data[["gender"]]  %in% .env[["gender"]],
      .data[["season_end_year"]] %in% .env[["season_end_year"]],
      .data[["tier"]] %in% .env[["tier"]]
    ) %>%
    dplyr::pull(.data[["seasons_urls"]]) %>%
    unique()

  if (length(league_urls) == 0) {
    stop("Could not find any URLs matching input parameters")
  }

  urls <- tidyr::crossing(
    "league_url" = league_urls,
    "stat_type" = stat_type
  ) %>%
    dplyr::transmute(
      "url" = sprintf("%s/%s/%s", dirname(.data[["league_url"]]), .data[["stat_type"]], basename(.data[["league_url"]]))
    ) %>%
    dplyr::pull(.data[["url"]])

  fp <- purrr::possibly(.fb_single_league_stats, otherwise = tibble::tibble())

  n_urls <- length(urls)
  pb <- progress::progress_bar$new(total = n_urls)
  purrr::map_dfr(
    seq_along(urls),
    ~{
      if (.x > 1) {
        Sys.sleep(time_pause)
      }
      pb$tick()
      url <- urls[.x]
      res <- fp(url, team_or_player = team_or_player)
      res[["url"]] <- url
      res
    }
  )

}