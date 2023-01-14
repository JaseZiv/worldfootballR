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

      renamed_table <- if (team_or_player == "player") {
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
          renamed_table,
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
#' @importFrom tibble tibble
#' @importFrom tidyr crossing
#' @importFrom purrr possibly map_dfr
#' @importFrom stats setNames
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
fb_league_stats <- function(country, gender, season_end_year, tier = "1st", non_dom_league_url = NA, stat_type, team_or_player) {

  purrr::map_chr(
    stat_type,
    ~rlang::arg_match0(
      .x,
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
  )

  rlang::arg_match0(
    team_or_player,
    c("team", "player")
  )

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  league_urls <- seasons %>%
    # dplyr::filter(.data$country == .env$country)
    dplyr::filter(
      # stringr::str_detect(.data[["competition_type"]], "Leagues"),
      .data[["country"]] %in% .env[["country"]],
      .data[["gender"]]  %in% .env[["gender"]],
      .data[["season_end_year"]] %in% .env[["season_end_year"]],
      .data[["tier"]] %in% .env[["tier"]]
    ) %>%
    dplyr::pull(seasons_urls) %>%
    unique()

  if (nrow(league_urls) == 0) {
    warning("Could not find any URLs matching input parameters")
    return(tibble::tibble())
  }

  urls <- tidyr::crossing(
    "league_url" = league_urls,
    "stat_type" = stat_type
  ) %>%
    dplyr::transmute(
      "url" = sprintf("%s/%s/%s", dirname(.data[["league_url"]]), .data[["stat_type"]], basename(.data[["league_url"]]))
    )

  fp <- purrr::possibly(.fb_single_league_stats, otherwise = tibble::tibble())

  purrr::map_dfr(
    stats::setNames(urls, urls),
    fp,
    team_or_player = team_or_player,
    .id = "url"
  )

}
