#' @importFrom rvest html_table
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate
#' @importFrom rlang inform
#' @importFrom tibble tibble
#' @importFrom readr type_convert
.fb_single_league_stats <- function(url, team_or_player) {

  clean_table <- if (team_or_player == "team") {
    page <- .load_page(url)

    tables <- rvest::html_table(page)

    n_tables <- length(tables)
    if (n_tables != 2) {
      warning(sprintf("Did not find the expected number of tables on the page (2). Found %s.", n_tables))
      return(tibble::tibble())
    }

    purrr::map_dfr(
      seq_along(n_tables),
      ~{
        tables[[.x]] %>%
          .rename_fb_cols() %>%
          dplyr::mutate(
            "Team_or_Opponent" = ifelse(.x == 1, "team", "opponent"),
            .before = 1
          )
      }
    )

  } else {

    rlang::inform(
      'Please be aware that `fb_league_stats(..., team_or_player = "player")` depends on promises, which may not always work.',
      .frequency = "once",
      .frequency_id = "fb_league_stats-player"
    )
    session <- worldfootballr_chromote_session(url)
    page <- worldfootballr_html_page(session)
    session$session$close(wait_ = FALSE)
    elements <- xml2::xml_children(xml2::xml_children(page))
    tables <- rvest::html_table(elements)

    n_tables <- length(tables)
    if (n_tables != 3) {
      warning(sprintf("Did not find the expected number of tables on the page (3). Found %s.", n_tables))
      return(tibble::tibble())
    }
    renamed_table <- .rename_fb_cols(tables[[3]])
    renamed_table <- renamed_table[renamed_table$Rk != "Rk", ]
    renamed_table <- .add_player_href(
      renamed_table,
      parent_element = elements[[3]],
      player_xpath = ".//tbody/tr/td[@data-stat='player']/a"
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


#' Get FBref Team or Player Season Statistics for an Entire League
#'
#' Get the season stats for all teams / players in a selected league
#'
#' \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
#'
#' @inheritParams fb_match_urls
#' @inheritParams fb_advanced_match_stats
#' @param rate passed to `purrr::insistently`. The function can be brittle, and works only on a second or third try. This parameter controls how much the function will retry to get a result.
#' @param stat_type the type of statistic required. Must be one of the following:
#' \itemize{
#' \item{standard}
#' \item{shooting}
#' \item{passing}
#' \item{passing_types}
#' \item{gca}
#' \item{defense}
#' \item{possession}
#' \item{playing_time}
#' \item{misc}
#' \item{keepers}
#' \item{keepers_adv}
#' }
#'
#' @return a dataframe of season stats for all teams / players in a league
#'
#' @importFrom rlang arg_match0 .data .env
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull transmute case_when
#' @importFrom stringr str_detect
#' @importFrom tidyr crossing
#' @importFrom tibble tibble
#' @importFrom purrr possibly map_dfr insistently rate_backoff
#' @importFrom progress progress_bar
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' fb_league_stats(
#'   country = "ENG",
#'   gender = "M",
#'   season_end_year = 2022,
#'   tier = "1st",
#'   stat_type = "shooting",
#'   team_or_player = "player"
#' )
#' ## Non-domestic league
#' ##   Note that this is more likely to fail due to the volume of players
#' fb_league_stats(
#'   country = NA_character_,
#'   gender = "M",
#'   season_end_year = 2023,
#'   tier = NA_character_,
#'   non_dom_league_url = "https://fbref.com/en/comps/8/history/Champions-League-Seasons",
#'   stat_type = "standard",
#'   team_or_player = "player"
#' )
#' })
#' }
fb_league_stats <- function(
    country,
    gender,
    season_end_year,
    tier = "1st",
    non_dom_league_url = NA,
    stat_type,
    team_or_player,
    time_pause = 3,
    rate = purrr::rate_backoff(
      max_times =  3
    )
) {


  stopifnot("`stat_type` must have length 1" = length(stat_type) == 1)
  rlang::arg_match0(
    stat_type,
    c(
      "standard",
      "shooting",
      "passing",
      "passing_types",
      "gca",
      "defense",
      "possession",
      "playing_time",
      "misc",
      "keepers",
      "keepers_adv"
    )
  )

  stopifnot("`team_or_player` must have length 1" = length(team_or_player) == 1)
  rlang::arg_match0(
    team_or_player,
    c("team", "player")
  )

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  seasons_urls <- seasons %>%
    dplyr::filter(
      .data[["country"]] %in% .env[["country"]],
      .data[["gender"]]  %in% .env[["gender"]],
      .data[["season_end_year"]] %in% .env[["season_end_year"]],
      .data[["tier"]] %in% .env[["tier"]]
    )

  filt_seasons_urls <- if (is.na(non_dom_league_url)) {
    seasons_urls %>%
      dplyr::filter(
        stringr::str_detect(.data[["competition_type"]], "Leagues")
      )
  } else {
    seasons_urls %>%
      dplyr::filter(
        .data[["comp_url"]] %in% .env[["non_dom_league_url"]]
      )
  }

  stopifnot("Could not find any URLs matching input parameters" = nrow(filt_seasons_urls) > 0)

  filt_seasons_urls[["stat_type"]] <- dplyr::case_when(
    stat_type == "standard" ~ "stats",
    stat_type == "keepers_adv" ~ "keepersadv",
    stat_type == "playing_time" ~ "playingtime",
    TRUE ~ stat_type
  )

  deduped_seasons_urls <- filt_seasons_urls[!duplicated(filt_seasons_urls$seasons_urls), , drop = FALSE]
  urls <- purrr::map_chr(
    1:nrow(deduped_seasons_urls),
    \(i) {
      deduped_seasons_url <- deduped_seasons_urls[i, ]
      sprintf(
        "%s/%s/%s",
        gsub("history", deduped_seasons_url[["seasons"]], dirname(deduped_seasons_url[["comp_url"]])),
        deduped_seasons_url[["stat_type"]],
        paste0(deduped_seasons_url[["seasons"]], "-", gsub("Seasons", "Stats", basename(deduped_seasons_url[["comp_url"]])))
      )
    }
  )

  fi <- purrr::insistently(
    .fb_single_league_stats,
    quiet = TRUE
  )

  fp <- purrr::possibly(
    fi,
    otherwise = tibble::tibble(),
    quiet = FALSE
  )

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
