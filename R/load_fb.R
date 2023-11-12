#' Load match results
#'
#' Loading version of \code{get_match_results}
#' Returns the game results for a given league season(s) from FBref
#'
#' @param country the three character country code
#' @param gender gender of competition, either "M" or "F"
#' @param season_end_year the year(s) the season concludes
#' @param tier the tier of the league, ie '1st' for the EPL or '2nd' for the Championship and so on
#'
#' @return returns a dataframe with the results of the competition, season and gender
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \donttest{
#' try({
#' df <- load_match_results(
#' country = c("ITA"), gender = "M", season_end_year = 2021, tier = "1st"
#' )
#' # for results from English 1st div for men and women:
#' df <- load_match_results(
#' country = "ENG", gender = c("M", "F"), season_end_year = 2021, tier = "1st"
#' )
#' })
#' }
load_match_results <- function(country, gender, season_end_year, tier) {

  dat_urls <- paste0("https://github.com/JaseZiv/worldfootballR_data/releases/download/match_results/", country, "_match_results.rds")

  # collect_date <- .file_reader("https://github.com/JaseZiv/worldfootballR_data/blob/master/data/match_results/scrape_time_match_results.rds?raw=true")
  dat_df <- dat_urls %>% purrr::map_df(.file_reader)

  if(nrow(dat_df) == 0) {
    cli::cli_alert("Data not loaded. Please check parameters.")
  } else {
    dat_df <- dat_df %>%
      dplyr::filter(.data[["Country"]] %in% country,
                    .data[["Gender"]] %in% gender,
                    .data[["Season_End_Year"]] %in% season_end_year,
                    .data[["Tier"]] %in% tier) %>%
      dplyr::select(-.data[["Tier"]])

    cli::cli_alert("Data last updated {attr(dat_df, 'scrape_timestamp')} UTC")
  }

  return(dat_df)

}


#' Load match competition results
#'
#' Returns the game results for a competition(s),
#' ie League cups or international competitions from FBref.
#' comp_name comes from https://github.com/JaseZiv/worldfootballR_data/tree/master/data/match_results_cups#readme
#'
#' @param comp_name the three character country code
#'
#' @return returns a dataframe with the results of the competition name
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \donttest{
#' try({
#' df <- load_match_comp_results(
#' comp_name = "Coppa Italia"
#' )
#' # for multiple competitions:
#' cups <- c("FIFA Women's World Cup",
#'           "FIFA World Cup")
#' df <- load_match_comp_results(
#' comp_name = cups
#' )
#' })
#' }
load_match_comp_results <- function(comp_name) {

  f_name <- janitor::make_clean_names(comp_name)

  dat_urls <- paste0("https://github.com/JaseZiv/worldfootballR_data/releases/download/match_results_cups/", f_name, "_match_results.rds")

  dat_df <- dat_urls %>% purrr::map_df(.file_reader)

  if(nrow(dat_df) == 0) {
    cli::cli_alert("Data not loaded. Please check parameters.")
  } else {
    dat_df <- dat_df %>%
      # dplyr::filter(.data[["Country"]] %in% country,
      #               .data[["Gender"]] %in% gender,
      #               .data[["Season_End_Year"]] %in% season_end_year,
      #               .data[["Tier"]] %in% tier) %>%
      dplyr::select(-.data[["Tier"]])

    cli::cli_alert("Data last updated {attr(dat_df, 'scrape_timestamp')} UTC")
  }

  return(dat_df)

}


#' Load Big 5 Euro League Season Stats
#'
#' Loading version of \code{fb_big5_advanced_season_stats}
#' Returns data frame of selected statistics for seasons of the big 5 Euro leagues, for either
#' whole team or individual players.
#' Multiple seasons can be passed to the function, but only one `stat_type` can be selected
#'
#' @param season_end_year the year(s) the season concludes
#' @param stat_type the type of team statistics the user requires
#' @param team_or_player result either summarised for each team, or individual players
#'
#' The statistic type options (stat_type) include:
#'
#' \emph{"standard"}, \emph{"shooting"}, \emph{"passing"}, \emph{"passing_types"},
#' \emph{"gca"}, \emph{"defense"}, \emph{"possession"}, \emph{"playing_time"},
#' \emph{"misc"}, \emph{"keepers"}, \emph{"keepers_adv"}
#'
#' @return returns a dataframe of a selected team or player statistic type for a selected season(s)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \donttest{
#' try({
#' df <- load_fb_big5_advanced_season_stats(
#' season_end_year = c(2018:2022), stat_type = "defense", team_or_player = "player"
#' )
#'
#' df <- load_fb_big5_advanced_season_stats(
#' season_end_year = 2022, stat_type = "defense", team_or_player = "player"
#' )
#' })
#' }

load_fb_big5_advanced_season_stats <- function(season_end_year = NA, stat_type, team_or_player) {

  dat_url <- paste0("https://github.com/JaseZiv/worldfootballR_data/releases/download/fb_big5_advanced_season_stats/big5_", team_or_player, "_", stat_type, ".rds")
  # collect_date <- .file_reader("https://github.com/JaseZiv/worldfootballR_data/blob/master/data/fb_big5_advanced_season_stats/scrape_time_big5_advanced_season_stats.rds?raw=true")

  dat_df <- tryCatch(.file_reader(dat_url), error = function(e) data.frame())

  if(nrow(dat_df) == 0) {
    cli::cli_alert("Data not available. Check you have the correct stat_type or team_or_player.")
  } else {

    if(!all(is.na(season_end_year))) {
      dat_df <- dat_df %>%
        dplyr::filter(.data[["Season_End_Year"]] %in% season_end_year)
    }

    cli::cli_alert("Data last updated {attr(dat_df, 'scrape_timestamp')} UTC")
  }

  return(dat_df)

}

#' Load pre-saved FBref match shooting event data
#'
#' Loading version of \code{fb_match_shooting}. Only some leagues available.
#'
#' @inheritParams load_match_results
#' @importFrom purrr map_dfr
#' @importFrom cli cli_alert
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @return returns a dataframe
#' @examples
#' \donttest{
#' try({
#' load_fb_match_shooting(
#'   country = "ENG",
#'   gender = "M",
#'   tier = "1st"
#' )
#'
#' load_fb_match_shooting(
#'   country = c("ITA", "ESP"),
#'   gender = "M",
#'   tier = "1st",
#'   season_end_year = 2019
#' )
#' })
#' }
#' @export
load_fb_match_shooting <- function(country, gender, tier, season_end_year = NA) {

  ## plural because there could be multiple
  urls <- sprintf(
    "https://github.com/JaseZiv/worldfootballR_data/releases/download/fb_match_shooting/%s_%s_%s_match_shooting.rds",
    country,
    gender,
    tier
  )

  res <- purrr::map_dfr(urls, .file_reader)

  if(nrow(res) == 0) {
    cli::cli_alert("Data not loaded. Please check parameters.")
    return(res)
  } else {
    cli::cli_alert("Data last updated {attr(res, 'scrape_timestamp')} UTC")
  }

  if (!all(is.na(season_end_year))) {
    res <- res %>%
      dplyr::filter(.data[["Season_End_Year"]] %in% season_end_year)
  }

  res

}

#' Load pre-saved FBref match summary data
#'
#' Loading version of \code{fb_match_summary}. Only some leagues available.
#'
#' @inheritParams load_match_results
#' @importFrom purrr map_dfr
#' @importFrom cli cli_alert
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @return returns a dataframe
#' @examples
#' \donttest{
#' try({
#' load_fb_match_summary(
#'   country = "ENG",
#'   gender = "M",
#'   tier = "1st"
#' )
#'
#' load_fb_match_summary(
#'   country = c("ITA", "ESP"),
#'   gender = "M",
#'   tier = "1st",
#'   season_end_year = 2019
#' )
#' })
#' }
#' @export
load_fb_match_summary <- function(country, gender, tier, season_end_year = NA) {

  urls <- sprintf(
    "https://github.com/JaseZiv/worldfootballR_data/releases/download/fb_match_summary/%s_%s_%s_match_summary.rds",
    country,
    gender,
    tier
  )

  res <- purrr::map_dfr(urls, .file_reader)

  if(nrow(res) == 0) {
    cli::cli_alert("Data not loaded. Please check parameters.")
    return(res)
  } else {
    cli::cli_alert("Data last updated {attr(res, 'scrape_timestamp')} UTC")
  }

  if (!all(is.na(season_end_year))) {
    res <- res %>%
      dplyr::filter(.data[["Season_End_Year"]] %in% season_end_year)
  }

  res

}

#' Load pre-saved FBref match advanced stats
#'
#' Loading version of \code{fb_advanced_match_stats}. Only some leagues available.
#'
#' @inheritParams load_match_results
#' @inheritParams fb_advanced_match_stats
#' @importFrom purrr map_dfr
#' @importFrom cli cli_alert
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @return returns a dataframe
#' @examples
#' \donttest{
#' try({
#' load_fb_advanced_match_stats(
#'   country = "ENG",
#'   gender = "M",
#'   tier = "1st",
#'   stat_type = "summary",
#'   team_or_player = "player"
#' )
#'
#' load_fb_advanced_match_stats(
#'   country = c("ITA", "ESP"),
#'   gender = "M",
#'   tier = "1st",
#'   season_end_year = 2023,
#'   stat_type = "defense",
#'   team_or_player = "player"
#' )
#' })
#' }
load_fb_advanced_match_stats <- function(country, gender, tier, stat_type, team_or_player, season_end_year = NA) {

  urls <- sprintf(
    "https://github.com/JaseZiv/worldfootballR_data/releases/download/fb_advanced_match_stats/%s_%s_%s_%s_%s_advanced_match_stats.rds",
    country,
    gender,
    tier,
    stat_type,
    team_or_player
  )

  res <- purrr::map_dfr(urls, .file_reader)

  if(nrow(res) == 0) {
    cli::cli_alert("Data not loaded. Please check parameters.")
    return(res)
  } else {
    cli::cli_alert("Data last updated {attr(res, 'scrape_timestamp')} UTC")
  }

  if (!all(is.na(season_end_year))) {
    res <- res %>%
      dplyr::filter(.data[["Season_End_Year"]] %in% season_end_year)
  }

  res

}
