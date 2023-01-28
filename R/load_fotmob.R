#' @importFrom purrr possibly map_dfr
#' @importFrom cli cli_alert
.load_fotmob <- function(country = NULL, league_name = NULL, league_id = NULL, cached = NULL, url_stem) {

  fotmob_urls <- .fotmob_get_league_ids(
    cached = cached,
    country = country,
    league_name = league_name,
    league_id = league_id
  )

  url_format <- sprintf(
    "https://github.com/JaseZiv/worldfootballR_data/releases/download/%s.rds",
    url_stem
  )

  urls <- sprintf(
    url_format,
    fotmob_urls$id
  )

  fp <- purrr::possibly(
    .file_reader,
    quiet = FALSE,
    otherwise = data.frame()
  )

  res <- purrr::map_dfr(urls, fp)

  if(nrow(res) == 0) {
    cli::cli_alert("Data not loaded. Please check parameters")
  } else {
    ## when there are multiple data sets loaded in, seems like this is the attribute for the first
    cli::cli_alert("Data last updated {attr(res, 'scrape_timestamp')} UTC")
  }
  res
}

#' Load pre saved fotmob match details
#'
#' Loading version of \code{fotmob_get_match_details}, but for all seasons for which
#' data is available, not just the current season.
#' Note that fotmob only has match details going back to the 2020-21 season for most leagues.
#'
#' @inheritParams fotmob_get_league_matches
#'
#' @return returns a dataframe of league matches
#' @importFrom rlang maybe_missing
#'
#' @examples
#' \dontrun{
#' try({
#' # one league
#' load_fotmob_match_details(
#'   country = "ENG",
#'   league_name = "Premier League"
#' )
#'
#' ## this is the same output format as the following
#' fotmob_get_match_details(match_id = 3411352)
#'
#' # one league, by id
#' load_fotmob_match_details(league_id = 47)
#'
#' # multiple leagues (could also use ids)
#' load_fotmob_match_details(
#'   country =     c("ENG",            "ESP"   ),
#'   league_name = c("Premier League", "LaLiga")
#' )
#' })
#' }
#' @export
load_fotmob_match_details <- function(country, league_name, league_id, cached = TRUE) {
  .load_fotmob(
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL),
    cached = cached,
    url_stem = "fotmob_match_details/%s_match_details"
  )
}

#' Load pre saved fotmob match ids by date
#'
#' Loading version of \code{fotmob_get_matches_by_date}. Goes back to August 2017.
#'
#' @inheritParams load_fotmob_match_details
#' @return returns a dataframe of league match ids
#' @importFrom rlang maybe_missing
#' @examples
#' \dontrun{
#' try({
#' ## just load match ids
#' load_fotmob_matches_by_date(
#'   country = "ENG",
#'   league_name = "Premier League"
#' )
#'
#' ## can also do it for multiple leagues
#' load_fotmob_matches_by_date(
#'   country =     c("ENG",            "ESP"   ),
#'   league_name = c("Premier League", "LaLiga")
#' )
#' })
#' }
#' @export
load_fotmob_matches_by_date <- function(country, league_name, league_id, cached = TRUE) {
  .load_fotmob(
    country = rlang::maybe_missing(country, NULL),
    league_name = rlang::maybe_missing(league_name, NULL),
    league_id = rlang::maybe_missing(league_id, NULL),
    cached = cached,
    url_stem = "fotmob_matches_by_date/%s_matches_by_date"
  )
}
