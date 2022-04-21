#' Get match URLs
#'
#' Returns the URL for each match played for a given league season
#'
#' @param country the three character country code
#' @param gender gender of competition, either "M" or "F", or both
#' @param season_end_year the year the season(s) concludes
#' @param tier the tier of the league, ie '1st' for the EPL or '2nd' for the Championship and so on
#' @param non_dom_league_url the URL for Cups and Competitions found at https://fbref.com/en/comps/
#' @param time_pause the wait time (in seconds) between page loads
#'
#' @return returns a character vector of all fbref match URLs for selected competition, season and gender
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_match_urls(country = "ENG", gender = "M", season_end_year = c(2019:2021), tier = "1st")
#' non_dom <- "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons"
#' get_match_urls(country = "", gender = "M", season_end_year = 2021, non_dom_league_url = non_dom)
#' }

get_match_urls <- function(country, gender, season_end_year, tier = "1st", non_dom_league_url = NA, time_pause=2) {
  main_url <- "https://fbref.com"

  # .pkg_message("Scraping match URLs")

  country_abbr <- country
  gender_M_F <- gender
  season_end_year_num <- season_end_year
  comp_tier <- tier
  cups_url <- non_dom_league_url

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  if(is.na(cups_url)) {
    fixtures_url <- seasons %>%
      dplyr::filter(stringr::str_detect(.data$competition_type, "Leagues")) %>%
      dplyr::filter(country %in% country_abbr,
                    gender %in% gender_M_F,
                    season_end_year %in% season_end_year_num,
                    tier %in% comp_tier,
                    !is.na(fixtures_url)) %>%
      dplyr::arrange(season_end_year) %>%
      dplyr::pull(fixtures_url) %>% unique()
  } else {
    fixtures_url <- seasons %>%
      dplyr::filter(.data$comp_url %in% cups_url,
                    gender %in% gender_M_F,
                    season_end_year %in% season_end_year_num,
                    !is.na(fixtures_url)) %>%
      dplyr::arrange(season_end_year) %>%
      dplyr::pull(fixtures_url) %>% unique()
  }

  time_wait <- time_pause

  get_each_seasons_urls <- function(fixture_url, time_pause=time_wait) {

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    match_report_urls <- xml2::read_html(fixture_url) %>%
      rvest::html_nodes("td.left~ .left+ .left a") %>%
      rvest::html_attr("href") %>%
      paste0(main_url, .) %>% unique()

    return(match_report_urls)
  }


  all_seasons_match_urls <- fixtures_url %>%
    purrr::map(get_each_seasons_urls) %>%
    unlist()

  history_index <- grep("-History", all_seasons_match_urls)
  if(length(history_index) != 0) {
    all_seasons_match_urls <- all_seasons_match_urls[-history_index]
  }


  # .pkg_message("Match URLs scrape completed")

  return(all_seasons_match_urls)

}
