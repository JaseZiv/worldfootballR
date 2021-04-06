#' Get match results
#'
#' Returns the game results for a given league season(s)
#'
#' @param country the three character country code
#' @param gender gender of competition, either "M" or "F"
#' @param season_end_year the year(s) the season concludes
#' @param tier the tier of the league, ie '1st' for the EPL or '2nd' for the Championship and so on
#' @param non_dom_league_url the URL for Cups and Competitions found at https://fbref.com/en/comps/
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
#' \dontrun{
#' get_match_results(country = c("ENG", "ITA", "GER"), gender = "M", season_end_year = c(2018:2021))
#' # for results from English Championship:
#' get_match_results(country = "ENG", gender = "M", season_end_year = 2021, tier = "2nd")
#' # for international friendlies:
#' non_dom <- "https://fbref.com/en/comps/218/history/Friendlies-M-Seasons"
#' get_match_results(country = "", gender = "M", season_end_year = 2018, non_dom_league_url = non_dom)
#'
#' }
get_match_results <- function(country, gender, season_end_year, tier = "1st", non_dom_league_url = NA) {
  main_url <- "https://fbref.com"
  print("Scraping match results")

  country_abbr <- country
  gender_M_F <- gender
  season_end_year_num <- season_end_year
  comp_tier <- tier
  cups_url <- non_dom_league_url

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv")

  if(is.na(cups_url)) {
    fixtures_urls <- seasons %>%
      dplyr::filter(stringr::str_detect(.data$competition_type, "Leagues")) %>%
      dplyr::filter(country %in% country_abbr,
                    gender %in% gender_M_F,
                    season_end_year %in% season_end_year_num,
                    tier %in% comp_tier,
                    !is.na(.data$fixtures_url)) %>%
      dplyr::arrange(season_end_year) %>%
      dplyr::pull(.data$fixtures_url) %>% unique()
  } else {
    fixtures_urls <- seasons %>%
      dplyr::filter(.data$comp_url %in% cups_url,
                    gender %in% gender_M_F,
                    season_end_year %in% season_end_year_num,
                    !is.na(.data$fixtures_url)) %>%
      dplyr::arrange(season_end_year) %>%
      dplyr::pull(.data$fixtures_url) %>% unique()
  }


  get_each_season_results <- function(fixture_url) {

    fixtures_page <- xml2::read_html(fixture_url)

    season_name <- fixtures_page %>% rvest::html_nodes("h2 span") %>% rvest::html_text() %>% .[1]

    season_summary <- fixtures_page %>%
      rvest::html_table() %>% .[1] %>% data.frame() %>%
      dplyr::filter(.data$Date != "")


    suppressWarnings(
      season_summary <- season_summary %>%
        dplyr::filter(.data$Time != "Time") %>%
        dplyr::mutate(Score = iconv(.data$Score, 'utf-8', 'ascii', sub=' ') %>% stringr::str_squish()) %>%
        tidyr::separate(.data$Score, into = c("HomeGoals", "AwayGoals"), sep = " ") %>%
        dplyr::mutate(HomeGoals = as.numeric(.data$HomeGoals),
                      AwayGoals = as.numeric(.data$AwayGoals),
                      Attendance = as.numeric(gsub(",", "", .data$Attendance)))
    )

    season_summary <- cbind(fixture_url, season_summary)

    if(!any(stringr::str_detect(names(season_summary), "Round"))) {
      Round <- rep(NA, nrow(season_summary))
      season_summary <- cbind(Round, season_summary)
    }

    if(!any(stringr::str_detect(names(season_summary), "Wk"))) {
      Wk <- rep(NA, nrow(season_summary))
      season_summary <- cbind(Wk, season_summary)
    }

    if(any(stringr::str_detect(names(season_summary), "xG"))) {
      season_summary <- season_summary %>%
        dplyr::select(.data$fixture_url, Round, .data$Wk, .data$Day, .data$Date, .data$Time, .data$Home, .data$HomeGoals, Home_xG=.data$xG, .data$Away, .data$AwayGoals, Away_xG=.data$xG.1, .data$Attendance, .data$Venue, .data$Referee, .data$Notes)
    } else {
      season_summary <- season_summary %>%
        dplyr::select(.data$fixture_url, Round, .data$Wk, .data$Day, .data$Date, .data$Time, .data$Home, .data$HomeGoals, .data$Away, .data$AwayGoals, .data$Attendance, .data$Venue, .data$Referee, .data$Notes)
    }

    return(season_summary)
  }

  stopifnot("Data not available for the season(s) selected" = length(fixtures_urls) > 0)

  all_results <- fixtures_urls %>%
    purrr::map_df(get_each_season_results)

  all_results <- seasons %>% dplyr::distinct(.keep_all = T) %>%
    dplyr::select(Competition_Name=.data$competition_name, Gender=.data$gender, Country=.data$country, Season_End_Year=.data$season_end_year, .data$seasons_urls, .data$fixtures_url) %>%
    dplyr::right_join(all_results, by = c("fixtures_url" = "fixture_url")) %>%
    dplyr::select(-.data$seasons_urls, -.data$fixtures_url) %>%
    dplyr::mutate(Date = lubridate::ymd(.data$Date)) %>%
    dplyr::arrange(.data$Country, .data$Competition_Name, .data$Gender, .data$Season_End_Year, .data$Wk, .data$Date, .data$Time)


  print("Match results finished scraping")

  return(all_results)
}
