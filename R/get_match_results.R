#' Get match results
#'
#' Returns the game results for a given league season(s)
#'
#' @param country the three character country code
#' @param gender gender of competition, either "M" or "F"
#' @param season_end_year the year(s) the season concludes
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
#' }

get_match_results <- function(country, gender, season_end_year) {

  print("Scraping match results")

  country_abbr <- country
  gender_M_F <- gender
  season_end_year_num <- season_end_year


  main_url <- "https://fbref.com"

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/league_seasons/all_tier1_season_URLs.csv")

  seasons <- seasons %>%
    dplyr::filter(country %in% country_abbr,
                  gender %in% gender_M_F,
                  season_end_year %in% season_end_year_num,
                  !is.na(.data$fixtures_url)) %>%
    dplyr::arrange(season_end_year)

  fixtures_urls <- seasons %>%
    dplyr::pull(.data$fixtures_url)


  get_each_season_results <- function(fixture_url) {

    fixtures_page <- xml2::read_html(fixture_url)

    season_name <- fixtures_page %>% rvest::html_nodes("h2 span") %>% rvest::html_text() %>% .[1]

    season_summary <- fixtures_page %>%
      rvest::html_table() %>% .[1] %>% data.frame() %>%
      dplyr::filter(.data$Date != "")


    suppressWarnings(
      season_summary <- season_summary %>%
        tidyr::separate(.data$Score, into = c("HomeGoals", "AwayGoals"), sep = "–") %>%
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

  all_results <- seasons %>%
    dplyr::select(Competition_Name=.data$competition_name, Gender=.data$gender, Country=.data$country, Season_End_Year=.data$season_end_year, .data$seasons_urls, .data$fixtures_url) %>%
    dplyr::left_join(all_results, by = c("fixtures_url" = "fixture_url")) %>%
    dplyr::select(-.data$seasons_urls, -.data$fixtures_url) %>%
    dplyr::mutate(Date = lubridate::ymd(.data$Date)) %>%
    dplyr::arrange(.data$Country, .data$Competition_Name, .data$Gender, .data$Season_End_Year, .data$Wk, .data$Date, .data$Time)


  print("Match results finished scraping")

  return(all_results)
}
