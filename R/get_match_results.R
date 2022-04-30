#' Get single match results
#'
#' Internal function used in get_match_results
#'
#' @param fixture_url URL of the league season's fixtures and results page
#' @param time_pause the wait time (in seconds) between page loads
#'
#' @return returns a raw dataframe with the results of the league season
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \donttest{
#' df <- .get_each_season_results(fixture_url =
#' "https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures"
#' )
#'
#' }

.get_each_season_results <- function(fixture_url, time_pause=2) {
  main_url <- "https://fbref.com"

  # pb$tick()

  # put sleep in as per new user agreement on FBref
  Sys.sleep(time_pause)

  fixtures_page <- xml2::read_html(fixture_url)

  season_name <- fixtures_page %>% rvest::html_nodes("h2 span") %>% rvest::html_text() %>% .[1]

  tab_holder <- fixtures_page %>%
    rvest::html_node(".stats_table tbody") %>% rvest::html_nodes("tr")

  spacer_idx <- !grepl("spacer partial", xml2::xml_attrs(tab_holder))

  tab_holder <- tab_holder[spacer_idx]

  idx_rm <- grep("thead", xml2::xml_attrs(tab_holder))

  if(length(idx_rm) != 0) {
    tab_holder <- tab_holder[-idx_rm]
  }

  season_summary <- tryCatch(fixtures_page %>%
                               rvest::html_table() %>% .[1] %>% data.frame(), error = function(e) data.frame())

  # error handling - The first available Iranian season has something weird in the HTML, meaning the above code wont work
  # (https://fbref.com/en/comps/64/741/schedule/2014-2015-Persian-Gulf-Pro-League-Scores-and-Fixtures)
  if(nrow(season_summary) == 0) {
    season_summary <- fixtures_page %>% rvest::html_node(".stats_table") %>%
      rvest::html_table() %>% data.frame()
  }

  season_summary <- season_summary[spacer_idx,]

  if(length(idx_rm) != 0) {
    season_summary <- season_summary[-idx_rm,]
  }


  get_url <- function(tab_element) {
    a <- tab_element %>% rvest::html_node(xpath='.//*[@data-stat="match_report"]//a') %>% rvest::html_attr("href")
    if(is.na(a) || length(a) == 0) {
      a <- NA_character_
    } else {
      a <-  paste0(main_url, a)
    }

    return(a)
  }

  match_urls <- purrr::map_chr(tab_holder, get_url)
  # match_urls <- match_urls[!duplicated(match_urls, incomparables = NA)]

  suppressWarnings(
    season_summary <- season_summary %>%
      dplyr::filter(is.na(.data$Time) | .data$Time != "Time") %>%
      dplyr::mutate(Score = iconv(.data$Score, 'utf-8', 'ascii', sub=' ') %>% stringr::str_squish()) %>%
      tidyr::separate(.data$Score, into = c("HomeGoals", "AwayGoals"), sep = " ") %>%
      dplyr::mutate(HomeGoals = as.numeric(.data$HomeGoals),
                    AwayGoals = as.numeric(.data$AwayGoals),
                    Attendance = as.numeric(gsub(",", "", .data$Attendance)))
  )

  season_summary <- season_summary %>%
    dplyr::mutate(HomeGoals = ifelse(is.na(.data$HomeGoals) & !is.na(.data$AwayGoals), .data$AwayGoals, .data$HomeGoals),
                  AwayGoals = ifelse(is.na(.data$AwayGoals) & !is.na(.data$HomeGoals), .data$HomeGoals, .data$AwayGoals))

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
      dplyr::select(.data$fixture_url, Round, .data$Wk, .data$Day, .data$Date, .data$Time, .data$Home, .data$HomeGoals, Home_xG=.data$xG, .data$Away, .data$AwayGoals, Away_xG=.data$xG.1, .data$Attendance, .data$Venue, .data$Referee, .data$Notes) %>%
      dplyr::mutate(Home_xG = as.numeric(.data$Home_xG),
                    Away_xG = as.numeric(.data$Away_xG))
  } else {
    season_summary <- season_summary %>%
      dplyr::select(.data$fixture_url, Round, .data$Wk, .data$Day, .data$Date, .data$Time, .data$Home, .data$HomeGoals, .data$Away, .data$AwayGoals, .data$Attendance, .data$Venue, .data$Referee, .data$Notes)
  }

  season_summary <- season_summary %>%
    dplyr::mutate(Wk = as.character(.data$Wk)) %>%
    dplyr::mutate(MatchURL = match_urls)

  return(season_summary)
}




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
#' \donttest{
#' df <- get_match_results(country = c("ITA"), gender = "M", season_end_year = 2021)
#' # for results from English Championship:
#' df <- get_match_results(country = "ENG", gender = "M", season_end_year = 2021, tier = "2nd")
#' # for international friendlies:
#'
#' }

get_match_results <- function(country, gender, season_end_year, tier = "1st", non_dom_league_url = NA) {
  main_url <- "https://fbref.com"
  # .pkg_message("Scraping match results")

  country_abbr <- country
  gender_M_F <- gender
  season_end_year_num <- season_end_year
  comp_tier <- tier
  cups_url <- non_dom_league_url

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

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

  stopifnot("Data not available for the season(s) selected" = length(fixtures_urls) > 0)

  # create the progress bar with a progress function.
  # pb <- progress::progress_bar$new(total = length(fixtures_urls))

  all_results <- fixtures_urls %>%
    purrr::map_df(.get_each_season_results)

  all_results <- seasons %>%
    dplyr::select(Competition_Name=.data$competition_name, Gender=.data$gender, Country=.data$country, Season_End_Year=.data$season_end_year, .data$seasons_urls, .data$fixtures_url) %>%
    dplyr::right_join(all_results, by = c("fixtures_url" = "fixture_url")) %>%
    dplyr::select(-.data$seasons_urls, -.data$fixtures_url) %>%
    dplyr::mutate(Date = lubridate::ymd(.data$Date)) %>%
    dplyr::arrange(.data$Country, .data$Competition_Name, .data$Gender, .data$Season_End_Year, .data$Wk, .data$Date, .data$Time) %>% dplyr::distinct(.keep_all = T)


  # .pkg_message("Match results finished scraping")

  return(all_results)
}
