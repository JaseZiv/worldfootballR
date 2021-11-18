#' Get weekly league table
#'
#' Returns the league table for each chosen matchday from transfermarkt
#'
#' @param country_name the country of the league's players
#' @param start_year the start year of the season (2020 for the 20/21 season)
#' @param matchday the matchweek number. Can be a vector of matchdays
#' @param league_url league url from transfermarkt.com. To be used when country_name not avalilable in main function
#'
#' @return returns a dataframe of the table for a selected league and matchday
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tm_matchday_table(country_name="England", start_year="2020", matchday=1)
#' tm_matchday_table(country_name="England", start_year="2020", matchday=c(1:34))
#' }
tm_matchday_table <- function(country_name, start_year, matchday, league_url=NA) {

  # .pkg_message("Scraping matchday table. Please acknowledge transfermarkt.com as the data source")

  main_url <- "https://www.transfermarkt.com"

  if(is.na(league_url)) {
    meta_df <- read.csv(url("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"),
                        stringsAsFactors = F)

    tryCatch({meta_df_seasons <- meta_df %>%
      dplyr::filter(.data$country %in% country_name, .data$season_start_year %in% start_year)}, error = function(e) {meta_df_seasons <- data.frame()})

    if(nrow(meta_df_seasons) == 0) {
      stop(glue::glue("Country {country_name} or season {start_year} not found. Check that the country and season exists at https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"))
    }

    season_url <- meta_df_seasons$season_urls

  } else {
    tryCatch({league_page <- xml2::read_html(league_url)}, error = function(e) {league_page <- c()})

    tryCatch({country_name <- league_page %>%
      rvest::html_nodes(".profilheader") %>%
      rvest::html_node("img") %>%
      rvest::html_attr("alt") %>% .[!is.na(.)]}, error = function(e) {country_name <- NA_character_})

    if(length(league_page) == 0) {
      stop(glue::glue("League URL(s) {league_url} not found. Please check transfermarkt.com for the correct league URL"))
    }

    season_url <- league_url

  }

  season_url <- gsub("startseite", "spieltagtabelle", season_url)


  all_matchdays <- data.frame()

  for(each_matchday in matchday) {

    # .pkg_message("Extracting league table for matchday {each_matchday}...")

    if(is.na(league_url)) {
      matchday_url <- paste0(season_url, "&spieltag=", each_matchday)
    } else {
      matchday_url <- paste0(season_url, "?saison_id=", start_year, "&spieltag=", each_matchday)
    }

    tab <- xml2::read_html(matchday_url)

    league_name <- tab %>% rvest::html_node("h1") %>% rvest::html_text()

   weekly_table <- tab %>%
      rvest::html_nodes(".box")

    weekly_table <- weekly_table[-1]

    all_lens <- c()

    for(i in weekly_table) {
      len <- i %>% rvest::html_nodes("th") %>% length()
      all_lens <- c(all_lens, len)
    }


    idx <- which(all_lens == 9)

    if(length(idx) == 1) {
      matchday_table <- weekly_table[idx] %>% rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame()
    } else {
      matchday_table_1 <- weekly_table[idx[1]] %>% rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame()
      matchday_table_2 <- weekly_table[idx[2]] %>% rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame()

      matchday_table <- rbind(matchday_table_1, matchday_table_2)
    }


    matchday_table <- matchday_table %>%
      dplyr::mutate(Matchday = each_matchday,
                    Country=country_name,
                    League = league_name) %>%
      dplyr::select(.data$Country, .data$League, .data$Matchday, Rk=.data$`X.`, Team=.data$club.1,
                    P=.data$Var.4, .data$W, .data$D, .data$L, .data$Goals, G_Diff=.data$`X...`, .data$Pts) %>%
      tidyr::separate(.data$Goals, into = c("GF", "GA"), sep = ":") %>%
      dplyr::mutate(GF = as.numeric(.data$GF),
                    GA = as.numeric(.data$GA)) %>% suppressWarnings()

    matchday_table <- matchday_table %>%
      dplyr::mutate_at(.vars = c("P", "W", "D", "L", "GF", "GA", "G_Diff", "Pts"), as.numeric) %>% suppressWarnings()

    matchday_table <- matchday_table %>%
      janitor::clean_names() %>%
      dplyr::rename(squad = .data$team)

    all_matchdays <- rbind(all_matchdays, matchday_table)
  }

  return(all_matchdays)

}

