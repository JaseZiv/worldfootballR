#' Team transfer balances
#'
#' Returns all team's transfer aggregated performances for a chosen league season
#'
#' @param country_name the country of the league's players
#' @param start_year the start year of the season (2020 for the 20/21 season)
#' @param league_url league url from transfermarkt.com. To be used when country_name not avalilable in main function
#'
#' @return returns a dataframe of the summarised financial transfer performance of all teams for a league season
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tm_team_transfer_balances(country_name="England", start_year="2020")
#' odd_league <- "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3"
#' tm_team_transfer_balances(start_year="2020", league_url=odd_league)
#' }

tm_team_transfer_balances <- function(country_name, start_year, league_url=NA) {

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

    season_url <- paste0(league_url, "?saison_id=", start_year)

  }

  season_url <- gsub("startseite", "transfers", season_url)

  page <- xml2::read_html(season_url)

  team_transfers <- page %>% rvest::html_nodes(".large-8") %>% rvest::html_nodes(".box")

  tryCatch({country_name <- page %>%
    rvest::html_nodes(".profilheader") %>%
    rvest::html_node("img") %>%
    rvest::html_attr("alt") %>% .[!is.na(.)]}, error = function(e) {country_name <- NA_character_})

  league_name <- page %>% rvest::html_nodes(".spielername-profil") %>% rvest::html_text()

  tryCatch({season <- team_transfers[1] %>%
    rvest::html_node(".table-header") %>%
    rvest::html_text() %>%
    stringr::str_squish() %>% gsub("Transfers ", "", .)}, error = function(e) {country_name <- NA_character_})

  all_df <- data.frame()

  for(i in team_transfers) {
    team_name <- i %>%
      rvest::html_nodes(".table-header") %>%
      rvest::html_text()

    if(length(team_name) == 0 || grepl("Transfer", team_name)) {
      df <- data.frame()
    } else {
      val_out <- i %>% rvest::html_nodes(".transfer-einnahmen-ausgaben") %>% rvest::html_text() %>% .[1] %>% stringr::str_squish()
      val_in <- i %>% rvest::html_nodes(".transfer-einnahmen-ausgaben") %>% rvest::html_text() %>% .[2] %>% stringr::str_squish()
      age_in <- i %>% rvest::html_nodes(".transfer-zusatzinfo-alter") %>% rvest::html_text() %>% .[1] %>% stringr::str_squish()
      age_out <- i %>% rvest::html_nodes(".transfer-zusatzinfo-alter") %>% rvest::html_text() %>% .[2] %>% stringr::str_squish()
      df <- data.frame(country=country_name, league=league_name, season=season, team=team_name, expenditure_euros=val_out, income_euros=val_in, avg_age_out=age_out, avg_age_in=age_in)
    }
    all_df <- dplyr::bind_rows(all_df, df)

  }

  all_df <- all_df %>%
    dplyr::mutate(expenditure_euros = gsub("Expenditure: ", "", .data$expenditure_euros),
                  income_euros = gsub("Income: ", "", .data$income_euros),
                  avg_age_out = as.numeric(gsub(".*:","", .data$avg_age_out)),
                  avg_age_in = as.numeric(gsub(".*:","", .data$avg_age_in))) %>%
    dplyr::mutate(expenditure_euros = mapply(.convert_value_to_numeric, .data$expenditure_euros),
                  income_euros = mapply(.convert_value_to_numeric, .data$income_euros))


  return(all_df)

}
