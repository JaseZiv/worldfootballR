#' Get team transfers
#'
#' Returns all transfer arrivals and departures for a given team season
#'
#' @param team_url transfermarkt.com team url for a season
#'
#' @return returns a dataframe of all team transfers
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' bayern <- tm_team_transfers(
#' team_url = "https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2020"
#' )
#' # can even do it for a number of teams:
#' team_urls <- tm_league_team_urls(country_name = "England", start_year = 2020)
#' epl_xfers_2020 <- tm_team_transfers(team_url = team_urls)
#' }
tm_team_transfers <- function(team_url) {

  print("Scraping team transfer arrivals and departures. Please acknowledge transfermarkt.com as the data source")

  each_team_xfer <- function(each_team_url) {

    xfers_url <- gsub("startseite", "transfers", each_team_url)

    team_page <- xml2::read_html(xfers_url)

    team_name <- team_page %>% rvest::html_nodes("h1") %>% rvest::html_text() %>% stringr::str_squish()
    league <- team_page %>% rvest::html_nodes(".hauptpunkt a") %>% rvest::html_text() %>% stringr::str_squish()
    country <- team_page %>% rvest::html_nodes(".mediumpunkt img") %>% rvest::html_attr("title")


    tab_box <- team_page %>% rvest::html_nodes(".box")
    # need to isolate the arrivals and departures tables
    tab_names <- tab_box %>% rvest::html_nodes("h2") %>% rvest::html_text() %>% stringr::str_squish()
    tab_box <- tab_box[which(tab_names %in% c("Arrivals", "Departures"))]
    both_tabs <- tab_box %>% rvest::html_nodes(".responsive-table")

    # create output for team of both arrivals and departures
    team_df <- data.frame()

    for(i in 1:length(tab_box)) {
      each_tab <- both_tabs[i] %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children()

      player_df <- data.frame()
      for(j in 1:length(each_tab)) {
        tryCatch({player_df[j, "transfer_type"] <- tab_box[i] %>% rvest::html_nodes("h2") %>% rvest::html_text() %>% stringr::str_squish()},
                 error = function(e) {player_df[j, "transfer_type"] <- NA_character_})
        tryCatch({player_df[j, "player_name"] <- each_tab[j] %>% rvest::html_nodes(".spielprofil_tooltip") %>% rvest::html_text()},
                 error = function(e) {player_df[j, "player_name"] <- NA_character_})
        tryCatch({player_df[j, "player_position"] <- each_tab[j] %>% rvest::html_nodes(".ma_pos+ td tr+ tr td") %>% rvest::html_text()},
                 error = function(e) {player_df[j, "player_position"] <- NA_character_})
        tryCatch({player_df[j, "player_age"] <- each_tab[j] %>% rvest::html_nodes("td.zentriert:nth-child(3)") %>% rvest::html_text()},
                 error = function(e) {player_df[j, "player_age"] <- NA_character_})
        tryCatch({player_df[j, "player_nationality"] <- each_tab[j] %>% rvest::html_nodes(".zentriert .flaggenrahmen") %>% .[1] %>% rvest::html_attr("title")},
                 error = function(e) {player_df[j, "player_nationality"] <- NA_character_})
        tryCatch({player_df[j, "club_2"] <- each_tab[j] %>% rvest::html_nodes(".hauptlink .vereinprofil_tooltip") %>% rvest::html_text()},
                 error = function(e) {player_df[j, "club_2"] <- NA_character_})
        tryCatch({player_df[j, "league_2"] <- each_tab[j] %>% rvest::html_nodes(".flaggenrahmen+ a") %>% rvest::html_text()},
                 error = function(e) {player_df[j, "league_2"] <- NA_character_})
        tryCatch({player_df[j, "country_2"] <- each_tab[j] %>% rvest::html_nodes(".inline-table .flaggenrahmen") %>% rvest::html_attr("alt")},
                 error = function(e) {player_df[j, "country_2"] <- NA_character_})
        tryCatch({player_df[j, "transfer_fee"] <- each_tab[j] %>% rvest::html_nodes(".rechts a") %>% rvest::html_text()},
                 error = function(e) {player_df[j, "transfer_fee"] <- NA_character_})
        tryCatch({ player_df[j, "is_loan"] <- grepl("loan", player_df[j, "transfer_fee"], ignore.case = T)},
                 error = function(e) {player_df[j, "is_loan"] <- NA_character_})
        tryCatch({player_df[j, "transfer_fee_dup"] <- player_df[j, "transfer_fee"]},
                 error = function(e) {player_df[j, "transfer_fee_dup"] <- NA_character_})
        if(length(each_tab[j] %>% rvest::html_nodes(".rechts.hauptlink a i") %>% rvest::html_text()) == 0) {
          player_df[j, "transfer_fee_notes1"] <- NA_character_
        } else {
          tryCatch({player_df[j, "transfer_fee_notes1"] <- each_tab[j] %>% rvest::html_nodes(".rechts.hauptlink a i") %>% rvest::html_text()},
                   error = function(e) {player_df[j, "transfer_fee_notes1"] <- NA_character_})
        }

      }
      team_df <- dplyr::bind_rows(team_df, player_df)
    }

    # add metadata
    team_df <- cbind(team_name, league, country, team_df)

    # cleaning up final output data
    team_df <- team_df %>%
      dplyr::mutate(transfer_fee = ifelse(stringr::str_detect(.data$transfer_fee_dup, "Loan fee:"), .data$transfer_fee_notes1, .data$transfer_fee)) %>%
      dplyr::mutate(transfer_fee = mapply(.convert_value_to_numeric, euro_value = .data$transfer_fee)) %>%
      dplyr::mutate(transfer_fee_dup = ifelse(is.na(.data$transfer_fee), .data$transfer_fee_dup, NA_character_),
                    transfer_fee_dup = gsub("End of loan", "End of loan ", .data$transfer_fee_dup)) %>%
      dplyr::rename(transfer_notes = .data$transfer_fee_dup) %>%
      dplyr::select(-.data$transfer_fee_notes1)

    return(team_df)
  }

  final_output <- team_url %>%
    purrr::map_df(each_team_xfer)

  return(final_output)
}

