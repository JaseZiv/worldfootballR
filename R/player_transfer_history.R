#' Get player transfer history
#'
#' Returns data frame of player transfer history from transfermarkt.com
#' Function used as internal function to player_transfer_history()
#'
#' @param player_url the single player url from transfermarket
#'
#' @return returns a dataframe of player transfers
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' single_player_transfer_history(player_url)
#' }
single_player_transfer_history <- function(player_url) {

  page <- xml2::read_html(player_url)

  player_name <- page %>% rvest::html_node("h1") %>% rvest::html_text() %>% stringr::str_squish()

  all_transfers_player <- page %>%
    rvest::html_node(".transferhistorie") %>%
    rvest::html_nodes(".responsive-table") %>%
    rvest::html_nodes("tbody")


  all_transfer_rows <- all_transfers_player %>% rvest::html_nodes("tr")

  each_player_df <- data.frame()

  for(each_row in 1:length(all_transfer_rows)) {
    tryCatch(season <- all_transfer_rows[each_row] %>% rvest::html_nodes(".zentriert") %>% .[1] %>% rvest::html_text(), error = function(e) NA_character_)
    if(rlang::is_empty(season)) {
      each_row_df <- data.frame()
    } else {
      tryCatch(transfer_date <- all_transfer_rows[each_row] %>% rvest::html_nodes(".zentriert") %>% .[2] %>% rvest::html_text(), error = function(e) NA_character_)

      tryCatch(country_flags <- all_transfer_rows[each_row] %>% rvest::html_nodes(".flagge"), error = function(e) NA)
      tryCatch(country_from <- xml2::xml_attrs(xml2::xml_child(country_flags[[1]], 1))[["title"]], error = function(e) NA_character_)
      tryCatch(country_to <- xml2::xml_attrs(xml2::xml_child(country_flags[[2]], 1))[["title"]], error = function(e) NA_character_)

      tryCatch(team_from <- all_transfer_rows[each_row] %>% rvest::html_nodes(".vereinsname") %>% .[1] %>% rvest::html_text(), error = function(e) NA_character_)
      tryCatch(team_to <- all_transfer_rows[each_row] %>% rvest::html_nodes(".vereinsname") %>% .[2] %>% rvest::html_text(), error = function(e) NA_character_)

      tryCatch(market_value <- all_transfer_rows[each_row] %>% rvest::html_nodes(".zelle-mw") %>% rvest::html_text(), error = function(e) NA_character_)
      tryCatch(transfer_value <- all_transfer_rows[each_row] %>% rvest::html_nodes(".zelle-abloese") %>% rvest::html_text(), error = function(e) NA_character_)

      each_row_df <- cbind(player_name, season, transfer_date, country_from, team_from, country_to, team_to, market_value, transfer_value) %>%
        data.frame()
    }

    each_player_df <- rbind(each_player_df, each_row_df)
  }

  each_player_df <- each_player_df %>%
    dplyr::mutate(market_value = mapply(.convert_value_to_numeric, market_value),
                  transfer_value = mapply(.convert_value_to_numeric, transfer_value)) %>%
    tidyr::separate(., transfer_date, into = c("Month", "Day", "Year"), sep = " ", remove = F) %>%
    dplyr::mutate(Day = gsub(",", "", .data$Day) %>% as.numeric(),
                  Year = as.numeric(.data$Year),
                  Month = match(.data$Month, month.abb),
                  transfer_date = lubridate::ymd(paste(.data$Year, .data$Month, .data$Day, sep = "-"))) %>%
    dplyr::select(-.data$Year, -.data$Month, -.data$Day)
  return(each_player_df)
}


#' Get player transfer history
#'
#' Returns data frame of player(s) transfer history from transfermarkt.com
#'
#' @param player_urls the player url(s) from transfermarket
#'
#' @return returns a dataframe of player transfers
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' player_transfer_history(player_urls)
#' }
player_transfer_history <- function(player_urls) {
  print("Extracting player transfer history data...")
  all_players <- player_urls %>%
    purrr::map_df(single_player_transfer_history)

  return(all_players)
}
