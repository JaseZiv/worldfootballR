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
  print("Extracting player transfer history data. Please acknowledge transfermarkt.com as the data source.")

  single_player_transfer_history <- function(player_url) {
    pb$tick()

    main_url <- "https://www.transfermarkt.com"

    page <- xml2::read_html(player_url)

    player_name <- page %>% rvest::html_node("h1") %>% rvest::html_text() %>% stringr::str_squish()

    all_transfers_player <- page %>%
      rvest::html_node(".transferhistorie") %>%
      rvest::html_nodes(".responsive-table") %>%
      rvest::html_nodes("tbody")


    all_transfer_rows <- all_transfers_player %>% rvest::html_nodes("tr")

    each_player_df <- data.frame()

    for(each_row in 1:length(all_transfer_rows)) {
      season <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".zentriert") %>% .[1] %>% rvest::html_text(), error = function(e) NA_character_)
      if(rlang::is_empty(season)) {
        each_row_df <- data.frame()
      } else {
        transfer_date <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".zentriert") %>% .[2] %>% rvest::html_text() %>%
                                    .tm_fix_dates() %>% lubridate::ymd(), error = function(e) NA_character_)

        country_flags <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".flagge"), error = function(e) NA)
        country_from <- tryCatch(xml2::xml_attrs(xml2::xml_child(country_flags[[1]], 1))[["title"]], error = function(e) NA_character_)
        country_to <- tryCatch(xml2::xml_attrs(xml2::xml_child(country_flags[[2]], 1))[["title"]], error = function(e) NA_character_)

        team_from <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".vereinsname") %>% .[1] %>% rvest::html_text(), error = function(e) NA_character_)
        team_to <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".vereinsname") %>% .[2] %>% rvest::html_text(), error = function(e) NA_character_)

        market_value <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".zelle-mw") %>% rvest::html_text() %>%
                                   .convert_value_to_numeric(), error = function(e) NA_character_)
        transfer_value <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".zelle-abloese") %>% rvest::html_text() %>%
                                     .convert_value_to_numeric, error = function(e) NA_character_)

        # to get contract length, which isn't on the main page listing all transfers:
        extra_info_url <- all_transfer_rows[each_row] %>% rvest::html_nodes(".zentriert") %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% paste0(main_url, .)
        extra_info <- tryCatch(xml2::read_html(extra_info_url), error = function(e) NA)
        contract_box <- extra_info %>% rvest::html_nodes(".large-4.columns") %>% rvest::html_node("table") %>% rvest::html_children()
        contract_idx <- grep("Remaining contract duration", contract_box %>% rvest::html_text())
        if(is.na(extra_info)) {
          contract_expiry <- NA
          days_remaining <- NA
        } else {
          text_to_remove <- contract_box[contract_idx] %>% rvest::html_nodes("b") %>% rvest::html_text()
          if(length(text_to_remove) == 0) {
            contract_expiry <- NA
            days_remaining <- NA
          } else {
            contract_expiry <- contract_box[contract_idx] %>% rvest::html_text() %>%
              gsub(text_to_remove, "", .) %>% stringr::str_squish() %>% gsub(".*\\((.*)\\).*", "\\1", .) %>% .tm_fix_dates() %>% lubridate::ymd()
            days_remaining <- difftime(contract_expiry, transfer_date, units = c("days")) %>% as.numeric()
          }

        }


        each_row_df <- data.frame(player_name=as.character(player_name), season=as.character(season), transfer_date=lubridate::ymd(transfer_date),
                                  country_from=as.character(country_from), team_from=as.character(team_from), country_to=as.character(country_to),
                                  team_to=as.character(team_to), market_value=as.numeric(market_value), transfer_value=as.numeric(transfer_value),
                                  contract_expiry=lubridate::ymd(contract_expiry), days_remaining=as.numeric(days_remaining))

      }

      each_player_df <- rbind(each_player_df, each_row_df)
    }

    return(each_player_df)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(player_urls))

  all_players <- player_urls %>%
    purrr::map_df(single_player_transfer_history)

  return(all_players)
}

