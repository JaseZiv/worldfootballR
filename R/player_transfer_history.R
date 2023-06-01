#' Get Transfermarkt player transfer history
#'
#' Returns data frame of player(s) transfer history from transfermarkt.com
#' Replaces the deprecated function player_transfer_history
#'
#' @param player_urls the player url(s) from transfermarkt
#' @param get_extra_info allows users to decide if they want to scrape extra info (contract length, countries involved) or not
#'
#' @return returns a dataframe of player transfers
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
tm_player_transfer_history <- function(player_urls, get_extra_info = TRUE) {
  # .pkg_message("Extracting player transfer history data. Please acknowledge transfermarkt.com as the data source.")

  single_player_transfer_history <- function(player_url) {
    pb$tick()

    main_url <- "https://www.transfermarkt.com"

    page <- .load_page_tm(player_url)

    player_name <- page %>% rvest::html_node("h1") %>% rvest::html_text() %>% gsub("#[[:digit:]]+ ", "", .) %>% stringr::str_squish()

    box_holder <- page %>% rvest::html_nodes(".viewport-tracking")

    # need to get the index of the box containing transfer history data
    box_idx <- box_holder %>% rvest::html_attr('data-viewport') %>% grep("Transferhistorie", .)
    all_transfers_player <- box_holder[box_idx]

    # now want the rows that contain data, but need to remove heading, column headers and footer
    # this step is new as transfermarkt USED TO contain this data in a table element
    all_transfer_rows <- all_transfers_player %>% rvest::html_children()
    rem_rows_idx <- c(grep("Transfer history", all_transfer_rows %>% rvest::html_text()),
                      grep("Season", all_transfer_rows %>% rvest::html_text()),
                      grep("Total", all_transfer_rows %>% rvest::html_text()))
    # only keep the necessary rows
    all_transfer_rows <- all_transfer_rows[-rem_rows_idx]

    # loop through each of the rows
    each_player_df <- data.frame()

    for(each_row in 1:length(all_transfer_rows)) {
      season <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__season") %>% rvest::html_text() %>% stringr::str_squish(), error = function(e) NA_character_)
      if(rlang::is_empty(season)) {
        each_row_df <- data.frame()
      } else {
        transfer_date <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__date") %>% rvest::html_text() %>% stringr::str_squish() %>%
                                    .tm_fix_dates() %>% lubridate::ymd(), error = function(e) NA_character_)

        team_from <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__old-club .tm-player-transfer-history-grid__club-link") %>% rvest::html_text() %>%
                                stringr::str_squish(), error = function(e) NA_character_)
        team_to <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__new-club .tm-player-transfer-history-grid__club-link") %>% rvest::html_text() %>%
                              stringr::str_squish(), error = function(e) NA_character_)

        market_value <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__market-value") %>% rvest::html_text() %>%
                                   stringr::str_squish() %>%
                                   .convert_value_to_numeric(), error = function(e) NA_character_)

        transfer_value_info <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__fee") %>% rvest::html_text() %>%
                                     stringr::str_squish() %>% tolower, error = function(e) NA_character_)

        transfer_value <- transfer_value_info %>% .convert_value_to_numeric

        if(grepl("free", transfer_value_info)) {
          transfer_type <- 'Free transfer'
        } else if(grepl("end of", transfer_value_info)) {
          transfer_type <- 'End of loan'
        } else if(grepl("fee", transfer_value_info)) {
          transfer_type <- 'Paid loan'
        } else if(grepl("loan", transfer_value_info)) {
          transfer_type <- 'Loan'
        } else if(grepl("m", transfer_value_info)) {
          transfer_type <- 'Transfer'
        } else if(grepl("th.", transfer_value_info)) {
          transfer_type <- 'Transfer'
        } else if(grepl("k", transfer_value_info)) {
          transfer_type <- 'Transfer'
        } else {
          transfer_type <- NA_character_
        }

        # Executed if the user wants to get more info. Contains: Contract Expiring date + Days remaining. From which countries was the transfer.
        if(get_extra_info == TRUE){

          extra_info_url <- all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__link") %>% rvest::html_attr("href") %>% paste0(main_url, .)
          extra_info <- tryCatch(.load_page_tm(extra_info_url), error = function(e) NA)
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

          country_to <- tryCatch(extra_info %>% rvest::html_nodes(".large-4.columns table .rechts .flaggenrahmen") %>% rvest::html_attr("title"), error = function(e) NA)

          if(length(country_to) < 1){
            country_to <- NA
          }

          countries <- tryCatch(extra_info %>% rvest::html_nodes(".large-4.columns table .flaggenrahmen") %>% rvest::html_attr("title"), error = function(e) NA)
          if(length(countries) < 2){
            if(is.na(country_to)){
              country_from <- countries[1]
            } else {
              country_from <- NA
            }
          } else {
            country_from <- countries[1]
          }
        }

        if(get_extra_info == TRUE){
          each_row_df <- data.frame(player_name=as.character(player_name), season=as.character(season), transfer_date=lubridate::ymd(transfer_date),
                                  country_from=as.character(country_from), team_from=as.character(team_from), country_to=as.character(country_to),
                                  team_to=as.character(team_to), market_value=as.numeric(market_value), transfer_value=as.numeric(transfer_value),
                                  transfer_type = as.character(transfer_type), contract_expiry=lubridate::ymd(contract_expiry),
                                  days_remaining=as.numeric(days_remaining))
        } else {
          each_row_df <- data.frame(player_name=as.character(player_name), season=as.character(season), transfer_date=lubridate::ymd(transfer_date),
                                    team_from=as.character(team_from), team_to=as.character(team_to), market_value=as.numeric(market_value),
                                    transfer_value=as.numeric(transfer_value), transfer_type = as.character(transfer_type))
        }

      }

      each_player_df <- rbind(each_player_df, each_row_df)
    }

    return(each_player_df)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(player_urls))

  all_players <- player_urls %>% purrr::map_df(single_player_transfer_history)

  return(all_players)
}







#' Get player transfer history
#'
#' Returns data frame of player(s) transfer history from transfermarkt.com
#'
#' @param player_urls the player url(s) from transfermarkt
#'
#' @return returns a dataframe of player transfers
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
player_transfer_history <- function(player_urls) {

  .Deprecated("tm_player_transfer_history")
  # .pkg_message("Extracting player transfer history data. Please acknowledge transfermarkt.com as the data source.")

  single_player_transfer_history <- function(player_url) {
    pb$tick()

    main_url <- "https://www.transfermarkt.com"

    page <- .load_page_tm(player_url)

    player_name <- page %>% rvest::html_node("h1") %>% rvest::html_text() %>% gsub("#[[:digit:]]+ ", "", .) %>% stringr::str_squish()

    box_holder <- page %>% rvest::html_nodes(".viewport-tracking")

    # need to get the index of the box containing transfer history data
    box_idx <- box_holder %>% rvest::html_attr('data-viewport') %>% grep("Transferhistorie", .)
    all_transfers_player <- box_holder[box_idx]

    # now want the rows that contain data, but need to remove heading, column headers and footer
    # this step is new as transfermarkt USED TO contain this data in a table element
    all_transfer_rows <- all_transfers_player %>% rvest::html_children()
    rem_rows_idx <- c(grep("Transfer history", all_transfer_rows %>% rvest::html_text()),
                      grep("Season", all_transfer_rows %>% rvest::html_text()),
                      grep("Total", all_transfer_rows %>% rvest::html_text()))
    # only keep the necessary rows
    all_transfer_rows <- all_transfer_rows[-rem_rows_idx]

    # loop through each of the rows
    each_player_df <- data.frame()

    for(each_row in 1:length(all_transfer_rows)) {
      season <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__season") %>% rvest::html_text() %>% stringr::str_squish(), error = function(e) NA_character_)
      if(rlang::is_empty(season)) {
        each_row_df <- data.frame()
      } else {
        transfer_date <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__date") %>% rvest::html_text() %>% stringr::str_squish() %>%
                                    .tm_fix_dates() %>% lubridate::ymd(), error = function(e) NA_character_)

        # country_flags <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".flagge"), error = function(e) NA)
        country_from <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__old-club .tm-player-transfer-history-grid__flag") %>% rvest::html_attr("alt")  %>% stringr::str_squish() %>% .replace_empty_na(), error = function(e) NA_character_)
        country_to <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__new-club .tm-player-transfer-history-grid__flag") %>% rvest::html_attr("alt"), error = function(e) NA_character_)
        # to handle for players that are retired, like: "https://www.transfermarkt.com/massimiliano-allegri/profil/spieler/163501":
        if(rlang::is_empty(country_to)) {
          country_to <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__new-club .tm-player-transfer-history-grid__club-link") %>% rvest::html_text() %>% stringr::str_squish(), error = function(e) NA_character_)
        }

        team_from <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__old-club .tm-player-transfer-history-grid__club-link") %>% rvest::html_text() %>%
                                stringr::str_squish(), error = function(e) NA_character_)
        team_to <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__new-club .tm-player-transfer-history-grid__club-link") %>% rvest::html_text() %>%
                              stringr::str_squish(), error = function(e) NA_character_)

        market_value <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__market-value") %>% rvest::html_text() %>%
                                   stringr::str_squish() %>%
                                   .convert_value_to_numeric(), error = function(e) NA_character_)
        transfer_value <- tryCatch(all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__fee") %>% rvest::html_text() %>%
                                     stringr::str_squish() %>%
                                     .convert_value_to_numeric, error = function(e) NA_character_)

        # to get contract length, which isn't on the main page listing all transfers:
        extra_info_url <- all_transfer_rows[each_row] %>% rvest::html_nodes(".tm-player-transfer-history-grid__link") %>% rvest::html_attr("href") %>% paste0(main_url, .)
        extra_info <- tryCatch(.load_page_tm(extra_info_url), error = function(e) NA)
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

