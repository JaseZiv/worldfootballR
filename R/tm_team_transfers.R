#' Get team transfers
#'
#' Returns all transfer arrivals and departures for a given team season
#'
#' @param team_url transfermarkt.com team url for a season
#' @param transfer_window which window the transfer occurred - options include "all" for both, "summer" or "winter"
#'
#' @return returns a dataframe of all team transfers
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export

tm_team_transfers <- function(team_url, transfer_window = "all") {
  main_url <- "https://www.transfermarkt.com"

  if(!tolower(transfer_window) %in% c("all", "summer", "winter")) stop("check transfer window is either 'all', 'summer' or 'winter'")

  # .pkg_message("Scraping team transfer arrivals and departures. Please acknowledge transfermarkt.com as the data source")

  each_team_xfer <- function(each_team_url) {
    pb$tick()
    xfers_url <- gsub("startseite", "transfers", each_team_url)

    team_page <- xml2::read_html(xfers_url)

    team_name <- team_page %>% rvest::html_nodes("h1") %>% rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na()
    league <- team_page %>% rvest::html_nodes(".hauptpunkt a") %>% rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na()
    country <- team_page %>% rvest::html_nodes(".mediumpunkt img") %>% rvest::html_attr("title") %>% .replace_empty_na()
    season <- xfers_url %>% gsub(".*saison_id/", "", .) %>% .replace_empty_na()


    tab_box <- team_page %>% rvest::html_nodes(".box")
    tab_names <- tab_box %>% rvest::html_nodes("h2") %>% rvest::html_text() %>% stringr::str_squish()
    # need to get the URL to be able to pass transfer window
    xfers_window_box <- tab_box[grep("Transfers ", tab_names)]

    # there are summer ("s") and winter ("w") transfer periods - which get used will be set by the user?
    if(tolower(transfer_window) == "all") {
      summer_winter <- c("s", "w")
    } else if (tolower(transfer_window) == "summer") {
      summer_winter <- "s"
    } else {
      summer_winter <- "w"
    }

    team_df <- data.frame()

    for(each_window in summer_winter){
      xfers_window_url <- xfers_window_box %>% rvest::html_nodes(".content") %>% rvest::html_children() %>% rvest::html_attr("action")
      xfers_window_url <- xfers_window_url %>% paste0(main_url, ., "?saison_id=", season, "&pos=&detailpos=&w_s=", each_window)
      team_page_window <- xml2::read_html(xfers_window_url)
      tab_box_window <- team_page_window %>% rvest::html_nodes(".box")
      # need to isolate the arrivals and departures tables
      tab_names <- tab_box_window %>% rvest::html_nodes("h2") %>% rvest::html_text() %>% stringr::str_squish()
      tab_box_window <- tab_box_window[which(tab_names %in% c("Arrivals", "Departures"))]
      both_tabs <- tab_box_window %>% rvest::html_nodes(".responsive-table")


      # create output for team of both arrivals and departures
      team_df_each_window <- data.frame()

      for(i in 1:length(tab_box_window)) {
        each_tab <- tryCatch(both_tabs[i] %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children(), error = function(e) NA_character_)

        if(any(is.na(each_tab))) {
          player_df <- data.frame()
        } else {
          player_df <- data.frame()
          for(j in 1:length(each_tab)) {
            player_df[j, "transfer_type"] <- tryCatch(tab_box_window[i] %>% rvest::html_nodes("h2") %>% rvest::html_text() %>% stringr::str_squish(),
                                                      error = function(e) player_df[j, "transfer_type"] <- NA_character_)
            player_df[j, "player_name"] <- tryCatch(each_tab[j] %>% rvest::html_node(".hauptlink") %>% rvest::html_nodes("a") %>% rvest::html_text(),
                                                    error = function(e) player_df[j, "player_name"] <- NA_character_)
            player_df[j, "player_url"] <- tryCatch(each_tab[j] %>% rvest::html_node(".hauptlink") %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% paste0(main_url, .),
                                                   error = function(e) player_df[j, "player_url"] <- NA_character_)
            player_df[j, "player_position"] <- tryCatch(each_tab[j] %>% rvest::html_nodes("td:nth-child(2) tr+ tr td") %>% rvest::html_text() %>% .replace_empty_na(),
                                                        error = function(e) player_df[j, "player_position"] <- NA_character_)
            player_df[j, "player_age"] <- tryCatch(each_tab[j] %>% rvest::html_nodes("td.zentriert:nth-child(3)") %>% rvest::html_text() %>% .replace_empty_na(),
                                                   error = function(e) player_df[j, "player_age"] <- NA_character_)
            player_df[j, "player_nationality"] <- tryCatch(each_tab[j] %>% rvest::html_nodes(".zentriert .flaggenrahmen") %>% .[1] %>% rvest::html_attr("title") %>% .replace_empty_na(),
                                                           error = function(e) player_df[j, "player_nationality"] <- NA_character_)
            player_df[j, "club_2"] <- tryCatch(each_tab[j] %>% rvest::html_nodes(".inline-table") %>% rvest::html_nodes("a") %>% .[3] %>% rvest::html_text() %>% .replace_empty_na(),
                                               error = function(e) player_df[j, "club_2"] <- NA_character_)
            player_df[j, "league_2"] <- tryCatch(each_tab[j] %>% rvest::html_nodes(".flaggenrahmen+ a") %>% rvest::html_text() %>% .replace_empty_na(),
                                                 error = function(e) player_df[j, "league_2"] <- NA_character_)
            player_df[j, "country_2"] <- tryCatch(each_tab[j] %>% rvest::html_nodes(".inline-table .flaggenrahmen") %>% rvest::html_attr("alt") %>% .replace_empty_na(),
                                                  error = function(e) player_df[j, "country_2"] <- NA_character_)
            player_df[j, "transfer_fee"] <- tryCatch(each_tab[j] %>% rvest::html_nodes(".rechts a") %>% rvest::html_text() %>% .replace_empty_na(),
                                                     error = function(e) player_df[j, "transfer_fee"] <- NA_character_)
            player_df[j, "is_loan"] <- tryCatch(grepl("loan", player_df[j, "transfer_fee"], ignore.case = T),
                                                error = function(e) player_df[j, "is_loan"] <- NA_character_)
            player_df[j, "transfer_fee_dup"] <- tryCatch(player_df[j, "transfer_fee"],
                                                         error = function(e) player_df[j, "transfer_fee_dup"] <- NA_character_)
            if(is.na(each_tab[j])) {
              player_df[j, "transfer_fee_notes1"] <- NA_character_
            } else {
              if(length(each_tab[j] %>% rvest::html_nodes(".rechts.hauptlink a i") %>% rvest::html_text()) == 0) {
                player_df[j, "transfer_fee_notes1"] <- NA_character_
              } else {
                player_df[j, "transfer_fee_notes1"] <- tryCatch(each_tab[j] %>% rvest::html_nodes(".rechts.hauptlink a i") %>% rvest::html_text(),
                                                                error = function(e) player_df[j, "transfer_fee_notes1"] <- NA_character_)
              }
            }

            player_df[j, "window"] <- tryCatch(each_window,
                                               error = function(e) player_df[j, "window"] <- NA_character_)

          }

        }

        team_df_each_window <- dplyr::bind_rows(team_df_each_window, player_df)
      }

      team_df <- dplyr::bind_rows(team_df, team_df_each_window)
    }

    team_df <- team_df %>%
      dplyr::mutate(window = dplyr::case_when(
        window == "s" ~ "Summer",
        window == "w" ~ "Winter",
        TRUE ~ "Unknown"
      ))

    # add metadata
    team_df <- cbind(team_name, league, country, season, team_df)

    # cleaning up final output data
    team_df <- team_df %>%
      dplyr::mutate(transfer_fee = ifelse(stringr::str_detect(.data$transfer_fee_dup, "Loan fee:"), .data$transfer_fee_notes1, .data$transfer_fee)) %>%
      dplyr::mutate(transfer_fee = mapply(.convert_value_to_numeric, euro_value = .data$transfer_fee)) %>%
      dplyr::mutate(transfer_fee_dup = ifelse(is.na(.data$transfer_fee), .data$transfer_fee_dup, NA_character_),
                    transfer_fee_dup = gsub("End of loan", "End of loan ", .data$transfer_fee_dup)) %>%
      dplyr::rename(transfer_notes = .data$transfer_fee_dup) %>%
      dplyr::select(-.data$transfer_fee_notes1)


    #----- Get player stats including goals and appearances: -----#
    team_data_url <- gsub("startseite", "leistungsdaten", each_team_url)
    team_data_page <- xml2::read_html(team_data_url)

    team_data_table <- team_data_page %>% rvest::html_nodes("#yw1") %>% rvest::html_node("table") %>% rvest::html_nodes("tbody") %>% rvest::html_children()

    player_name <- team_data_table %>% rvest::html_nodes(".hauptlink") %>% rvest::html_nodes(".hide-for-small") %>% rvest::html_text()
    # player_age <- team_data_table %>% rvest::html_nodes(".posrela+ .zentriert") %>% rvest::html_text()
    in_squad <- team_data_table %>% rvest::html_nodes("td:nth-child(5)") %>% rvest::html_text() %>%
      gsub("-", "0", .) %>% as.numeric()
    appearances <- team_data_table %>% rvest::html_nodes("td:nth-child(6)") %>% rvest::html_text() %>%
      gsub("Not.*", "0", .) %>% as.numeric()
    goals <- team_data_table %>% rvest::html_nodes(".zentriert:nth-child(7)") %>% rvest::html_text() %>%
      gsub("-", "0", .) %>% as.numeric()
    minutes_played <- team_data_table %>% rvest::html_nodes(".rechts") %>% rvest::html_text() %>%
      gsub("\\.", "", .) %>% gsub("'", "", .) %>% gsub("-", "0", .) %>% as.numeric()

    team_data_df <- data.frame(player_name = as.character(player_name), in_squad = as.numeric(in_squad),
                               appearances = as.numeric(appearances), goals = as.numeric(goals), minutes_played = as.numeric(minutes_played))

    # now join the two data sets together:
    team_df <- team_df %>%
      dplyr::left_join(team_data_df, by = c("player_name"))

    return(team_df)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(team_url))

  final_output <- team_url %>%
    purrr::map_df(each_team_xfer)

  final_output <- final_output %>%
    dplyr::filter(!is.na(.data$player_name))

  return(final_output)
}
