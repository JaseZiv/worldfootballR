#' Get expiring contracts
#'
#' Returns a data frame of players with expiring contracts for a selected league and time period
#'
#' @param country_name the country of the league's players
#' @param contract_end_year the year the contract is due to expire
#' @param league_url league url from transfermarkt.com. To be used when country_name not available in main function
#'
#' @return returns a dataframe of expiring contracts in the selected league
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'

tm_expiring_contracts <- function(country_name, contract_end_year, league_url = NA) {
  main_url <- "https://www.transfermarkt.com"

  if(is.na(league_url)) {
    meta_df <- read.csv(url("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"),
                        stringsAsFactors = F)

    tryCatch({meta_df_seasons <- meta_df %>%
      dplyr::filter(.data$country %in% country_name)}, error = function(e) {meta_df_seasons <- data.frame()})

    if(nrow(meta_df_seasons) == 0) {
      stop(glue::glue("Country {country_name} not found. Check that the country and season exists at https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"))
    }

    comp_url <- meta_df_seasons$comp_url %>% unique() %>% .[1]

  } else {
    tryCatch({league_page <- xml2::read_html(league_url)}, error = function(e) {league_page <- c()})

    tryCatch({country <- league_page %>%
      rvest::html_nodes(".profilheader") %>%
      rvest::html_node("img") %>%
      rvest::html_attr("alt") %>% .[!is.na(.)]}, error = function(e) {country_name <- NA_character_})

    tryCatch({comp_name <- league_page %>% rvest::html_nodes(".headerfoto img") %>% rvest::html_attr("title")},
             error = function(e) {comp_name <- NA_character_})

    comp_url <- league_url

    if(length(league_page) == 0) {
      stop(glue::glue("League URL(s) {league_url} not found. Please check transfermarkt.com for the correct league URL"))
    }

  }


  comp_url <- gsub("startseite", "endendevertraege", comp_url)
  comp_url <- paste0(comp_url, "/plus/1/galerie/0?jahr=", contract_end_year, "&ausrichtung=alle&spielerposition_id=alle&altersklasse=alle&land_id=0&yt0=Show")


  expiring_page <- xml2::read_html(comp_url)

  expiring_urls <- expiring_page %>% rvest::html_nodes(".tm-pagination__list-item a") %>% rvest::html_attr("href")

  if(length(expiring_urls)==0) {
    expiring_pages <- comp_url
  } else {
    expiring_pages <- expiring_urls  %>%
      paste0(main_url, .) %>% unique()
  }


  get_each_page <- function(page_url) {

    if(length(expiring_pages) == 0) {
      exp_pg <- tryCatch(expiring_page %>% rvest::html_nodes("#yw1") %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children(),
                     error = function(e) exp_pg <- NA_character_)
    } else {
      exp_pg <- xml2::read_html(page_url) %>% rvest::html_nodes("#yw1") %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children()
    }


    # exp_pg <- xml2::read_html(page_url) %>% rvest::html_nodes("#yw1") %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children()

    player_name <- tryCatch(exp_pg %>% rvest::html_nodes(".inline-table .hauptlink a") %>% rvest::html_text(),
                            error = function(e) player_name <- NA_character_)
    player_url <- tryCatch(exp_pg %>% rvest::html_nodes(".inline-table .hauptlink a") %>% rvest::html_attr("href") %>% paste0(main_url, .),
                           error = function(e) player_url <- NA_character_)
    position <- tryCatch(exp_pg %>% rvest::html_nodes(".inline-table tr+ tr td") %>% rvest::html_text(),
                         error = function(e) position <- NA_character_)
    nationality <- tryCatch(exp_pg %>% rvest::html_nodes(".flaggenrahmen:nth-child(1)") %>% html_attr("title"),
                            error = function(e) nationality <- NA_character_)
    second_nationality <- c()
    for(i in exp_pg) {
      nat <- tryCatch(i %>% rvest::html_nodes("br+ .flaggenrahmen") %>% rvest::html_attr("title") %>% .replace_empty_na(),
                      error = function(e) nat <- NA_character_)
      second_nationality <- tryCatch(c(second_nationality, nat),
                                     error = function(e) second_nationality <- NA_character_)
    }
    current_club <- tryCatch(exp_pg %>% rvest::html_nodes(".tiny_wappen") %>% rvest::html_attr("alt"),
                          error = function(e) current_club <- NA_character_)
    contract_expiry <- tryCatch(exp_pg %>% rvest::html_nodes(".no-border-rechts+ .zentriert") %>% rvest::html_text() %>% .tm_fix_dates(),
                            error = function(e) contract_expiry <- NA_character_)
    contract_option <- tryCatch(exp_pg %>% rvest::html_nodes(".no-border-rechts~ .zentriert+ .zentriert") %>% rvest::html_text(),
                      error = function(e) contract_option <- NA_character_)
    player_market_value <- tryCatch(exp_pg %>% rvest::html_nodes(".zentriert+ .hauptlink") %>% rvest::html_text(),
                                         error = function(e) player_market_value <- NA_character_)
    transfer_fee <- tryCatch(exp_pg %>% rvest::html_nodes(".hauptlink+ .rechts") %>% rvest::html_text(),
                               error = function(e) transfer_fee <- NA_character_)
    agent <- tryCatch(exp_pg %>% rvest::html_nodes(".rechts+ .hauptlink") %>% rvest::html_text() %>% stringr::str_squish(),
                      error = function(e) agent <- NA_character_)

    out_df <- cbind(player_name, player_url, position, nationality, second_nationality,
                    current_club, contract_expiry, contract_option, player_market_value, transfer_fee, agent) %>% data.frame()

    if(is.na(league_url)) {
      out_df <- meta_df_seasons %>% dplyr::select(.data$comp_name, .data$country, .data$comp_url) %>% dplyr::distinct() %>%
        cbind(out_df)
    } else {
      out_df <- cbind(comp_name, country, comp_url, out_df)
    }

    out_df <- out_df %>%
      dplyr::mutate(player_name = as.character(.data$player_name),
                    player_url = as.character(.data$player_url),
                    position = as.character(.data$position),
                    nationality = as.character(.data$nationality),
                    second_nationality = as.character(.data$second_nationality),
                    current_club = as.character(.data$current_club),
                    contract_expiry = lubridate::ymd(.data$contract_expiry),
                    contract_option = as.character(.data$contract_option),
                    player_market_value = mapply(.convert_value_to_numeric, .data$player_market_value),
                    transfer_fee = mapply(.convert_value_to_numeric, .data$transfer_fee),
                    agent = as.character(.data$agent))




    return(out_df)
  }

  f_possibly <- purrr::possibly(get_each_page, otherwise = data.frame(), quiet = FALSE)
  purrr::map_dfr(
    expiring_pages,
    f_possibly
  )

}
