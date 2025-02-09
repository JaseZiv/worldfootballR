#' Get league injuries
#'
#' Returns a data frame of all currently injured players for a selected league
#'
#' @param country_name the country of the league's players
#' @param league_url league url from transfermarkt.com. To be used when country_name not available in main function
#'
#' @return returns a dataframe of injured players in the selected league
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
tm_league_injuries <- function(country_name, league_url = NA) {
  main_url <- "https://www.transfermarkt.com"

  if(is.na(league_url)) {
    meta_df <- read.csv(url("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"),
                        stringsAsFactors = F)

    tryCatch({meta_df_seasons <- meta_df %>%
      dplyr::filter(.data[["country"]] %in% country_name)}, error = function(e) {meta_df_seasons <- data.frame()})

    if(nrow(meta_df_seasons) == 0) {
      stop(glue::glue("Country {country_name} not found. Check that the country and season exists at https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"))
    }

    comp_url <- meta_df_seasons$comp_url %>% unique() %>% .[1]

  } else {
    tryCatch({league_page <- xml2::read_html(league_url)}, error = function(e) {league_page <- c()})

    tryCatch({country <- league_page %>%
      rvest::html_nodes(".data-header") %>%
      rvest::html_node("img") %>%
      rvest::html_attr("alt") %>% .[!is.na(.)] %>% stringr::str_squish()}, error = function(e) {country_name <- NA_character_})

    tryCatch({comp_name <- league_page %>% rvest::html_nodes(".data-header__headline-wrapper--oswald") %>% rvest::html_text() %>% stringr::str_squish()},
             error = function(e) {country_name <- comp_name})

    comp_url <- league_url

    if(length(league_page) == 0) {
      stop(glue::glue("League URL(s) {league_url} not found. Please check transfermarkt.com for the correct league URL"))
    }

  }


  comp_url <- gsub("startseite", "verletztespieler", comp_url)
  comp_url <- paste0(comp_url, "/plus/1")


  injuries_page <- xml2::read_html(comp_url)
  injuries_page <- injuries_page %>% rvest::html_nodes("#yw1") %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children()

  player_name <- tryCatch(injuries_page %>% rvest::html_nodes(".inline-table .hauptlink a") %>% rvest::html_text(),
                          error = function(e) player_name <- NA_character_)
  player_url <- tryCatch(injuries_page %>% rvest::html_nodes(".inline-table .hauptlink a") %>% rvest::html_attr("href") %>% paste0(main_url, .),
                         error = function(e) player_url <- NA_character_)
  position <- tryCatch(injuries_page %>% rvest::html_nodes(".inline-table tr+ tr td") %>% rvest::html_text(),
                       error = function(e) position <- NA_character_)
  current_club <- tryCatch(injuries_page %>% rvest::html_nodes(".tiny_wappen") %>% rvest::html_attr("alt"),
                           error = function(e) current_club <- NA_character_)
  age <- tryCatch(injuries_page %>% rvest::html_nodes(".no-border-rechts+ .zentriert") %>% rvest::html_text() %>% as.numeric(),
                       error = function(e) age <- NA_integer_)

  nationality <- tryCatch(injuries_page %>% rvest::html_nodes(".flaggenrahmen:nth-child(1)") %>% rvest::html_attr("title"),
                          error = function(e) nationality <- NA_character_)
  second_nationality <- c()
  for(i in injuries_page) {
    nat <- tryCatch(i %>% rvest::html_nodes("br+ .flaggenrahmen") %>% rvest::html_attr("title") %>% .replace_empty_na(),
                    error = function(e) nat <- NA_character_)
    second_nationality <- tryCatch(c(second_nationality, nat),
                                   error = function(e) second_nationality <- NA_character_)
  }
  injury <- tryCatch(injuries_page %>% rvest::html_nodes("td.links") %>% rvest::html_text(),
                              error = function(e) injury <- NA_character_)
  injured_since <- tryCatch(injuries_page %>% rvest::html_nodes(".links+ td") %>% rvest::html_text() %>% .tm_fix_dates(),
                             error = function(e) injured_since <- NA_character_)
  injured_until <- tryCatch(injuries_page %>% rvest::html_nodes(".links~ .zentriert+ td.zentriert") %>% rvest::html_text() %>% .tm_fix_dates(),
                            error = function(e) injured_until <- NA_character_)
  player_market_value <- tryCatch(injuries_page %>% rvest::html_nodes("td.rechts") %>% rvest::html_text(),
                                  error = function(e) player_market_value <- NA_character_)


  out_df <- cbind(player_name, player_url, position, current_club, age, nationality, second_nationality,
                  injury, injured_since, injured_until, player_market_value) %>% data.frame()

  if(is.na(league_url)) {
    out_df <- meta_df_seasons %>% dplyr::select(.data[["comp_name"]], .data[["country"]], .data[["comp_url"]]) %>% dplyr::distinct() %>%
      cbind(out_df)
  } else {
    out_df <- cbind(comp_name, country, comp_url, out_df)
  }

  out_df <- out_df %>%
    dplyr::mutate(player_name = as.character(.data[["player_name"]]),
                  player_url = as.character(.data[["player_url"]]),
                  position = as.character(.data[["position"]]),
                  current_club = as.character(.data[["current_club"]]),
                  age = as.numeric(.data[["age"]]),
                  nationality = as.character(.data[["nationality"]]),
                  second_nationality = as.character(.data[["second_nationality"]]),
                  injury = as.character(.data[["injury"]]),
                  injured_since = lubridate::ymd(.data[["injured_since"]]),
                  injured_until = lubridate::ymd(.data[["injured_until"]]),
                  player_market_value = mapply(.convert_value_to_numeric, .data[["player_market_value"]]))

}
