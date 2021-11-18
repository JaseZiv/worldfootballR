#' Get player market values
#'
#' Returns data frame of player valuations (in Euros) from transfermarkt.com
#'
#' @param country_name the country of the league's players
#' @param start_year the start year of the season (2020 for the 20/21 season)
#' @param league_url league url from transfermarkt.com. To be used when country_name not avalilable in main function
#'
#' @return returns a dataframe of player valuations for country/seasons
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \donttest{
#' get_player_market_values(country_name = "England", start_year = c(2019, 2020))
#' }

get_player_market_values <- function(country_name, start_year, league_url = NA) {

  # .pkg_message("Extracting player market values...")

  main_url <- "https://www.transfermarkt.com"

  if(is.na(league_url)) {
    meta_df <- read.csv(url("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"),
                        stringsAsFactors = F)

    tryCatch({meta_df_seasons <- meta_df %>%
      dplyr::filter(.data$country %in% country_name, .data$season_start_year %in% start_year)}, error = function(e) {meta_df_seasons <- data.frame()})

    if(nrow(meta_df_seasons) == 0) {
      stop(glue::glue("Country {country_name} or season {start_year} not found. Check that the country and season exists at https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"))
    }

  } else {

    tryCatch({league_page <- xml2::read_html(league_url)}, error = function(e) {league_page <- c()})

    if(length(league_page) == 0) {
      stop(glue::glue("League URL(s) {league_url} not found. Please check transfermarkt.com for the correct league URL"))

    }

    comp_name <- league_page %>% rvest::html_nodes(".spielername-profil") %>% rvest::html_text()

    country <- league_page %>% rvest::html_nodes(".miniflagge")
    country <- xml2::xml_attrs(country[[1]])[["title"]]

    seasons <- league_page %>% rvest::html_nodes(".chzn-select") %>% rvest::html_nodes("option")

    season_start_year <- c()
    for(each_season in seasons) {
      season_start_year <- c(season_start_year, xml2::xml_attrs(each_season)[["value"]])
    }

    season_urls <- paste0(league_url, "/plus/?saison_id=", season_start_year)

    meta_df_seasons <- data.frame(comp_name=as.character(comp_name), region=NA_character_, country=as.character(country), comp_url=as.character(league_url), season_start_year=as.numeric(season_start_year), season_urls=as.character(season_urls))

    tryCatch({meta_df_seasons <- meta_df_seasons %>%
      dplyr::filter(.data$season_start_year %in% start_year)}, error = function(e) {meta_df_seasons <- data.frame()})

    if(nrow(meta_df_seasons) == 0) {
      stop(glue::glue("League URL(s) {league_urls} or seasons {start_year} not found. Please check transfermarkt.com for the correct league URL"))

    }

  }

  all_seasons_urls <- meta_df_seasons$season_urls

  all_seasons_df <- data.frame()

  for(each_season in all_seasons_urls) {
    season_page <- xml2::read_html(each_season)

    team_urls <- season_page %>%
      rvest::html_nodes("#yw1 .hauptlink a") %>% rvest::html_attr("href") %>%
      # rvest::html_elements("tm-tooltip a") %>% rvest::html_attr("href") %>%
      unique() %>% paste0(main_url, .)
    # there now appears to be an errorneous URL so will remove that manually:
    team_urls <- team_urls[-grep(".com#", team_urls)]

    team_urls <- gsub("startseite", "kader", team_urls) %>%
      paste0(., "/plus/1")




    # Get Teams ---------------------------------------------------------------
    league_season_df <- data.frame()

    for(each_team in team_urls) {
      team_page <- xml2::read_html(each_team)

      team_data <- team_page %>% rvest::html_nodes("#yw1") %>% rvest::html_nodes(".items") %>% rvest::html_node("tbody")

      tab_head_names <- team_page %>% rvest::html_nodes("#yw1") %>% rvest::html_nodes(".items") %>% rvest::html_nodes("th") %>% rvest::html_text()

      # team name
      squad <- team_page %>% rvest::html_node(".dataName") %>% rvest::html_text() %>% stringr::str_squish()
      # numbers
      player_num <- team_data %>% rvest::html_nodes(".rn_nummer") %>% rvest::html_text()
      if(length(player_num) == 0) {
        player_num <- NA_character_
      }
      # player names
      player_name <- team_data %>% rvest::html_nodes(".hauptlink .di.nowrap") %>% rvest::html_elements(".hide-for-small a") %>% rvest::html_text()
      if(length(player_name) == 0) {
        player_name <- NA_character_
      }
      # player_url
      player_url <- team_data %>% rvest::html_nodes(".hauptlink .di.nowrap") %>% rvest::html_elements(".hide-for-small a") %>% rvest::html_attr("href") %>%
        paste0(main_url, .)
      if(length(player_url) == 0) {
        player_url <- NA_character_
      }
      # player position
      player_position <- team_data %>% rvest::html_nodes(".inline-table tr+ tr td") %>% rvest::html_text()
      if(length(player_position) == 0) {
        player_position <- NA_character_
      }
      # birthdate
      player_birthday <- team_data %>% rvest::html_nodes(".posrela+ .zentriert") %>% rvest::html_text()
      if(length(player_birthday) == 0) {
        player_birthday <- NA_character_
      }
      # player_nationality
      player_nationality <- c()
      player_nat <- team_data %>% rvest::html_nodes(".flaggenrahmen:nth-child(1)")
      if(length(player_nat) == 0) {
        player_nationality <- NA_character_
      } else {
        for(i in 1:length(player_nat)) {
          player_nationality <- c(player_nationality, xml2::xml_attrs(player_nat[[i]])[["title"]])
        }
      }
      # current club - only for previous seasons, not current:
      current_club_idx <- grep("Current club", tab_head_names)
      if(length(current_club_idx) == 0) {
        current_club <- NA_character_
      } else {
        c_club <- team_data %>% rvest::html_nodes(paste0("td:nth-child(", current_club_idx,")"))
        current_club <- c()
        for(cc in c_club) {
          each_current <- cc %>% rvest::html_nodes("a") %>% rvest::html_nodes("img") %>% rvest::html_attr("alt")
          if(length(each_current) == 0) {
            each_current <- NA_character_
          }
          current_club <- c(current_club, each_current)
        }
      }
      # player height
      height_idx <- grep("Height", tab_head_names)
      if(length(height_idx) == 0) {
        player_height_mtrs <- NA_character_
      } else {
        suppressWarnings(player_height_mtrs <- team_data %>% rvest::html_nodes(paste0("td:nth-child(", height_idx, ")")) %>% rvest::html_text() %>%
                           gsub(",", "\\.", .) %>% gsub("m", "", .) %>% stringr::str_squish() %>% as.numeric())
      }
      # player_foot
      foot_idx <- grep("Foot", tab_head_names)
      if(length(foot_idx) == 0) {
        player_foot <- NA_character_
      } else {
        player_foot <- team_data %>% rvest::html_nodes(paste0("td:nth-child(", foot_idx,")")) %>% rvest::html_text()
      }
      # date joined club
      joined_idx <- grep("Joined", tab_head_names)
      if(length(joined_idx) == 0) {
        date_joined <- NA_character_
      } else {
        date_joined <- team_data %>% rvest::html_nodes(paste0("td:nth-child(", joined_idx, ")")) %>% rvest::html_text()
      }
      # joined from
      from_idx <- grep("Signed from", tab_head_names)
      if(length(from_idx) == 0) {
        joined_from <- NA_character_
      } else {
        p_club <- tryCatch(team_data %>% rvest::html_nodes(paste0("td:nth-child(", from_idx, ")")), error = function(e) NA)
        joined_from <- c()
        for(pc in p_club) {
          each_past <- pc %>% rvest::html_nodes("a") %>% rvest::html_nodes("img") %>% rvest::html_attr("alt")
          if(length(each_past) == 0) {
            each_past <- NA_character_
          }
          joined_from <- c(joined_from, each_past)
        }
      }
      # contract expiry
      contract_idx <- grep("Contract", tab_head_names)
      if(length(contract_idx) == 0) {
        contract_expiry <- NA_character_
      } else {
        contract_expiry <- team_data %>% rvest::html_nodes(paste0("td:nth-child(", contract_idx, ")")) %>% rvest::html_text()
      }
      # value
      player_market_value <- team_data %>% rvest::html_nodes(".rechts.hauptlink") %>% rvest::html_text()
      if(length(player_market_value) == 0) {
        player_market_value <- NA_character_
      }

      suppressWarnings(team_df <- cbind(each_season, squad, player_num, player_name, player_url, player_position, player_birthday, player_nationality, current_club,
                                        player_height_mtrs, player_foot, date_joined, joined_from, contract_expiry, player_market_value) %>% data.frame())

      team_df <- team_df %>%
        dplyr::rename(season_urls=each_season)

      league_season_df <- rbind(league_season_df, team_df)

    }

    all_seasons_df <- rbind(all_seasons_df, league_season_df)

  }

  all_seasons_df <- meta_df_seasons %>%
    dplyr::left_join(all_seasons_df, by = "season_urls")


  all_seasons_df <- all_seasons_df %>%
    dplyr::mutate(player_market_value_euro = mapply(.convert_value_to_numeric, player_market_value)) %>%
    dplyr::mutate(date_joined = .tm_fix_dates(.data$date_joined),
                  contract_expiry = .tm_fix_dates(.data$contract_expiry)) %>%
    tidyr::separate(., player_birthday, into = c("Month", "Day", "Year", "player_age"), sep = " ", remove = F) %>%
    dplyr::mutate(Day = gsub(",", "", .data$Day) %>% as.numeric(),
                  Year = as.numeric(.data$Year),
                  Month = match(.data$Month, month.abb),
                  player_dob = lubridate::ymd(paste(.data$Year, .data$Month, .data$Day, sep = "-"))) %>%
    dplyr::mutate(player_age = as.numeric(gsub("\\D", "", .data$player_age))) %>%
    dplyr::select(.data$comp_name, .data$region, .data$country, .data$season_start_year, .data$squad, .data$player_num, .data$player_name, .data$player_position, .data$player_dob, .data$player_age, .data$player_nationality, .data$current_club,
                  .data$player_height_mtrs, .data$player_foot, .data$date_joined, .data$joined_from, .data$contract_expiry, .data$player_market_value_euro, .data$player_url)


  return(all_seasons_df)
}
