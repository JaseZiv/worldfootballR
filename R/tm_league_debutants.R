#' Get league debutants
#'
#' Returns a data frame of debutants for a selected league
#'
#' @param country_name the country of the league's players
#' @param league_url league url from transfermarkt.com. To be used when country_name not available in main function
#' @param debut_type whether you want 'league' debut or 'pro' debut
#' @param debut_start_year the season start year of the beginning of the period you want results for
#' @param debut_end_year the season start year of the end of the period you want results for
#'
#' @return returns a dataframe of players who debuted in the selected league
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
tm_league_debutants <- function(country_name, league_url = NA, debut_type, debut_start_year, debut_end_year) {
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
    tryCatch({league_page <- .load_page_tm(league_url)}, error = function(e) {league_page <- c()})

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

  comp_url <- gsub("startseite", "profidebuetanten", comp_url)
  comp_url <- paste0(comp_url, "/plus/1/galerie/0?saisonIdVon=", debut_start_year, "&saison_id=", debut_end_year, "&option=")

  if(debut_type == "league") {
    comp_url <- paste0(comp_url, "liga")
  } else if (debut_type == "pro") {
    comp_url <- paste0(comp_url, "profi")
  } else {
    stop("Check debut type is either 'league' or 'pro'")
  }

  debutants_page <- .load_page_tm(comp_url)

  debutants_pages <- debutants_page %>% rvest::html_nodes(".tm-pagination__list-item a") %>% rvest::html_attr("href") %>%
    paste0(main_url, .)


  get_each_page <- function(page_url) {
    debs <- .load_page_tm(page_url) %>% rvest::html_nodes("#yw1") %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children()

    player_name <- tryCatch(debs %>% rvest::html_nodes(".inline-table .hauptlink a") %>% rvest::html_text(),
                            error = function(e) player_name <- NA_character_)
    player_url <- tryCatch(debs %>% rvest::html_nodes(".inline-table .hauptlink a") %>% rvest::html_attr("href") %>% paste0(main_url, .),
    error = function(e) player_url <- NA_character_)
    position <- tryCatch(debs %>% rvest::html_nodes(".inline-table tr+ tr td") %>% rvest::html_text(),
    error = function(e) position <- NA_character_)
    nationality <- tryCatch(debs %>% rvest::html_nodes(".flaggenrahmen:nth-child(1)") %>% html_attr("title"),
    error = function(e) nationality <- NA_character_)
    second_nationality <- c()
    for(i in debs) {
      nat <- tryCatch(i %>% rvest::html_nodes("br+ .flaggenrahmen") %>% rvest::html_attr("title") %>% .replace_empty_na(),
      error = function(e) nat <- NA_character_)
      second_nationality <- tryCatch(c(second_nationality, nat),
      error = function(e) second_nationality <- NA_character_)
    }
    debut_for <- tryCatch(debs %>% rvest::html_nodes(".zentriert+ .zentriert img") %>% rvest::html_attr("alt"),
    error = function(e) debut_for <- NA_character_)
    appearances <- tryCatch(debs %>% rvest::html_nodes(".zentriert.hauptlink a") %>% rvest::html_text() %>%
      gsub("Not.*", "0", .) %>% as.numeric(),
    error = function(e) appearances <- NA_character_)
    goals <- tryCatch(debs %>% rvest::html_nodes(".hauptlink+ .zentriert a") %>% rvest::html_text() %>%
      gsub("-", "0", .) %>% as.numeric(),
    error = function(e) goals <- NA_character_)
    minutes_played <- tryCatch(debs %>% rvest::html_nodes("td:nth-child(6)") %>% rvest::html_text() %>%
      gsub("\\.", "", .) %>% gsub("'", "", .) %>% gsub("-", "0", .) %>% as.numeric(),
    error = function(e) minutes_played <- NA_character_)
    debut_date <- tryCatch(debs %>% rvest::html_nodes(".spielDatum") %>% rvest::html_text() %>% .tm_fix_dates(),
    error = function(e) debut_date <- NA_character_)
    home_team <- tryCatch(debs %>% rvest::html_nodes(".rechts img") %>% rvest::html_attr("alt"),
    error = function(e) home_team <- NA_character_)
    away_team <- tryCatch(debs %>% rvest::html_nodes(".links img") %>% rvest::html_attr("alt"),
    error = function(e) away_team <- NA_character_)
    score <- tryCatch(debs %>% rvest::html_nodes(".ergebnis-link span") %>% rvest::html_text(),
    error = function(e) score <- NA_character_)
    home_goals <- tryCatch(gsub(":.*", "", score) %>% as.numeric(),
    error = function(e) home_goals <- NA_character_)
    away_goals <- tryCatch(gsub("*.:", "", score) %>% as.numeric(),
    error = function(e) away_goals <- NA_character_)
    opponent <- tryCatch(ifelse(debut_for == home_team, away_team, home_team),
    error = function(e) opponent <- NA_character_)
    goals_for <- tryCatch(ifelse(debut_for == home_team, home_goals, away_goals),
    error = function(e) goals_for <- NA_character_)
    goals_against <- tryCatch(ifelse(debut_for == home_team, away_goals, home_goals),
    error = function(e) goals_against <- NA_character_)
    age_debut <- tryCatch(debs %>% rvest::html_nodes(".match+ .hauptlink") %>% rvest::html_text() %>% stringr::str_squish(),
    error = function(e) age_debut <- NA_character_)
    value_at_debut <- tryCatch(debs %>% rvest::html_nodes(".hauptlink:nth-child(9)") %>% rvest::html_text(),
    error = function(e) value_at_debut <- NA_character_)
    player_market_value <- tryCatch(debs %>% rvest::html_nodes(".rechts.hauptlink~ .hauptlink+ .hauptlink") %>% rvest::html_text(),
    error = function(e) player_market_value <- NA_character_)

    out_df <- cbind(player_name, player_url, position, nationality, second_nationality,
                    debut_for, debut_date, opponent, goals_for, goals_against, age_debut, value_at_debut, player_market_value,
                    appearances, goals, minutes_played) %>% data.frame()

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
                    nationality = as.character(.data[["nationality"]]),
                    second_nationality = as.character(.data[["second_nationality"]]),
                    debut_for = as.character(.data[["debut_for"]]),
                    debut_date = lubridate::ymd(.data[["debut_date"]]),
                    opponent = as.character(.data[["opponent"]]),
                    goals_for = as.numeric(.data[["goals_for"]]),
                    goals_against = as.numeric(.data[["goals_against"]]),
                    age_debut = as.character(.data[["age_debut"]]),
                    value_at_debut = mapply(.convert_value_to_numeric, .data[["value_at_debut"]]),
                    player_market_value = mapply(.convert_value_to_numeric, .data[["player_market_value"]]),
                    appearances = as.numeric(.data[["appearances"]]),
                    goals = as.numeric(.data[["goals"]]),
                    minutes_played = as.numeric(.data[["minutes_played"]]))

    out_df <- out_df %>%
      dplyr::mutate(debut_type = debut_type)

    return(out_df)
  }

  f_possibly <- purrr::possibly(get_each_page, otherwise = data.frame(), quiet = FALSE)
  purrr::map_dfr(
    debutants_pages,
    f_possibly
  )

}

