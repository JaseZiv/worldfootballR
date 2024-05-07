#' Get player injury history
#'
#' Returns data frame of a player's injury history transfermarkt.com
#'
#' @param player_urls player url(s) from transfermarkt
#'
#' @return returns a dataframe of player injury history
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
tm_player_injury_history <- function(player_urls) {

  main_url <- "https://www.transfermarkt.com"

  # .pkg_message("Scraping player bios. Please acknowledge transfermarkt.com as the data source")

  each_player <- function(player_url) {
    pb$tick()

    # player_bio <- tm_player_bio(player_urls = player_url)

    player_url_fixed <- gsub("profil", "verletzungen", player_url) %>% paste0(., "/plus/1")

    injury_page <- xml2::read_html(player_url_fixed)

    player_name <- injury_page %>% rvest::html_nodes("h1") %>% rvest::html_text() %>% gsub("#[0-9]+ ", "", .) %>% stringr::str_squish()

    injury_urls <- injury_page %>% rvest::html_nodes(".tm-pagination__list-item a") %>% rvest::html_attr("href")

    if(length(injury_urls)==0) {
      injury_pages <- player_url_fixed
    } else {
      injury_pages <- injury_urls %>%
        paste0(main_url, .) %>% unique()
    }


    get_each_page <- function(page_url) {

      if(length(injury_urls) == 0) {
        pg <- tryCatch(injury_page %>% rvest::html_nodes("#yw1") %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children(),
                       error = function(e) pg <- NA_character_)
      } else {
        pg <- xml2::read_html(page_url) %>% rvest::html_nodes("#yw1") %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children()
      }

      get_each_tab_row <- function(pg_row) {

        season_injured <- tryCatch(pg_row %>% rvest::html_nodes("td:nth-child(1)") %>% rvest::html_text(),
                                   error = function(e) season_injured <- NA_character_) %>% .replace_empty_na()
        injury <- tryCatch(pg_row %>% rvest::html_nodes(".zentriert+ .hauptlink") %>% rvest::html_text(),
                           error = function(e) injury <- NA_character_) %>% .replace_empty_na()
        injured_since <- tryCatch(pg_row %>% rvest::html_nodes(".hauptlink+ .zentriert") %>% rvest::html_text() %>% .tm_fix_dates(),
                                  error = function(e) injured_since <- NA_character_) %>% .replace_empty_na()
        injured_until <- tryCatch(pg_row %>% rvest::html_nodes(".zentriert+ td.zentriert") %>% rvest::html_text() %>% .tm_fix_dates(),
                                  error = function(e) injured_until <- NA_character_) %>% .replace_empty_na()
        duration <- tryCatch(pg_row %>% rvest::html_nodes(".zentriert+ td.rechts") %>% rvest::html_text(),
                             error = function(e) duration <- NA_character_) %>% .replace_empty_na()
        games_missed <- tryCatch(pg_row %>% rvest::html_nodes(".wappen_verletzung") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings(),
                                 error = function(e) games_missed <- NA_integer_) %>% .replace_empty_na()
        club_missed_games_for <- tryCatch(pg_row %>% rvest::html_nodes("img") %>% rvest::html_attr("alt"),
                                          error = function(e) goals <- NA_character_) %>% .replace_empty_na() |> paste0(collapse = ", ")


        df <- cbind(player_name, player_url, season_injured, injury, injured_since, injured_until, duration, games_missed, club_missed_games_for) %>%
          suppressWarnings() %>% data.frame()

        df <- df %>%
          dplyr::mutate(player_name = as.character(.data[["player_name"]]),
                        player_url = as.character(.data[["player_url"]]),
                        season_injured = as.character(.data[["season_injured"]]),
                        injury = as.character(.data[["injury"]]),
                        injured_since = lubridate::ymd(.data[["injured_since"]]),
                        injured_until = lubridate::ymd(.data[["injured_until"]]),
                        duration = as.character(.data[["duration"]]),
                        games_missed = as.character(.data[["games_missed"]]),
                        club_missed_games_for = as.character(.data[["club_missed_games_for"]]))

        return(df)
      }

      out_df <- pg |>
        purrr::map_dfr(get_each_tab_row)


      return(out_df)
    }

    f_possibly <- purrr::possibly(get_each_page, otherwise = data.frame(), quiet = FALSE)
    player_df <- purrr::map_dfr(
      injury_pages,
      f_possibly
    )

    # player_df <- cbind(player_bio, player_df)

    return(player_df)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(player_urls))

  f_possibly2 <- purrr::possibly(each_player, otherwise = data.frame(), quiet = FALSE)
  purrr::map_dfr(
    player_urls,
    f_possibly2
  )

}
