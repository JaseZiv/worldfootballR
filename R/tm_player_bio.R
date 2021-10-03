#' Get transfermarkt player bios
#'
#' Returns data frame of player bois from transfermarkt.com
#'
#' @param player_urls player url(s) from transfermarket
#'
#' @return returns a dataframe of player bios
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' player_url <- "https://www.transfermarkt.com/eden-hazard/profil/spieler/50202"
#' tm_player_bio(player_url)
#' tm_player_bio(player_urls = c("https://www.transfermarkt.com/eden-hazard/profil/spieler/50202",
#'                               "https://www.transfermarkt.com/sergio-ramos/profil/spieler/25557",
#'                               "https://www.transfermarkt.com/ivo-grbic/profil/spieler/226073"))
#' }
tm_player_bio <- function(player_urls) {

  print("Scraping player bios. Please acknowledge transfermarkt.com as the data source")

  each_bio <- function(player_url) {
    pb$tick()

    player_page <- tryCatch(xml2::read_html(player_url), error = function(e) NA)

    if(!is.na(player_page)) {
      player_name <- player_page %>% rvest::html_nodes("h1") %>% rvest::html_text()

      # print(glue::glue("Scraping player_bio for {player_name}"))

      X1 <- player_page %>% rvest::html_nodes(".info-table__content--regular") %>% rvest::html_text() %>% stringr::str_squish()
      X2 <- player_page %>% rvest::html_nodes(".info-table__content--bold") %>% rvest::html_text() %>% stringr::str_squish()

      a <- cbind(X1, X2) %>% data.frame()

      a <- a %>% dplyr::filter(!stringr::str_detect(.data$X1, "Social-Media")) %>%
        dplyr::mutate(X1 = gsub(":", "", .data$X1))

      X2 <- tryCatch(player_page %>% rvest::html_nodes(".socialmedia-icons") %>% rvest::html_nodes("a") %>% rvest::html_attr("href"), error = function(e) NA_character_)
      X1 <- tryCatch(player_page %>% rvest::html_nodes(".socialmedia-icons") %>% rvest::html_nodes("a") %>% rvest::html_attr("title"), error = function(e) NA_character_)
      socials <- cbind(X1, X2)
      a <- rbind(a, socials) %>% dplyr::mutate(X1 = ifelse(.data$X1 == "", "Website", .data$X1))

      player_val <- tryCatch(player_page %>% rvest::html_nodes(".dataMarktwert") %>% rvest::html_nodes("a") %>%
                               rvest::html_text() %>% strsplit(split = "  ") %>% .[[1]] %>% .[1], error = function(e) NA_character_)
      val_df <- data.frame(X1="player_valuation", X2=player_val)
      a <- rbind(a, val_df)

      a <- a %>%
        dplyr::mutate(player_name = player_name) %>%
        tidyr::pivot_wider(names_from = .data$X1, values_from = .data$X2) %>%
        janitor::clean_names() %>%
        dplyr::mutate(player_valuation = .convert_value_to_numeric(euro_value = .data$player_valuation)) %>%
        dplyr::mutate(URL = player_url)
    } else {
      a <- data.frame()
    }

    return(a)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(player_urls))

  full_bios <- player_urls %>%
    purrr::map_df(each_bio)


  # some of the following columns may not exist, so this series of if statements will handle for this:
  if(any(grepl("date_of_birth", colnames(full_bios)))) {
    full_bios <- full_bios %>%
      dplyr::mutate(date_of_birth = .tm_fix_dates(.data$date_of_birth))
  }

  if(any(grepl("joined", colnames(full_bios)))) {
    full_bios <- full_bios %>%
      dplyr::mutate(joined = .tm_fix_dates(.data$joined))
  }

  if(any(grepl("contract_expires", colnames(full_bios)))) {
    full_bios <- full_bios %>%
      dplyr::mutate(contract_expires = .tm_fix_dates(.data$contract_expires))
  }

  if(any(grepl("date_of_last_contract_extension", colnames(full_bios)))) {
    full_bios <- full_bios %>%
      dplyr::mutate(date_of_last_contract_extension = .tm_fix_dates(.data$date_of_last_contract_extension))
  }

  if(any(grepl("height", colnames(full_bios)))) {
    full_bios <- full_bios %>%
      dplyr::mutate(height = gsub(",", "\\.", .data$height) %>% gsub("m", "", .) %>% stringr::str_squish() %>% as.numeric())
  }

  return(full_bios)
}
