#' Get transfermarkt player bios
#'
#' Returns data frame of player bios from transfermarkt.com
#'
#' @param player_urls player url(s) from transfermarkt
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
#' try({
#' player_url <- "https://www.transfermarkt.com/eden-hazard/profil/spieler/50202"
#' tm_player_bio(player_url)
#' tm_player_bio(player_urls = c("https://www.transfermarkt.com/eden-hazard/profil/spieler/50202",
#'                               "https://www.transfermarkt.com/sergio-ramos/profil/spieler/25557",
#'                               "https://www.transfermarkt.com/ivo-grbic/profil/spieler/226073"))
#' })
#' }
tm_player_bio <- function(player_urls) {

  # .pkg_message("Scraping player bios. Please acknowledge transfermarkt.com as the data source")

  each_bio <- function(player_url) {
    pb$tick()

    player_page <- tryCatch(xml2::read_html(player_url), error = function(e) NA)

    if(!is.na(player_page)) {
      player_name <- player_page %>% rvest::html_nodes("div h1") %>% rvest::html_text()
      # there was a change detected on 2022-04-12 of the name and valuation changing in the html
      player_name <- gsub("#[[:digit:]]+ ", "", player_name) %>% stringr::str_squish()

      # print(glue::glue("Scraping player_bio for {player_name}"))

      player_id <- stringr::str_extract(player_url, "(?<=spieler/)\\d+") %>% as.numeric()
      player_picture_url <- tryCatch({
        if(inherits(player_page, "xml_document")) {
          player_page %>%
            rvest::html_nodes(".data-header__profile-container img.data-header__profile-image") %>%
            rvest::html_attr("src")
        } else {
          NA_character_
        }
      }, error = function(e) {
        NA_character_
      }) %>%
        .replace_empty_na()

      X1 <- player_page %>% rvest::html_nodes(".info-table__content--regular") %>% rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na()
      X2 <- player_page %>% rvest::html_nodes(".info-table__content--bold") %>% rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na()

      a <- cbind(X1, X2) %>% data.frame()

      a <- a %>% dplyr::filter(!stringr::str_detect(.data[["X1"]], "Social-Media")) %>%
        dplyr::mutate(X1 = gsub(":", "", .data[["X1"]]))

      X2 <- tryCatch(player_page %>% rvest::html_nodes(".socialmedia-icons") %>% rvest::html_nodes("a") %>% rvest::html_attr("href"), error = function(e) NA_character_) %>% .replace_empty_na()
      X1 <- tryCatch(player_page %>% rvest::html_nodes(".socialmedia-icons") %>% rvest::html_nodes("a") %>% rvest::html_attr("title"), error = function(e) NA_character_) %>% .replace_empty_na()
      socials <- cbind(X1, X2)
      a <- rbind(a, socials) %>% dplyr::mutate(X1 = ifelse(.data[["X1"]] == "", "Website", .data[["X1"]]))
      # handle for duplicate socials
      a <- a %>% dplyr::distinct(X1, .keep_all = TRUE)

      player_val <- tryCatch(player_page %>% rvest::html_nodes(".data-header__market-value-wrapper") %>% rvest::html_text() %>%
                               stringr::str_squish() %>% gsub(" Last.*", "", .), error = function(e) NA_character_) %>% .replace_empty_na()
      player_val_max <- tryCatch(player_page %>% rvest::html_nodes(".max") %>% rvest::html_text() %>%
                               stringr::str_squish(), error = function(e) NA_character_) %>% .replace_empty_na()
      player_val_max_date <- tryCatch(player_page %>% rvest::html_nodes(".tm-player-market-value-development__max div") %>% .[3] %>% rvest::html_text() %>%
                                   stringr::str_squish(), error = function(e) NA_character_) %>% .replace_empty_na()
      val_df <- data.frame(X1=c("player_valuation", "max_player_valuation", "max_player_valuation_date"), X2=c(player_val, player_val_max, player_val_max_date))
      a <- rbind(a, val_df)

      squad_number_url <- gsub("profil", "rueckennummern", player_url)
      squad_number_page <- tryCatch(xml2::read_html(squad_number_url), error = function(e) NA)
      squad_number <- squad_number_page %>% rvest::html_node("tbody tr:first-child td:nth-child(4)") %>% rvest::html_text(trim = TRUE)

      a <- a %>%
        dplyr::mutate(player_name = player_name, player_id = player_id) %>%
        tidyr::pivot_wider(names_from = .data[["X1"]], values_from = .data[["X2"]]) %>%
        janitor::clean_names() %>%
        dplyr::mutate(squad_number = squad_number) %>%
        dplyr::mutate(player_valuation = .convert_value_to_numeric(euro_value = .data[["player_valuation"]]),
                      max_player_valuation = .convert_value_to_numeric(euro_value = .data[["max_player_valuation"]]),
                      max_player_valuation_date = .tm_fix_dates(dirty_dates = .data[["max_player_valuation_date"]])) %>%
        dplyr::mutate(URL = player_url, picture_url = player_picture_url) %>%
        dplyr::select(-na)
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
    # tm changed the column name from date_of_birth to date_of_birth_age - need to clean up the age in parentheses
    c_name <- names(full_bios[grepl("date_of_birth", colnames(full_bios))])
    full_bios <- full_bios %>%
      dplyr::mutate(date_of_birth = .tm_fix_dates(gsub(" \\(.*", "", .data[[c_name]])))

    # to not introduce breaking changes, we will keep the column name consistent with existing expected output and remove the new date_of_birth_age column
    full_bios[, c_name] <- NULL
  }

  if(any(grepl("joined", colnames(full_bios)))) {
    full_bios <- full_bios %>%
      dplyr::mutate(joined = .tm_fix_dates(.data[["joined"]]))
  }

  if(any(grepl("contract_expires", colnames(full_bios)))) {
    full_bios <- full_bios %>%
      dplyr::mutate(contract_expires = .tm_fix_dates(.data[["contract_expires"]]))
  }

  if(any(grepl("date_of_last_contract_extension", colnames(full_bios)))) {
    full_bios <- full_bios %>%
      dplyr::mutate(date_of_last_contract_extension = .tm_fix_dates(.data[["date_of_last_contract_extension"]]))
  }

  if(any(grepl("height", colnames(full_bios)))) {
    full_bios <- full_bios %>%
      dplyr::mutate(height = gsub(",", "\\.", .data[["height"]]) %>% gsub("m", "", .) %>% stringr::str_squish() %>% as.numeric())
  }

  return(full_bios)
}
