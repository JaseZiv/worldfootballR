#' @importFrom rvest html_elements html_text html_table html_attr
#' @importFrom xml2 read_html
#' @importFrom purrr pluck
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across
.tm_each_absence_page <- function(absence_page_url) {

  absence_pg <- xml2::read_html(absence_page_url)

  # get the main table, knowing that come columns won't be returned
  main_df <- absence_pg |> rvest::html_elements("#yw1 .items") |> rvest::html_table() |> data.frame()

  # create an object of each table row and the col headings
  tab_rows <- absence_pg |> rvest::html_elements("#yw1 .items tbody tr")
  tab_head <- absence_pg |> rvest::html_elements("#yw1 .items tr th") |> rvest::html_text()

  # index of columns we need to get extra html elements for
  competition_idx <- grep("competition", tolower(tab_head))
  club_missed_idx <- grep("games missed", tolower(tab_head))


  # parse competiton name
  comp_name <- c()
  for(i in 1:length(tab_rows)) {
    each <- tab_rows[i] |>
      rvest::html_elements("td") |>
      purrr::pluck(competition_idx) |>
      rvest::html_elements("img") |> rvest::html_attr("title") |>
      .replace_empty_na()

    comp_name <- c(comp_name, each)
  }


  # parse team name
  club_name <- c()
  for(i in 1:length(tab_rows)) {
    each <- tab_rows[i] |>
      rvest::html_elements("td") |>
      purrr::pluck(club_missed_idx) |>
      rvest::html_elements("a") |> rvest::html_attr("title") |>
      .replace_empty_na()

    club_name <- c(club_name, each)
  }


  main_df$Competition <- comp_name
  main_df$club_missed <- club_name

  main_df <- main_df |>
    dplyr::mutate(dplyr::across(c(from, until), .tm_fix_dates)) |>
    janitor::clean_names()

  return(main_df)

}



#' Get Player Absences
#'
#' Returns data frame of a player's absences from suspension from transfermarkt.com
#'
#' @param player_urls player url(s) from transfermarkt
#'
#' @return returns a dataframe
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' player_urls <- c("https://www.transfermarkt.com/cristian-romero/profil/spieler/355915",
#' "https://www.transfermarkt.com/micky-van-de-ven/profil/spieler/557459")
#'
#' df <- tm_get_player_absence(player_urls)
#' })
#' }
#'
tm_get_player_absence <- function(player_urls) {


  .tm_each_players_absence <- function(player_url) {

    # pb$tick()

    main_url <- "https://www.transfermarkt.com"

    # player_url <- "https://www.transfermarkt.com/cristian-romero/profil/spieler/355915"
    #
    # # change the url to point to the absences url
    # "https://www.transfermarkt.com/cristian-romero/ausfaelle/spieler/355915"

    player_url_changed <- gsub("profil", "ausfaelle", player_url)



    player_page <- xml2::read_html(player_url_changed)

    player_name <- player_page %>% rvest::html_nodes("h1") %>% rvest::html_text() %>% gsub("#[0-9]+ ", "", .) %>% stringr::str_squish()
    player_meta <- data.frame(player_name = player_name,
                              player_url = player_url)

    absence_paginated <- player_page |>
      rvest::html_elements(".tm-pagination__list-item a") |> rvest::html_attr("href") |>
      unique()

    if(length(absence_paginated) == 0) {
      absence_paginated <- player_url_changed
    } else {
      absence_paginated <- paste0(main_url, absence_paginated)
    }


    f_possibly <- purrr::possibly(.tm_each_absence_page, otherwise = data.frame(), quiet = FALSE)
    player_out <- purrr::map_dfr(
      absence_paginated,
      f_possibly
    )

    player_out <- dplyr::bind_cols(player_meta, player_out)

    return(player_out)

  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(player_urls))

  f_possibly2 <- purrr::possibly(.tm_each_players_absence, otherwise = data.frame(), quiet = FALSE)
  purrr::map_dfr(
    player_urls,
    f_possibly2
  )

}



