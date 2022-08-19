#' Get team staff history
#'
#' Returns all people who have held the selected role in a team's history
#'
#' @param team_urls transfermarkt.com team(s) url for a season
#' @param staff_role the role description which can be found here:
#' https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/transfermarkt_staff/tm_staff_types.csv
#'
#' @return returns a data frame of all selected staff roles for a team(s) history
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export

tm_team_staff_history <- function(team_urls, staff_role = "Manager") {

  main_url <- "https://www.transfermarkt.com"

  tm_staff_types <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/transfermarkt_staff/tm_staff_types.csv", stringsAsFactors = F)

  if(!tolower(staff_role) %in% tolower(tm_staff_types$staff_type_text)) stop("Check that staff role exists...")

  tm_staff_idx <- tm_staff_types$staff_type_idx[tolower(tm_staff_types$staff_type_text) == tolower(staff_role)]

  get_each_team_staff_history <- function(team_url) {
    pb$tick()
    manager_history_url <- gsub("startseite", "mitarbeiterhistorie", team_url) %>% gsub("saison_id.*", "", .) %>% paste0(., "personalie_id/", tm_staff_idx, "/plus/1")

    history_pg <- .load_page(manager_history_url)

    team_name <- history_pg %>% rvest::html_nodes("h1") %>% rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na()
    league <- history_pg %>% rvest::html_nodes(".hauptpunkt a") %>% rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na()
    country <- history_pg %>% rvest::html_nodes(".mediumpunkt img") %>% rvest::html_attr("title") %>% .replace_empty_na()

    mgrs <- history_pg %>% rvest::html_nodes("#yw1") %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children()

    mgr_df <- data.frame()

    for(each_row in 1:length(mgrs)) {
      mgr_df[each_row, "staff_name"] <- tryCatch(mgrs[each_row] %>% rvest::html_node(".hauptlink") %>% rvest::html_nodes("a") %>% rvest::html_text(),
                                                 error = function(e)  mgr_df[each_row, "manager_name"] <- NA_character_)
      mgr_df[each_row, "staff_url"] <- tryCatch(mgrs[each_row] %>% rvest::html_node(".hauptlink") %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% paste0(main_url, .),
                                                error = function(e) mgr_df[each_row, "manager_url"] <- NA_character_)
      mgr_df[each_row, "staff_dob"] <- tryCatch(mgrs[each_row] %>% rvest::html_node(".inline-table tr+ tr td") %>% rvest::html_text(),
                                                error = function(e) mgr_df[each_row, "dob"] <- NA_character_)
      mgr_df[each_row, "staff_nationality"] <- tryCatch(mgrs[each_row] %>% rvest::html_nodes(".zentriert .flaggenrahmen") %>% .[[1]] %>% rvest::html_attr("title"),
                                                        error = function(e) mgr_df[each_row, "staff_nationality"] <- NA_character_)
      mgr_df[each_row, "staff_nationality_secondary"] <- tryCatch(mgrs[each_row] %>% rvest::html_nodes(".zentriert .flaggenrahmen") %>% .[[2]] %>% rvest::html_attr("title"),
                                                                  error = function(e) mgr_df[each_row, "staff_nationality_secondary"] <- NA_character_)
      mgr_df[each_row, "appointed"] <- tryCatch(mgrs[each_row] %>% rvest::html_nodes("td:nth-child(3)") %>% rvest::html_text() %>% .tm_fix_dates(),
                                                error = function(e) mgr_df[each_row, "appointed"] <- NA_character_)
      mgr_df[each_row, "end_date"] <- tryCatch(mgrs[each_row] %>% rvest::html_nodes("td:nth-child(4)") %>% rvest::html_text() %>% .tm_fix_dates(),
                                               error = function(e) mgr_df[each_row, "end_date"] <- NA_character_)
      mgr_df[each_row, "days_in_post"] <- tryCatch(mgrs[each_row] %>% rvest::html_nodes("td.rechts") %>% rvest::html_text(),
                                                   error = function(e) mgr_df[each_row, "days_in_post"] <- NA_character_)
      mgr_df[each_row, "matches"] <- tryCatch(mgrs[each_row] %>% rvest::html_nodes(".rechts+ td") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings(),
                                              error = function(e) mgr_df[each_row, "matches"] <- NA_character_)
      mgr_df[each_row, "wins"] <- tryCatch(mgrs[each_row] %>% rvest::html_nodes("td:nth-child(7)") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings(),
                                           error = function(e) mgr_df[each_row, "wins"] <- NA_character_)
      mgr_df[each_row, "draws"] <- tryCatch(mgrs[each_row] %>% rvest::html_nodes("td:nth-child(8)") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings(),
                                            error = function(e) mgr_df[each_row, "draws"] <- NA_character_)
      mgr_df[each_row, "losses"] <- tryCatch(mgrs[each_row] %>% rvest::html_nodes("td:nth-child(9)") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings(),
                                             error = function(e) mgr_df[each_row, "losses"] <- NA_character_)
      mgr_df[each_row, "ppg"] <- tryCatch(mgrs[each_row] %>% rvest::html_nodes("td:nth-child(10)") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings(),
                                          error = function(e) mgr_df[each_row, "ppg"] <- NA_character_)
    }

    # add metadata
    mgr_df <- cbind(team_name, league, country, staff_role, mgr_df)

    mgr_df <- mgr_df %>%
     dplyr:: mutate(matches = dplyr::case_when( is.na(.data$matches) ~ 0, TRUE ~ .data$matches),
             wins = dplyr::case_when( is.na(.data$wins) ~ 0, TRUE ~ .data$wins),
             draws = dplyr::case_when( is.na(.data$draws) ~ 0, TRUE ~ .data$draws),
             losses = dplyr::case_when( is.na(.data$losses) ~ 0, TRUE ~ .data$losses),
             ppg = dplyr::case_when( is.na(.data$ppg) ~ 0, TRUE ~ .data$ppg),
             ) %>%
      dplyr::mutate(appointed = lubridate::ymd(.data$appointed),
             end_date = lubridate::ymd(.data$end_date)) %>%
      dplyr::mutate(days_in_post = dplyr::case_when(is.na(.data$end_date) ~ as.numeric(lubridate::today() - .data$appointed), TRUE ~ as.numeric(.data$end_date - .data$appointed)))

    return(mgr_df)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(team_urls))

  f_possibly <- purrr::possibly(get_each_team_staff_history, otherwise = data.frame(), quiet = FALSE)
  purrr::map_dfr(
    team_urls,
    f_possibly
  )

}

