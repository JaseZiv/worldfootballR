#' Get Staff Member's job history
#'
#' Returns all roles a selected staff member(s) has held and performance data
#'
#' @param staff_urls transfermarkt.com staff(s) url (can use tm_league_staff_urls() to get)
#'
#' @return returns a data frame of all roles a selected staff member(s) has held and performance data
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export

tm_staff_job_history <- function(staff_urls) {

  get_each_staff <- function(staff_url) {
    pb$tick()

    main_staff_url <- staff_url
    staff_url <- staff_url %>% gsub("profil", "stationen", .) %>% paste0(., "/plus/1")

    staff_pg <- xml2::read_html(staff_url)

    name <- staff_pg %>% rvest::html_nodes("h1") %>% rvest::html_text() %>% stringr::str_squish()
    current_club <- staff_pg %>% rvest::html_nodes(".data-header__club a") %>% rvest::html_text() %>% .replace_empty_na()
    current_role <- staff_pg %>% rvest::html_nodes(".data-header__label b") %>% rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na()
    metadata <- staff_pg %>% rvest::html_nodes(".data-header__details")

    data_head_top <- metadata %>% rvest::html_nodes(".data-header__items li") %>% rvest::html_text() %>% stringr::str_squish()
    # data_vals <- metadata %>% rvest::html_nodes(".data-header__content") %>% rvest::html_text() %>% stringr::str_squish()

    meta_df <- data.frame(data_head_top) %>%
      tidyr::separate(data_head_top, into = c("data_head", "data_vals"), sep = ":") %>%
      tidyr::pivot_wider(names_from = data_head, values_from = data_vals) %>%
      janitor::clean_names()

    meta_df <- meta_df %>% tidyr::separate(.data[["date_of_birth_age"]], into = "date_of_birth", sep = " \\(") %>% suppressWarnings()

    staff_hist <- staff_pg %>% rvest::html_nodes("#yw1") %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children()

    club <- staff_hist %>% rvest::html_nodes(".no-border-links a") %>% rvest::html_text()
    position <- staff_hist %>% rvest::html_nodes("td:nth-child(2)") %>% rvest::html_text()
    appointed <- staff_hist %>% rvest::html_nodes(".no-border-links+ .zentriert") %>% rvest::html_text() %>%
      gsub(".*\\(", "", .) %>% gsub("\\)", "", .) %>% stringr::str_squish()
    contract_expiry <- staff_hist %>% rvest::html_nodes("td:nth-child(4)") %>% rvest::html_text() %>%
      gsub(".*\\(", "", .) %>% gsub("\\)", "", .) %>% stringr::str_squish() %>% gsub("expected ", "", .)
    days_in_charge <- staff_hist %>% rvest::html_nodes("td:nth-child(5)") %>% rvest::html_text() %>% gsub(" Days", "", .) %>% as.numeric()
    matches <- staff_hist %>% rvest::html_nodes("td:nth-child(6)") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings()
    wins <- staff_hist %>% rvest::html_nodes("td:nth-child(7)") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings()
    draws <- staff_hist %>% rvest::html_nodes("td:nth-child(8)") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings()
    losses <- staff_hist %>% rvest::html_nodes("td:nth-child(9)") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings()
    players_used <- staff_hist %>% rvest::html_nodes("td:nth-child(10)") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings()
    goals <- staff_hist %>% rvest::html_nodes("td:nth-child(11)") %>% rvest::html_text()
    avg_goals_for <- gsub(":.*", "", goals) %>% stringr::str_squish() %>% as.numeric()
    avg_goals_against <- gsub(".*:", "", goals) %>% stringr::str_squish() %>% as.numeric()
    ppm <- staff_hist %>% rvest::html_nodes("td:nth-child(12)") %>% rvest::html_text() %>% as.numeric() %>% suppressWarnings()

    staff_df <- cbind(name, current_club, current_role, meta_df, position, club, appointed, contract_expiry, days_in_charge, matches, wins, draws, losses, players_used, avg_goals_for, avg_goals_against, ppm)

    clean_role <- function(dirty_string, dirt) {
      gsub(dirt, "", dirty_string)
    }

    staff_df <- staff_df %>%
      dplyr::mutate(position = mapply(clean_role, dirty_string=position, dirt=club)) %>%
      dplyr::mutate(appointed = .tm_fix_dates(.data[["appointed"]]),
             contract_expiry = .tm_fix_dates(.data[["contract_expiry"]])) %>%
      dplyr::mutate(staff_url = main_staff_url)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(staff_urls))

  f_possibly <- purrr::possibly(get_each_staff, otherwise = data.frame(), quiet = FALSE)
  purrr::map_dfr(
    staff_urls,
    f_possibly
  )

}
