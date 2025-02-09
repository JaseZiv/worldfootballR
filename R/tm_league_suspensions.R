#' Get League Suspensions
#'
#' Returns a data frame of all suspended players in a selected league
#'
#' @param country_name The country of the league (used if league_url not provided)
#' @param league_url Optional direct league URL from transfermarkt.com
#' @return A data frame of suspended players
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' try({
#' tm_get_suspensions(country_name = "England")
#' tm_get_suspensions(league_url = "https://www.transfermarkt.com/premier-league/sperrenausfaelle/wettbewerb/GB1")
#' })
#' }
tm_get_suspensions <- function(country_name = NA, league_url = NA) {
  main_url <- "https://www.transfermarkt.com"

  .replace_empty_na <- function(x) {
    ifelse(is.na(x) | x == "", NA_character_, x)
  }

  if (is.na(league_url) && is.na(country_name)) {
    stop("You must provide either league_url or country_name")
  }

  if (is.na(league_url)) {
    tryCatch({
      meta_df <- utils::read.csv(url("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"),
                                 stringsAsFactors = FALSE)
      meta_filtered <- meta_df %>% dplyr::filter(.data[["country"]] == country_name)

      if (nrow(meta_filtered) == 0) {
        stop(glue::glue("Country {country_name} not found in metadata"))
      }

      comp_url <- meta_filtered$comp_url[1]
      comp_name <- meta_filtered$comp_name[1]
      country <- country_name
    }, error = function(e) {
      stop("Failed to retrieve league metadata")
    })
  } else {
    tryCatch({
      league_page <- xml2::read_html(league_url)
      comp_url <- league_url
      comp_name <- league_page %>%
        rvest::html_nodes(".data-header__headline-wrapper--oswald") %>%
        rvest::html_text() %>%
        stringr::str_squish()
      country <- league_page %>%
        rvest::html_nodes(".data-header img") %>%
        rvest::html_attr("alt") %>%
        .[1] %>%
        stringr::str_squish()
    }, error = function(e) {
      stop(glue::glue("Invalid league URL: {league_url}"))
    })
  }

  suspensions_url <- comp_url %>%
    gsub("startseite", "sperrenausfaelle", .) %>%
    paste0(., "/plus/1")

  tryCatch({
    page <- xml2::read_html(suspensions_url)
  }, error = function(e) {
    warning(glue::glue("Failed to read suspensions page: {suspensions_url}"))
    return(data.frame())
  })

  suspensions_data <- tryCatch({
    boxes <- page %>% rvest::html_nodes("div.box")
    suspensions_table <- boxes[[1]] %>% rvest::html_node("table.items")

    rows <- suspensions_table %>%
      rvest::html_nodes("tbody tr") %>%
      purrr::keep(~length(rvest::html_node(.x, "table.inline-table")) > 0)

    purrr::map_df(rows, ~{
      player_info <- .x %>% rvest::html_node("td:first-child table.inline-table")

      tibble::tibble(
        Player = tryCatch(player_info %>%
                            rvest::html_node("td:nth-child(2) a") %>%
                            rvest::html_text(trim = TRUE),
                          error = function(e) NA_character_),
        Position = tryCatch(player_info %>%
                              rvest::html_node("tr:nth-child(2) td") %>%
                              rvest::html_text(trim = TRUE),
                            error = function(e) NA_character_),
        Club = tryCatch(.x %>%
                          rvest::html_node("td:nth-child(2) a img") %>%
                          rvest::html_attr("title"),
                        error = function(e) NA_character_),
        Age = tryCatch(.x %>%
                         rvest::html_node("td:nth-child(3)") %>%
                         rvest::html_text(trim = TRUE) %>%
                         as.numeric(),
                       error = function(e) NA_real_),
        Reason = tryCatch(.x %>%
                            rvest::html_node("td:nth-child(4)") %>%
                            rvest::html_text(trim = TRUE),
                          error = function(e) NA_character_),
        Since = tryCatch(.x %>%
                           rvest::html_node("td:nth-child(5)") %>%
                           rvest::html_text(trim = TRUE) %>%
                           .tm_fix_dates() %>%
                           as.Date(),
                         error = function(e) NA_Date_),
        Until = tryCatch(.x %>%
                           rvest::html_node("td:nth-child(6)") %>%
                           rvest::html_text(trim = TRUE) %>%
                           .tm_fix_dates() %>%
                           as.Date(),
                         error = function(e) NA_Date_),
        Matches_Missed = tryCatch(.x %>%
                                    rvest::html_node("td:nth-child(7)") %>%
                                    rvest::html_text(trim = TRUE) %>%
                                    as.numeric(),
                                  error = function(e) NA_real_)
      )
    }) %>%
      dplyr::mutate(dplyr::across(where(is.character), .replace_empty_na))
  }, error = function(e) {
    warning("Failed to extract suspension data")
    return(data.frame())
  })

  if (nrow(suspensions_data) > 0) {
    suspensions_data <- suspensions_data %>%
      dplyr::mutate(
        Country = ifelse(is.na(league_url), country_name, country),
        Competition = comp_name,
        .before = 1
      )
  }

  return(suspensions_data)
}

#' Get League Risk of Suspension
#'
#' Returns a data frame of players at risk of suspension
#'
#' @inheritParams tm_get_suspensions
#' @return A data frame of players at risk of suspension
#' @export
#' @examples
#' \dontrun{
#' try({
#' tm_get_risk_of_suspension(country_name = "England")
#' tm_get_risk_of_suspension(league_url = "https://www.transfermarkt.com/premier-league/sperrenausfaelle/wettbewerb/GB1")
#' })
#' }
tm_get_risk_of_suspension <- function(country_name, league_url = NA) {
  main_url <- "https://www.transfermarkt.com"

  .replace_empty_na <- function(x) {
    ifelse(is.na(x) | x == "", NA_character_, x)
  }

  if(is.na(league_url)) {
    tryCatch({
      meta_df <- utils::read.csv(url("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv"),
                                 stringsAsFactors = FALSE)
      meta_filtered <- meta_df %>% dplyr::filter(.data[["country"]] == country_name)
      comp_url <- meta_filtered$comp_url[1]
      comp_name <- meta_filtered$comp_name[1]
    }, error = function(e) {
      stop("Failed to retrieve league metadata")
    })
  } else {
    tryCatch({
      league_page <- xml2::read_html(league_url)
      comp_url <- league_url
      comp_name <- league_page %>%
        rvest::html_nodes(".data-header__headline-wrapper--oswald") %>%
        rvest::html_text() %>%
        stringr::str_squish()
      country <- league_page %>%
        rvest::html_nodes(".data-header img") %>%
        rvest::html_attr("alt") %>%
        .[1] %>%
        stringr::str_squish()
    }, error = function(e) {
      stop(glue::glue("Invalid league URL: {league_url}"))
    })
  }

  risk_url <- comp_url %>%
    gsub("startseite", "sperrenausfaelle", .) %>%
    paste0(., "/plus/1")

  tryCatch({
    page <- xml2::read_html(risk_url)
  }, error = function(e) {
    warning(glue::glue("Failed to read risk page: {risk_url}"))
    return(data.frame())
  })

  risk_data <- tryCatch({
    boxes <- page %>% rvest::html_nodes("div.box")
    risk_table <- boxes[[2]] %>% rvest::html_node("table.items")

    rows <- risk_table %>%
      rvest::html_nodes("tbody tr") %>%
      purrr::keep(~length(rvest::html_node(.x, "table.inline-table")) > 0)

    purrr::map_df(rows, ~{
      player_info <- .x %>% rvest::html_node("td:first-child table.inline-table")

      tibble::tibble(
        Player = tryCatch(player_info %>% rvest::html_node("td:nth-child(2) a") %>%
                            rvest::html_text(trim = TRUE), error = function(e) NA),
        Position = tryCatch(player_info %>% rvest::html_node("tr:nth-child(2) td") %>%
                              rvest::html_text(trim = TRUE), error = function(e) NA),
        Club = tryCatch(.x %>% rvest::html_node("td:nth-child(2) a img") %>%
                          rvest::html_attr("title") %>% trimws(), error = function(e) NA),
        Age = tryCatch(.x %>% rvest::html_node("td:nth-child(3)") %>%
                         rvest::html_text(trim = TRUE) %>% as.numeric(), error = function(e) NA),
        Yellow_Cards = tryCatch(.x %>% rvest::html_node("td:nth-child(4)") %>%
                                  rvest::html_text(trim = TRUE) %>% as.numeric(), error = function(e) NA)
      )
    }) %>%
      dplyr::mutate(dplyr::across(where(is.character), .replace_empty_na))
  }, error = function(e) {
    warning("Failed to extract risk data")
    return(data.frame())
  })

  if(is.na(league_url)) {
    risk_data %>%
      dplyr::mutate(Country = country_name,
                    Competition = comp_name,
                    .before = 1)
  } else {
    risk_data %>%
      dplyr::mutate(Country = country,
                    Competition = comp_name,
                    .before = 1)
  }
}
