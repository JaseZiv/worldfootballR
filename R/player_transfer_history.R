
.standardize_fee <- function(x) {
  case_when(
    grepl("free", x) ~ "Free transfer",
    grepl("end of", x) ~ "End of loan",
    grepl("fee", x) ~ "Paid loan",
    grepl("loan", x) ~ "Loan",
    grepl("m|th.|k", x) ~ "Transfer",
    TRUE ~ NA_character_
  )
}

#' Get Transfermarkt player transfer history
#'
#' Returns data frame of player(s) transfer history from transfermarkt.com
#' Replaces the deprecated function player_transfer_history
#'
#' @param player_urls the player url(s) from transfermarkt
#' @param get_extra_info allows users to decide if they want to scrape extra info (contract length, countries involved) or not
#'
#' @return returns a dataframe of player transfers
#'
#' @importFrom magrittr %>%
#' @importFrom xml2 read_html
#' @importFrom purrr map map_dfr
#' @importFrom rvest html_node html_nodes html_text html_children html_attr
#' @importFrom stringr str_squish
#' @importFrom httr GET add_headers content
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate bind_rows select
#' @importFrom progress progress_bar
#' @importFrom tidyselect vars_select_helpers
#'
#' @export
tm_player_transfer_history <- function(player_urls, get_extra_info = TRUE) {

  single_player_transfer_history <- function(player_url) {
    pb$tick()

    page <- xml2::read_html(player_url)

    player_name <- page %>%
      rvest::html_node("h1") %>%
      rvest::html_text() %>%
      gsub("#[[:digit:]]+ ", "", .) %>%
      stringr::str_squish()

    player_id <- basename(player_url)
    transfer_history_resp <- httr::GET(
      url = paste0("https://www.transfermarkt.com/ceapi/transferHistory/list/", player_id),
      httr::add_headers(.headers = c(
        `User-Agent` = getOption("worldfootballR.agent")
      ))
    )

    transfer_history_content <- content(transfer_history_resp)
    raw_transfer_history <- transfer_history_content[["transfers"]]

    formatted_transfer_history <- purrr::map(
      raw_transfer_history,
      \(.x) {
        list(
          "url" = .x[["url"]],
          "season" = .x[["season"]],
          "transfer_date" = lubridate::ymd(.x[["dateUnformatted"]]),
          "team_from" = .x[["from"]][["clubName"]],
          "team_to" = .x[["to"]][["clubName"]],
          "market_value" = .convert_value_to_numeric(.x[["marketValue"]]),
          "transfer_value" = .convert_value_to_numeric(.x[["fee"]]),
          "transfer_type" = .standardize_fee(.x[["fee"]])
        )
      }
    )

    # Executed if the user wants to get more info. Contains: Contract Expiring date + Days remaining. From which countries was the transfer.
    res <- if (isTRUE(get_extra_info)) {
      purrr::map_dfr(
        formatted_transfer_history,
        \(.x) {
          url <- .x$url
          extra_info <- tryCatch(
            xml2::read_html(paste0("https://www.transfermarkt.com", url)),
            error = function(e) NA
          )
          if (is.na(extra_info)) {
            return(.x)
          }
          contract_box <- extra_info %>%
            rvest::html_nodes(".large-4.columns") %>%
            rvest::html_node("table") %>%
            rvest::html_children()

          contract_idx <- contract_box %>%
            rvest::html_text() %>%
            grep("Remaining contract duration", .)

          if(is.na(extra_info)) {
            contract_expiry <- NA
            days_remaining <- NA
          } else {
            text_to_remove <- contract_box[contract_idx] %>%
              rvest::html_nodes("b") %>%
              rvest::html_text()

            if(length(text_to_remove) == 0) {
              contract_expiry <- NA
              days_remaining <- NA
            } else {

              contract_expiry <- contract_box[contract_idx] %>%
                rvest::html_text() %>%
                gsub(text_to_remove, "", .) %>%
                stringr::str_squish() %>%
                gsub(".*\\((.*)\\).*", "\\1", .) %>%
                .tm_fix_dates() %>%
                lubridate::ymd()

              days_remaining <- difftime(
                contract_expiry,
                .x$transfer_date,
                units = c("days")
              ) %>%
                as.numeric()
            }
          }

          country_to <- tryCatch(
            extra_info %>%
              rvest::html_nodes(".large-4.columns table .rechts .flaggenrahmen") %>%
              rvest::html_attr("title"),
            error = function(e) NA
          )

          if(length(country_to) < 1){
            country_to <- NA
          }

          countries <- tryCatch(
            extra_info %>%
              rvest::html_nodes(".large-4.columns table .flaggenrahmen") %>%
              rvest::html_attr("title"),
            error = function(e) NA
          )

          if (length(countries) < 2) {
            if (is.na(country_to)) {
              country_from <- countries[1]
            } else {
              country_from <- NA
            }
          } else {
            country_from <- countries[1]
          }

          .x[["contract_expiry"]] <- contract_expiry
          .x[["days_remaining"]] <- days_remaining
          .x[["country_from"]] <- country_from
          .x[["country_to"]] <- country_to
          .x
        }
      ) %>%
        ## for backwards compatability
        dplyr::select(
          tidyselect::vars_select_helpers$all_of(
            c(
              "season",
              "transfer_date",
              "country_from",
              "team_from",
              "country_to",
              "team_to",
              "market_value",
              "transfer_value",
              "transfer_type",
              "contract_expiry",
              "days_remaining"
            )
          )
        )
    } else {
      dplyr::bind_rows(formatted_transfer_history)
    }

    res <- dplyr::mutate(
      res,
      "player_name" = player_name,
      .before = 1
    )
    res[["url"]] <- NULL
    res
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(player_urls))
  purrr::map_dfr(
    player_urls,
    single_player_transfer_history
  )
}

#' Get player transfer history
#'
#' Returns data frame of player(s) transfer history from transfermarkt.com
#'
#' @param player_urls the player url(s) from transfermarkt
#'
#' @return returns a dataframe of player transfers
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
player_transfer_history <- function(player_urls) {

  .Deprecated("tm_player_transfer_history")
  tm_player_transfer_history(player_urls)
}

