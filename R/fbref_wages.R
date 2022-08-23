#' Get team player wages
#'
#' Returns all player wages from FBref via Capology
#'
#' @param team_urls the URL(s) of the teams(s) (can come from fb_teams_urls())
#' @param time_pause the wait time (in seconds) between page loads
#'
#' @return returns a dataframe with all available estimated player wages for the selected team(s)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' # for single teams:
#' man_city_url <- "https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats"
#' fb_squad_wages(team_urls = man_city_url)
#' })
#' }
fb_squad_wages <- function(team_urls, time_pause=3) {

  time_wait <- time_pause
  main_url <- "https://fbref.com"

  each_fb_squad_wages <- function(team_url, time_pause=time_wait) {
    pb$tick()

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    pg <- .load_page(team_url)

    team_name <- sub('.*\\/', '', team_url) %>% gsub("-Stats", "", .) %>% gsub("-", " ", .)
    league <- pg %>% rvest::html_elements(".prevnext+ p a") %>% rvest::html_text()
    season <- pg %>% rvest::html_nodes("h1") %>% rvest::html_text() %>% stringr::str_squish() %>% sub(" .*", "", .)

    # because current season team urls don't contain the season, need to do this additional page load
    # there would be too many flakey assumptions to stitch this together
    # might look at storing data on all team URLs for the leagues, which would then allow this to be read in and only one page load
    wage_url <- pg %>% rvest::html_elements("#inner_nav .hoversmooth a")
    idx <- grep("Wages", rvest::html_text(wage_url))
    wage_url <- wage_url[idx] %>% rvest::html_attr("href") %>% unique() %>% paste0(main_url, .)

    Sys.sleep(time_pause)
    wage_pg <- tryCatch(.load_page(wage_url), error = function(e) NA)

    if(!is.na(wage_pg)) {

      tab_box <- wage_pg %>% rvest::html_nodes(".stats_table")
      tab_element <- tab_box %>% rvest::html_nodes("tbody tr")

      # get player urls, row by row
      player_urls <- c()
      for(i in tab_element) {
        each_url <- i %>% rvest::html_elements("th a") %>% rvest::html_attr("href") %>%
          paste0(main_url, .)
        # the below will catch when there is no player URL and it returns NA
        if(each_url == "https://fbref.com") {
          each_url <- NA_character_
        }
        player_urls <- c(player_urls, each_url)
      }

      # first, want a raw df which cleans the Nation col, separates the columns into each currency and appends player urls
      dat_raw <- tab_box %>%
        rvest::html_table() %>% data.frame() %>%
        dplyr::filter(!grepl("Players", .data$Player)) %>%
        dplyr::mutate(Nation = gsub(".*? ", "", .data$Nation)) %>%
        tidyr::separate(col = .data$Weekly.Wages, into = c("WeeklyWage1", "WeeklyWage2"), sep = "\\(") %>%
        tidyr::separate(col = .data$WeeklyWage2, into = c("WeeklyWage2", "WeeklyWage3"), sep = ", ") %>%
        tidyr::separate(col = .data$Annual.Wages, into = c("AnnualWage1", "AnualWage2"), sep = "\\(") %>%
        tidyr::separate(col = .data$AnualWage2, into = c("AnnualWage2", "AnnualWage3"), sep = "\\, ") %>% suppressWarnings() %>%
        dplyr::mutate(Url = player_urls)


      #---- Codes for currency -----#
      # # EUR: "(\u20AC)"
      #
      # # GBP: "(\u00A3)"
      #
      # # USD: "\\$"

      # now make a long df, rename columns based on currency and empty records where there is no wage provided
      dat_long <- dat_raw %>%
        tidyr::pivot_longer(cols = tidyselect::contains("Wage"), names_to = "pay_type") %>%
        dplyr::mutate(value = gsub(" ", "", .data$value) %>% gsub(",", "", .) %>% gsub("\\)", "", .)) %>%
        dplyr::mutate(currency = dplyr::case_when(
          grepl("(\u20AC)", .data$value) ~ "EUR",
          grepl("(\u00A3)", .data$value) ~ "GBP",
          grepl("\\$", .data$value) ~ "USD",
          TRUE ~ NA_character_
        )) %>%
        dplyr::mutate(pay_type = gsub("\\d+", "", .data$pay_type)) %>%
        dplyr::mutate(pay_type = dplyr::case_when(
          is.na(.data$currency) ~ NA_character_,
          !is.na(.data$currency) ~ paste0(.data$pay_type, .data$currency)
        )) %>%
        dplyr::mutate(value = gsub("[^\x20-\x7E]", "", .data$value) %>% gsub("\\$", "", .) %>% as.numeric())

      # finally convert it back to a wide df, doing this only for players who have a value in wages
      dat_wide <- dat_long %>%
        dplyr::filter(!is.na(.data$value)) %>%
        tidyr::pivot_wider(id_cols = c(.data$Player, .data$Nation, .data$Pos, .data$Age, .data$Notes, .data$Url),
                           names_from = .data$pay_type, values_from = .data$value) %>%
        # then we join back the empty wage players so as not to stuff up our pivot_wider
        dplyr::bind_rows(
          dat_long %>%
            dplyr::filter(is.na(.data$value)) %>%
            dplyr::select(.data$Player, .data$Nation, .data$Pos, .data$Age, .data$Notes, .data$Url) %>% dplyr::distinct()
        )

      # now add metadata
      dat_wide <- dat_wide %>%
        dplyr::mutate(Team = team_name,
               Comp = league,
               Season = season) %>%
        dplyr::select(.data$Team, .data$Comp, .data$Season, .data$Player, .data$Nation, .data$Pos, .data$Age, tidyselect::contains("Wage"), .data$Notes, .data$Url)

    } else {
      print(glue::glue("NOTE: Wage data is not available for {team_url}. Check {team_url} to see if it exists."))
      dat_wide <- data.frame()
    }

    return(dat_wide)

  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(team_urls))

  all_team_wages <- team_urls %>%
    purrr::map_df(each_fb_squad_wages)

  return(all_team_wages)

}

