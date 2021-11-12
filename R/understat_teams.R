#' Get Understat team info
#'
#' Retrieve Understat team metadata. Similar to `understatr::get_team_meta`.
#'
#' @param team_names a vector of team names (can be just 1)
#'
#' @return a data.frame
#'
#' @export
#' @examples \dontrun{
#' understat_team_meta(team_name = c("Liverpool", "Manchester City"))
#' }
understat_team_meta <- function(team_names) {
  f_possibly <- purrr::possibly(.understat_team_meta, otherwise = data.frame())
  purrr::map_dfr(team_names, f_possibly)
}

#' @importFrom stringr str_replace_all
#' @importFrom rvest read_html html_nodes html_attr html_text
.understat_team_meta <- function(team_name) {
  print(glue::glue("Scraping {team_name} metadata."))

  main_url <- "https://understat.com"

  team_name <- stringr::str_replace_all(team_name, " ", "_")
  team_url <- paste(main_url, "team", team_name, sep = "/")

  team_page <- xml2::read_html(team_url)
  year_link <- rvest::html_nodes(team_page, "#header :nth-child(2)")
  year_options <- rvest::html_nodes(year_link[2], "option")

  team_df <- data.frame(
    team_name = team_name,
    year = as.numeric(rvest::html_attr(year_options, "value")),
    season = rvest::html_text(year_options),
    stringsAsFactors = FALSE
  )

  team_df$url <- paste(team_url, team_df$year, sep = "/")

  return(team_df)

}

#' Get Understat team player stats
#'
#' Retrieve Understat team player stats. Similar to `understatr::get_team_players_stats`.
#'
#' @inheritParams understat_team_meta
#' @param years a vector of years/seasons (can be just 1)
#'
#' @return a tibble
#'
#' @export
#' @examples \dontrun{
#' understat_team_players_stats(team_names = c("Liverpool", "Manchester City"), years = c(2019, 2020))
#' }
understat_team_players_stats <- function(team_names, years) {
  df <- expand.grid(
    team_name = team_names,
    year = years,
    stringsAsFactors = FALSE
  )

  f_possibly <- purrr::possibly(.understat_team_players_stats, otherwise = data.frame(), quiet = FALSE)
  purrr::map2_dfr(
    df$team_name, df$year,
    f_possibly
  )
}

.understat_team_players_stats <- function(team_name, year) {
  print(glue::glue("Scraping {team_name} player stats for the {year} season. Please acknowledge understat.com as the data source."))

  main_url <- "https://understat.com"

  team_name <- stringr::str_replace_all(team_name, " ", "_")
  team_url <- paste(main_url, "team", team_name, year, sep = "/")

  players_data <- team_url %>% .get_clean_understat_json("playersData")

  players_data$year <- as.numeric(year)
  names(players_data)[names(players_data) == "team_title"] <- "team_name"
  names(players_data)[names(players_data) == "id"] <- "player_id"

  withr::local_options(list(readr.num_columns = 0))
  players_data <- readr::type_convert(players_data)

  return(players_data)
}
