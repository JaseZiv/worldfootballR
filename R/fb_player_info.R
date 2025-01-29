#' Get Player Attributes
#'
#' Return a data frame of a player's info
#'
#' @param player_urls the URL(s) of the player(s)
#' @param time_pause the wait time (in seconds) between page loads
#'
#' @return A data frame
#' @export
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' player_urls <- c(
#'   "https://fbref.com/en/players/d70ce98e/", # Messi
#'   "https://fbref.com/en/players/3515d404/" # JWP
#' )
#' fb_player_info(player_urls)
#' })
#' }
fb_player_info <- function(player_urls, time_pause = 3) {

  get_each_player_info <- function(url) {
    pb$tick()

    Sys.sleep(time_pause)

    page <- xml2::read_html(url)

    full_name <- page %>% rvest::html_node("h1") %>% rvest::html_text2() %>% stringr::str_trim()

    position_node <- page %>% rvest::html_node(xpath = "//p[contains(., 'Position:')]")
    position_footed <- position_node %>% rvest::html_text2() %>% stringr::str_trim()
    position <- stringr::str_split(position_footed, "▪", 2)[[1]][1] %>%
      stringr::str_remove("Position:") %>% stringr::str_trim()
    footed <- stringr::str_split(position_footed, "▪", 2)[[1]][2] %>%
      stringr::str_remove("Footed:") %>% stringr::str_trim()

    height_weight <- page %>% rvest::html_node(xpath = "//p[contains(., 'cm')]") %>% rvest::html_text2()
    height <- stringr::str_extract(height_weight, "\\d+cm")
    weight <- stringr::str_extract(height_weight, "\\d+kg")

    birth_date <- page %>% rvest::html_node("#necro-birth") %>% rvest::html_attr("data-birth")

    # Calculates age from birth_date
    age <- NA
    birth_date_clean <- .replace_empty_na(birth_date)

    if (!is.na(birth_date_clean)) {
      birth_date_date <- as.Date(birth_date_clean)
      today <- Sys.Date()

      # Calculates years
      years <- as.integer(format(today, "%Y")) - as.integer(format(birth_date_date, "%Y"))
      current_year_birthday <- as.Date(paste0(format(today, "%Y"), "-", format(birth_date_date, "%m-%d")))

      # Handles invalid dates (e.g., February 29 in non-leap years)
      if (is.na(current_year_birthday)) {
        current_year_birthday <- as.Date(paste0(format(today, "%Y"), "-03-01"))
      }

      # Adjusts years and find last valid birthday
      if (current_year_birthday > today) {
        years <- years - 1
        last_birthday <- as.Date(paste0(as.integer(format(today, "%Y")) - 1, "-", format(birth_date_date, "%m-%d")))
        # Handles invalid adjusted dates
        if (is.na(last_birthday)) {
          last_birthday <- as.Date(paste0(as.integer(format(today, "%Y")) - 1, "-03-01"))
        }
      } else {
        last_birthday <- current_year_birthday
      }

      # Calculates days since last birthday
      days <- as.integer(difftime(today, last_birthday, units = "days"))
      age <- paste0(years, "-", days, "d")
    }

    birth_place <- page %>% rvest::html_node(xpath = "//p[contains(., 'Born:')]//span[contains(., 'in ')]") %>%
      rvest::html_text2() %>% stringr::str_remove("^in ") %>% stringr::str_trim()

    national_team <- page %>% rvest::html_node(xpath = "//p[contains(., 'National Team:')]/a") %>% rvest::html_text2()
    club <- page %>% rvest::html_node(xpath = "//p[contains(., 'Club:')]/a") %>% rvest::html_text2()

    wages <- page %>% rvest::html_node(".important.poptip") %>% rvest::html_text2() %>% stringr::str_trim()

    twitter <- page %>% rvest::html_node(xpath = "//p[contains(., 'Twitter:')]/a") %>% rvest::html_text2()
    instagram <- page %>% rvest::html_node(xpath = "//p[contains(., 'Instagram:')]/a") %>% rvest::html_text2()

    data.frame(
      full_name = .replace_empty_na(full_name),
      position = .replace_empty_na(position),
      footed = .replace_empty_na(footed),
      height = .replace_empty_na(height),
      weight = .replace_empty_na(weight),
      birth_date = .replace_empty_na(birth_date),
      age = .replace_empty_na(age),
      birth_place = .replace_empty_na(birth_place),
      national_team = .replace_empty_na(national_team),
      club = .replace_empty_na(club),
      wages = .replace_empty_na(wages),
      twitter = .replace_empty_na(twitter),
      instagram = .replace_empty_na(instagram),
      stringsAsFactors = FALSE
    )
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(player_urls))

  player_urls %>%
    purrr::map_df(get_each_player_info)

}
