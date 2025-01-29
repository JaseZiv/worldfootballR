#' Get Player Attributes
#'
#' Return a data frame of a player's info
#'
#' @param url URL of the player's profile page on fbref.com
#'
#' @return A data frame
#' @export

get_player_info <- function(url) {
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
  age <- page %>% rvest::html_node(xpath = "//nobr[contains(., 'Age:')]") %>% rvest::html_text2() %>%
    stringr::str_extract("\\d+-\\d+d")

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
