#' Get fbref Full Player Scouting Report
#'
#' Returns the scouting report for a selected player
#'
#' @param player_url the URL of the player (can come from fb_player_urls())
#' @param pos_versus either "primary" or "secondary" as fbref offer comparisons against multiple positions
#'
#' @return returns a dataframe of a player's full scouting information
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fb_player_scouting_report(player_url = "https://fbref.com/en/players/d70ce98e/Lionel-Messi",
#' pos_versus = "primary")
#' fb_player_scouting_report(player_url = "https://fbref.com/en/players/d70ce98e/Lionel-Messi",
#' pos_versus = "secondary")
#' }
fb_player_scouting_report <- function(player_url, pos_versus) {

  main_url <- "https://fbref.com"
  player_page <- xml2::read_html(player_url)

  player_name <- player_page %>% rvest::html_node("h1") %>% rvest::html_text() %>% stringr::str_squish()

  print(glue::glue("Scraping full scouting report for {player_name}"))

  main_cats <- player_page %>% rvest::html_nodes("#inner_nav") %>% rvest::html_nodes(".full.hasmore")
  span_names <- main_cats %>% rvest::html_nodes("span") %>% rvest::html_text()
  main_cats <- main_cats[grep("Scouting Report", span_names)]

  scout_level1_url <- main_cats %>% rvest::html_nodes("ul li") %>% .[1] %>%
    rvest::html_nodes("a") %>% rvest::html_attr("href") %>% paste0(main_url, .)


  scout_pg <- xml2::read_html(scout_level1_url)

  outer <- scout_pg %>% rvest::html_nodes("#all_scout") %>% rvest::html_nodes(".filter.switcher") %>% rvest::html_nodes("div")

  if(length(outer) == 1) {
    pos_versus <- 1
    versus <- outer %>% rvest::html_text() %>%
      stringr::str_squish()
  } else if (length(outer) == 2) {
    if(pos_versus != "primary") {
      pos_versus <- 2
    } else {
      pos_versus <- 1
    }

    versus <- outer %>% rvest::html_text() %>%
      stringr::str_squish() %>% .[pos_versus]
  } else {
    stop(glue::glue("Full scouting report not available for {player_name}"))
  }


  scouting_all <- scout_pg %>% rvest::html_nodes("#all_scout") %>% rvest::html_nodes(".table_container")


  scout_pos <- scouting_all[pos_versus] %>%
    rvest::html_nodes("table") %>% rvest::html_table() %>% data.frame()

  missing_idx <- scout_pos[,1] != ""
  scout_pos <- scout_pos[missing_idx,]

  names(scout_pos) <- scout_pos[1,]
  scout_pos <- scout_pos %>%
    dplyr::rename(Per90=.data$`Per 90`)

  df <- data.frame(Statistic="Standard", Per90="Standard", Percentile="Standard")
  scout_pos <- rbind(df, scout_pos)
  scout_pos$stat_group <- NA_character_

  stat_names <- scout_pos$Statistic
  idx <- grep("Statistic", stat_names)

  stat_vct <- c()
  for(i in 1:length(idx)) {
    id <- idx[i]-1
    st <- stat_names[id]
    tryCatch(scout_pos[c(id:(idx[i+1]-2)), "stat_group"] <- st, error = function(e) scout_pos[c(id:nrow(scout_pos)), "stat_group"] <- st)
  }
  scout_pos$stat_group[is.na(scout_pos$stat_group)] <- st
  scout_pos <- scout_pos[-c(idx, idx-1), ]

  scout_pos <- scout_pos %>%
    dplyr::mutate(Player=player_name,
                  Versus=gsub("vs. ", "", versus),
                  Per90 = gsub("\\+", "", .data$Per90) %>% gsub("\\%", "", .) %>% gsub("\\,", "", .) %>% as.numeric(),
                  Percentile = as.numeric(.data$Percentile)) %>%
    dplyr::select(.data$Player, .data$Versus, StatGroup=.data$stat_group, dplyr::everything())

  mins_played <- player_page %>% rvest::html_nodes("#all_scout")%>% rvest::html_nodes(".footer") %>% rvest::html_nodes("strong") %>%
    rvest::html_text() %>% gsub(" minutes", "", .) %>% as.numeric() %>% unique()

  scout_pos <- scout_pos %>%
    dplyr::mutate(BasedOnMinutes = mins_played)

  return(scout_pos)

}
