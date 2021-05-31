#' Big 5 Euro League Season Stats
#'
#' Returns data frame of selected statistics for seasons of the big 5 Euro leagues, for either
#' whole team or individual players.
#' Multiple seasons can be passed to the function, but only one `stat_type` can be selected
#'
#' @param season_end_year the year(s) the season concludes
#' @param stat_type the type of team statistics the user requires
#' @param team_or_player result either summarised for each team, or individual players
#'
#' The statistic type options (stat_type) include:
#'
#' \emph{"standard"}, \emph{"shooting"}, \emph{"passing"}, \emph{"passing"_types},
#' \emph{"gca"}, \emph{"defense"}, \emph{"possession"}, \emph{"playing_time"},
#' \emph{"misc"}, \emph{"keepers"}, \emph{"keepers_adv"}
#'
#' @return returns a dataframe of a selected team or player statistic type for a selected season(s)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' fb_big5_advanced_season_stats(season_end_year=2021,stat_type="possession",team_or_player="player")
#' }

fb_big5_advanced_season_stats <- function(season_end_year, stat_type, team_or_player) {

  stat_types <- c("standard", "shooting", "passing", "passing_types", "gca", "defense", "possession", "playing_time", "misc", "keepers", "keepers_adv")

  if(!stat_type %in% stat_types) stop("check stat type")

  print(glue::glue("Scraping {team_or_player} season '{stat_type}' stats"))

  main_url <- "https://fbref.com"

  season_end_year_num <- season_end_year

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  seasons_urls <- seasons %>%
    dplyr::filter(stringr::str_detect(.data$competition_type, "Big 5 European Leagues")) %>%
    dplyr::filter(season_end_year %in% season_end_year_num) %>%
    dplyr::arrange(season_end_year) %>%
    dplyr::pull(seasons_urls) %>% unique()


  get_each_big5_stats_type <- function(season_url) {

    pb$tick()

    if(stat_type == "standard") {
      stat_type <- "stats"
    }

    # fixes the change fbref made with the name of playing time
    if(stat_type == "playing_time") {
      stat_type <- "playingtime"
    }

    # fixes the change fbref made with the name of advanced keepers
    if(stat_type == "keepers_adv") {
      stat_type <- "keepersadv"
    }

    season_stats_page <- xml2::read_html(season_url)

    if(team_or_player == "player") {
      player_squad_ixd <- 1
    } else {
      player_squad_ixd <- 2
    }

    stat_urls <- season_stats_page %>%
      rvest::html_nodes(".hoversmooth") %>%
      rvest::html_nodes(".full") %>%
      rvest::html_nodes("ul") %>% .[player_squad_ixd] %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      paste0(main_url, .)

    stat_urls <- stat_urls[grepl(paste0(stat_type, "/"), stat_urls)]

    team_page <- stat_urls %>%
      xml2::read_html()

    if(team_or_player == "player") {
      stat_df <- team_page %>%
        rvest::html_nodes(".table_container") %>%
        rvest::html_nodes("table") %>%
        rvest::html_table() %>%
        data.frame()
    } else {
      stat_df <- team_page  %>%
        rvest::html_nodes(".table_container") %>%
        rvest::html_nodes("table")

      stat_df_for <- stat_df[1] %>%
        rvest::html_table() %>%
        data.frame()
      stat_df_against <- stat_df[2] %>%
        rvest::html_table() %>%
        data.frame()

      stat_df <- rbind(stat_df_for, stat_df_against)

    }

    var_names <- stat_df[1,] %>% as.character()

    new_names <- paste(var_names, names(stat_df), sep = "_")

    if(stat_type == "playingtime") {
      new_names <- new_names %>%
        gsub("\\..[[:digit:]]+", "", .) %>%
        gsub("\\.[[:digit:]]+", "", .) %>%
        gsub("_Var", "", .) %>%
        gsub("# Pl", "Num_Players", .) %>%
        gsub("%", "_percent", .) %>%
        gsub("_Performance", "", .) %>%
        # gsub("_Penalty", "", .) %>%
        gsub("1/3", "Final_Third", .) %>%
        gsub("/", "_per_", .) %>%
        gsub("-", "_minus_", .) %>%
        gsub("90s", "Mins_Per_90", .) %>%
        gsub("\\+", "plus", .)
    } else {
      new_names <- new_names %>%
        gsub("\\..*", "", .) %>%
        gsub("_Var", "", .) %>%
        gsub("# Pl", "Num_Players", .) %>%
        gsub("%", "_percent", .) %>%
        gsub("_Performance", "", .) %>%
        # gsub("_Penalty", "", .) %>%
        gsub("1/3", "Final_Third", .) %>%
        gsub("/", "_per_", .) %>%
        gsub("-", "_minus_", .) %>%
        gsub("90s", "Mins_Per_90", .)
    }

    names(stat_df) <- new_names
    stat_df <- stat_df[-1,]

    stat_df <- stat_df %>%
      dplyr::filter(.data$Rk != "Rk") %>%
      dplyr::select(-.data$Rk)

    if(team_or_player == "player") {
      stat_df <- stat_df %>%
        dplyr::select(-.data$Matches)

      cols_to_transform <- stat_df %>%
        dplyr::select(-.data$Squad, -.data$Player, -.data$Nation, -.data$Pos, -.data$Comp, -.data$Age) %>% names()

      chr_vars_to_transform <- stat_df %>%
        dplyr::select(.data$Nation, .data$Comp) %>% names()
    } else {
      cols_to_transform <- stat_df %>%
        dplyr::select(-.data$Squad, -.data$Comp) %>% names()

      chr_vars_to_transform <- stat_df %>%
        dplyr::select(.data$Comp) %>% names()
    }


    stat_df <- stat_df %>%
      dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub(",", "", x)}) %>%
      dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub("+", "", x)}) %>%
      dplyr::mutate_at(.vars = cols_to_transform, .funs = as.numeric) %>%
      dplyr::mutate_at(.vars = chr_vars_to_transform, .funs = function(x) {gsub("^\\S* ", "", x)})


    stat_df <- stat_df %>%
      dplyr::mutate(Team_or_Opponent = ifelse(!stringr::str_detect(.data$Squad, "vs "), "team", "opponent")) %>%
      dplyr::filter(.data$Team_or_Opponent == "team") %>%
      dplyr::bind_rows(
        stat_df %>%
          dplyr::mutate(Team_or_Opponent = ifelse(!stringr::str_detect(.data$Squad, "vs "), "team", "opponent")) %>%
          dplyr::filter(.data$Team_or_Opponent == "opponent")
      ) %>%
      dplyr::mutate(season_url = season_url,
                    Squad = gsub("vs ", "", .data$Squad)) %>%
      dplyr::select(.data$season_url, .data$Squad, .data$Comp, .data$Team_or_Opponent, dplyr::everything())


    stat_df <- seasons %>%
      dplyr::select(Season_End_Year=.data$season_end_year, .data$seasons_urls) %>%
      dplyr::left_join(stat_df, by = c("seasons_urls" = "season_url")) %>%
      dplyr::select(-.data$seasons_urls) %>%
      dplyr::filter(!is.na(.data$Squad)) %>%
      dplyr::arrange(.data$Season_End_Year, .data$Squad, dplyr::desc(.data$Team_or_Opponent))

    if(team_or_player == "player") {
      stat_df$Team_or_Opponent <- NULL
    }

    return(stat_df)

  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(seasons_urls))

  all_stats_df <- seasons_urls %>%
    purrr::map_df(get_each_big5_stats_type)

  return(all_stats_df)

}
