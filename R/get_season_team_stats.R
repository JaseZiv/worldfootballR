#' Get FBref season team stats
#'
#' Returns different team season statistics results for a given league season and stat type
#' Replaces the deprecated function get_season_team_stats
#'
#' @param country the three character country code for all countries
#' @param gender gender of competition, either "M", "F" or both
#' @param season_end_year the year the season(s) concludes
#' @param tier the tier of the league, ie '1st' for the EPL or '2nd' for the Championship and so on
#' @param stat_type the type of team statistics the user requires
#' @param time_pause the wait time (in seconds) between page loads
#'
#' The statistic type options (stat_type) include:
#'
#' \emph{"league_table"}, \emph{"league_table_home_away}", \emph{"standard"},
#' \emph{"keeper"}, \emph{"keeper_adv"}, \emph{"shooting"}, \emph{"passing"},
#' \emph{"passing_types"}, \emph{"goal_shot_creation"}, \emph{"defense" },
#' \emph{"possession"}, \emph{"playing_time"}, \emph{"misc"}
#'
#' @return returns a dataframe of a selected team statistic type for a selected league season
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' fb_season_team_stats("ITA", "M", 2021, "1st", "defense")
#' })
#' }
fb_season_team_stats <- function(country, gender, season_end_year, tier, stat_type, time_pause=3) {

  # .pkg_message("Scraping season {stat_type} stats")

  main_url <- "https://fbref.com"

  country_abbr <- country
  gender_M_F <- gender
  season_end_year_num <- season_end_year
  comp_tier <- tier

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  filtered_seasons <- seasons %>%
    dplyr::filter(stringr::str_detect(.data[["competition_type"]], "Leagues")) %>%
    dplyr::filter(.data[["country"]] %in% country_abbr,
                  .data[["gender"]] %in% gender_M_F,
                  .data[["season_end_year"]] %in% season_end_year_num,
                  .data[["tier"]] %in% comp_tier) %>%
    dplyr::arrange(.data[["season_end_year"]])

  seasons_urls <- filtered_seasons %>%
    dplyr::pull(.data[["seasons_urls"]]) %>%
    unique()

  time_wait <- time_pause

  get_each_stats_type <- function(single_season_url, time_pause=time_wait) {

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    competition_name <- filtered_seasons %>%
      dplyr::filter(.data[["seasons_urls"]] == single_season_url) %>%
      dplyr::pull(competition_name) %>%
      unique()

    season_stats_page <- .load_page(single_season_url)

    # have included this to differentiate between how different leagues handle ladders/tables
    league_tables <- season_stats_page %>% rvest::html_nodes("#content .table_wrapper") %>% rvest::html_elements("h2") %>% rvest::html_text()

    league_tables_idx <- 1

    league_standings <- season_stats_page %>% rvest::html_nodes("table")

    all_stat_tabs_holder <- season_stats_page %>% rvest::html_nodes(".stats_table")

    all_tables <- c()

    for(i in 1:length(all_stat_tabs_holder)) {
      all_tables <- c(all_tables, xml2::xml_attrs(all_stat_tabs_holder[[i]])[["id"]])
    }

    tryCatch(
      if(stat_type == "league_table") {
        stats_url <- NA
        if(any(grepl("Conference", all_tables, ignore.case = TRUE))) {
          stat_df1 <- league_standings[1] %>% rvest::html_table() %>% data.frame()
          conf1 <- season_stats_page %>% rvest::html_nodes(".section_content") %>% rvest::html_nodes("h2") %>% .[1] %>% rvest::html_text()
          stat_df1 <- stat_df1 %>%
            dplyr::mutate(Conference = conf1)

          stat_df2 <- league_standings[3] %>% rvest::html_table() %>% data.frame()
          conf2 <- season_stats_page %>% rvest::html_nodes(".section_content") %>% rvest::html_nodes("h2") %>% .[3] %>% rvest::html_text()
          stat_df2 <- stat_df2 %>%
            dplyr::mutate(Conference = conf2)

          stat_df <- dplyr::bind_rows(stat_df1, stat_df2)
        } else {
          # the first (min()) table with the header League Table or Regular Season is the overall table:
          stat_df <- league_standings[min(league_tables_idx)] %>% rvest::html_table() %>% data.frame()
        }

        if(any(grepl("Attendance", names(stat_df)))) {
          stat_df$Attendance <- gsub(",", "", stat_df$Attendance) %>% as.numeric()
        }

      } else if(stat_type == "league_table_home_away") {
        stats_url <- NA
        if(any(grepl("Conference", all_tables, ignore.case = TRUE))) {
          stat_df1 <- league_standings[2] %>% rvest::html_table() %>% data.frame()
          conf1 <- season_stats_page %>% rvest::html_nodes(".section_content") %>% rvest::html_nodes("h2") %>% .[2] %>% rvest::html_text()
          stat_df1 <- stat_df1 %>%
            dplyr::mutate(Conference = conf1)

          stat_df2 <- league_standings[4] %>% rvest::html_table() %>% data.frame()
          conf2 <- season_stats_page %>% rvest::html_nodes(".section_content") %>% rvest::html_nodes("h2") %>% .[4] %>% rvest::html_text()
          stat_df2 <- stat_df2 %>%
            dplyr::mutate(Conference = conf2)

          stat_df <- dplyr::bind_rows(stat_df1, stat_df2)
          stat_df$Conference[1] <- "Conference"
        } else {
          stat_df <- league_standings[grepl("home_away",all_tables)] %>% rvest::html_table() %>% data.frame()
        }

        var_names <- stat_df[1,] %>% as.character()

        new_names <- paste(var_names, names(stat_df), sep = "_")

        new_names <- new_names %>%
          gsub("\\..*", "", .) %>%
          gsub("_Var", "", .) %>%
          gsub("/", "_per_", .)

        names(stat_df) <- new_names
        stat_df <- stat_df[-1,]

        stat_df <- stat_df %>%
          dplyr::filter(.data[["Rk"]] != "Rk") %>%
          dplyr::rename(Conference = .data[["Conference_Conference"]])

        cols_to_transform <- stat_df %>%
          dplyr::select(-.data[["Squad"]], -.data[["Conference"]]) %>% names()

        stat_df <- stat_df %>%
          dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub(",", "", x)}) %>%
          dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub("+", "", x)}) %>%
          dplyr::mutate_at(.vars = cols_to_transform, .funs = as.numeric)

      } else if(stat_type == "standard") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_standard_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_standard_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "keeper") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_keeper_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_keeper_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "keeper_adv") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_keeper_adv_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_keeper_adv_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "shooting") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_shooting_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_shooting_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "passing") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_passing_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_passing_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "passing_types") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_passing_types_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_passing_types_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "goal_shot_creation") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_gca_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_gca_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "defense") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_defense_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_defense_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "possession") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_possession_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_possession_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "playing_time") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_playing_time_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_playing_time_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "misc") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_misc_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_misc_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())
      }, error = function(e)  {stat_df <- data.frame()} )

    if(nrow(stat_df) == 0) {
      stat_df <- data.frame()
      print(glue::glue("NOTE: Stat Type '{stat_type}' is not found for this league season. Check {single_season_url} to see if it exists."))

    } else {
      # stopifnot("Data not available, see message above" = length(stats_url) > 0)
      names(stat_df) <- gsub("\\+", "_plus_", names(stat_df))


      stat_df <- stat_df %>%
        dplyr::mutate(Team_or_Opponent = ifelse(!stringr::str_detect(.data[["Squad"]], "vs "), "team", "opponent")) %>%
        dplyr::filter(.data[["Team_or_Opponent"]] == "team") %>%
        dplyr::bind_rows(
          stat_df %>%
            dplyr::mutate(Team_or_Opponent = ifelse(!stringr::str_detect(.data[["Squad"]], "vs "), "team", "opponent")) %>%
            dplyr::filter(.data[["Team_or_Opponent"]] == "opponent")
        ) %>%
        dplyr::mutate(season_url = single_season_url) %>%
        dplyr::select(.data[["season_url"]], .data[["Squad"]], .data[["Team_or_Opponent"]], dplyr::everything())

      stat_df <- seasons %>%
        dplyr::select(Competition_Name=.data[["competition_name"]], Gender=.data[["gender"]], Country=.data[["country"]], Season_End_Year=.data[["season_end_year"]], .data[["seasons_urls"]]) %>%
        dplyr::left_join(stat_df, by = c("seasons_urls" = "season_url")) %>%
        dplyr::select(-.data[["seasons_urls"]]) %>%
        dplyr::filter(!is.na(.data[["Squad"]])) %>%
        dplyr::arrange(.data[["Country"]], .data[["Competition_Name"]], .data[["Gender"]], .data[["Season_End_Year"]], dplyr::desc(.data[["Team_or_Opponent"]]), .data[["Squad"]]) %>% dplyr::distinct(.keep_all = T)
    }


    return(stat_df)

  }

  all_stats_df <- seasons_urls %>%
    purrr::map_df(get_each_stats_type)

  return(all_stats_df)

}




#' Get season team stats
#'
#' Returns different team season statistics results for a given league season and stat type
#'
#' @param country the three character country code for all countries
#' @param gender gender of competition, either "M", "F" or both
#' @param season_end_year the year the season(s) concludes
#' @param tier the tier of the league, ie '1st' for the EPL or '2nd' for the Championship and so on
#' @param stat_type the type of team statistics the user requires
#' @param time_pause the wait time (in seconds) between page loads
#'
#' The statistic type options (stat_type) include:
#'
#' \emph{"league_table"}, \emph{"league_table_home_away}", \emph{"standard"},
#' \emph{"keeper"}, \emph{"keeper_adv"}, \emph{"shooting"}, \emph{"passing"},
#' \emph{"passing_types"}, \emph{"goal_shot_creation"}, \emph{"defense" },
#' \emph{"possession"}, \emph{"playing_time"}, \emph{"misc"}
#'
#' @return returns a dataframe of a selected team statistic type for a selected league season
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' try({
#' get_season_team_stats("ITA", "M", 2021, "1st", "defense")
#' })
#' }
get_season_team_stats <- function(country, gender, season_end_year, tier, stat_type, time_pause=3) {

  .Deprecated("fb_season_team_stats")

  # .pkg_message("Scraping season {stat_type} stats")

  main_url <- "https://fbref.com"

  country_abbr <- country
  gender_M_F <- gender
  season_end_year_num <- season_end_year
  comp_tier <- tier

  seasons <- read.csv("https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/all_leages_and_cups/all_competitions.csv", stringsAsFactors = F)

  seasons_urls <- seasons %>%
    dplyr::filter(stringr::str_detect(.data[["competition_type"]], "Leagues")) %>%
    dplyr::filter(country %in% country_abbr,
                  gender %in% gender_M_F,
                  season_end_year %in% season_end_year_num,
                  tier %in% comp_tier) %>%
    dplyr::arrange(season_end_year) %>%
    dplyr::pull(seasons_urls) %>% unique()

  time_wait <- time_pause

  get_each_stats_type <- function(season_url, time_pause=time_wait) {

    # put sleep in as per new user agreement on FBref
    Sys.sleep(time_pause)

    season_stats_page <- xml2::read_html(season_url)

    league_standings <- season_stats_page %>% rvest::html_nodes("table")

    all_stat_tabs_holder <- season_stats_page %>% rvest::html_nodes(".stats_table")

    all_tables <- c()

    for(i in 1:length(all_stat_tabs_holder)) {
      all_tables <- c(all_tables, xml2::xml_attrs(all_stat_tabs_holder[[i]])[["id"]])
    }

    tryCatch(
      if(stat_type == "league_table") {
        stats_url <- NA
        if(any(grepl("Conference", all_tables, ignore.case = TRUE))) {
          stat_df1 <- league_standings[1] %>% rvest::html_table() %>% data.frame()
          conf1 <- season_stats_page %>% rvest::html_nodes(".section_content") %>% rvest::html_nodes("h2") %>% .[1] %>% rvest::html_text()
          stat_df1 <- stat_df1 %>%
            dplyr::mutate(Conference = conf1)

          stat_df2 <- league_standings[3] %>% rvest::html_table() %>% data.frame()
          conf2 <- season_stats_page %>% rvest::html_nodes(".section_content") %>% rvest::html_nodes("h2") %>% .[3] %>% rvest::html_text()
          stat_df2 <- stat_df2 %>%
            dplyr::mutate(Conference = conf2)

          stat_df <- dplyr::bind_rows(stat_df1, stat_df2)
        } else {
          stat_df <- league_standings[1] %>% rvest::html_table() %>% data.frame()
        }

        if(any(grepl("Attendance", names(stat_df)))) {
          stat_df$Attendance <- gsub(",", "", stat_df$Attendance) %>% as.numeric()
        }

      } else if(stat_type == "league_table_home_away") {
        stats_url <- NA
        if(any(grepl("Conference", all_tables, ignore.case = TRUE))) {
          stat_df1 <- league_standings[2] %>% rvest::html_table() %>% data.frame()
          conf1 <- season_stats_page %>% rvest::html_nodes(".section_content") %>% rvest::html_nodes("h2") %>% .[2] %>% rvest::html_text()
          stat_df1 <- stat_df1 %>%
            dplyr::mutate(Conference = conf1)

          stat_df2 <- league_standings[4] %>% rvest::html_table() %>% data.frame()
          conf2 <- season_stats_page %>% rvest::html_nodes(".section_content") %>% rvest::html_nodes("h2") %>% .[4] %>% rvest::html_text()
          stat_df2 <- stat_df2 %>%
            dplyr::mutate(Conference = conf2)

          stat_df <- dplyr::bind_rows(stat_df1, stat_df2)
          stat_df$Conference[1] <- "Conference"
        } else {
          stat_df <- league_standings[2] %>% rvest::html_table() %>% data.frame()
        }

        var_names <- stat_df[1,] %>% as.character()

        new_names <- paste(var_names, names(stat_df), sep = "_")

        new_names <- new_names %>%
          gsub("\\..*", "", .) %>%
          gsub("_Var", "", .) %>%
          gsub("/", "_per_", .)

        names(stat_df) <- new_names
        stat_df <- stat_df[-1,]

        stat_df <- stat_df %>%
          dplyr::filter(.data[["Rk"]] != "Rk") %>%
          dplyr::rename(Conference = .data[["Conference_Conference"]])

        cols_to_transform <- stat_df %>%
          dplyr::select(-.data[["Squad"]], -.data[["Conference"]]) %>% names()

        stat_df <- stat_df %>%
          dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub(",", "", x)}) %>%
          dplyr::mutate_at(.vars = cols_to_transform, .funs = function(x) {gsub("+", "", x)}) %>%
          dplyr::mutate_at(.vars = cols_to_transform, .funs = as.numeric)

      } else if(stat_type == "standard") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_standard_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_standard_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "keeper") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_keeper_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_keeper_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "keeper_adv") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_keeper_adv_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_keeper_adv_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "shooting") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_shooting_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_shooting_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "passing") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_passing_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_passing_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "passing_types") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_passing_types_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_passing_types_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "goal_shot_creation") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_gca_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_gca_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "defense") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_defense_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_defense_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "possession") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_possession_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_possession_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "playing_time") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_playing_time_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_playing_time_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())

      } else if(stat_type == "misc") {
        stat_df_for <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_misc_for"))]), error = function(e) data.frame())
        stat_df_against <- tryCatch(.clean_advanced_stat_table(input_table_element = all_stat_tabs_holder[which(stringr::str_detect(all_tables, "stats_squads_misc_against"))]), error = function(e) data.frame())
        stat_df <- tryCatch(rbind(stat_df_for, stat_df_against), error = function(e) data.frame())
      }, error = function(e)  {stat_df <- data.frame()} )

    if(nrow(stat_df) == 0) {
      stat_df <- data.frame()
      print(glue::glue("NOTE: Stat Type '{stat_type}' is not found for this league season. Check {season_url} to see if it exists."))

    } else {
      # stopifnot("Data not available, see message above" = length(stats_url) > 0)
      names(stat_df) <- gsub("\\+", "_plus_", names(stat_df))


      stat_df <- stat_df %>%
        dplyr::mutate(Team_or_Opponent = ifelse(!stringr::str_detect(.data[["Squad"]], "vs "), "team", "opponent")) %>%
        dplyr::filter(.data[["Team_or_Opponent"]] == "team") %>%
        dplyr::bind_rows(
          stat_df %>%
            dplyr::mutate(Team_or_Opponent = ifelse(!stringr::str_detect(.data[["Squad"]], "vs "), "team", "opponent")) %>%
            dplyr::filter(.data[["Team_or_Opponent"]] == "opponent")
        ) %>%
        dplyr::mutate(season_url = season_url) %>%
        dplyr::select(.data[["season_url"]], .data[["Squad"]], .data[["Team_or_Opponent"]], dplyr::everything())

      stat_df <- seasons %>%
        dplyr::select(Competition_Name=.data[["competition_name"]], Gender=.data[["gender"]], Country=.data[["country"]], Season_End_Year=.data[["season_end_year"]], .data[["seasons_urls"]]) %>%
        dplyr::left_join(stat_df, by = c("seasons_urls" = "season_url")) %>%
        dplyr::select(-.data[["seasons_urls"]]) %>%
        dplyr::filter(!is.na(.data[["Squad"]])) %>%
        dplyr::arrange(.data[["Country"]], .data[["Competition_Name"]], .data[["Gender"]], .data[["Season_End_Year"]], dplyr::desc(.data[["Team_or_Opponent"]]), .data[["Squad"]]) %>% dplyr::distinct(.keep_all = T)
    }


    return(stat_df)

  }

  all_stats_df <- seasons_urls %>%
    purrr::map_df(get_each_stats_type)

  return(all_stats_df)

}
