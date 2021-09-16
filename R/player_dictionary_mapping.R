#' Player Mapping Dictionary
#'
#' Returns data frame of players from the top 5 Euro leagues, their player URL
#' and their respective Transfermarkt URL.
#' Currently only for the players who have been in the top 5 leagues since
#' the 2017-2018 season
#'
#' @return returns a dataframe of FBref players and respective Transfermarkt URL
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom readr read_csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' mapped_players <- player_dictionary_mapping()
#' }


player_dictionary_mapping <- function() {
  players_mapped <- readr::read_csv("https://github.com/JaseZiv/worldfootballR_data/raw/master/raw-data/fbref-tm-player-mapping/output/fbref_to_tm_mapping.csv",
                                    show_col_types = FALSE)

  return(players_mapped)
}
