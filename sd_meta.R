library(rvest)
library(dplyr)
library(stringr)


sd_league_meta <- function() {
  main_url <- "https://www.soccerdonna.de"

  pg <- rvest::read_html("https://www.soccerdonna.de/en/2010/startseite/wettbewerbeDE.html")


  tab <- pg |> rvest::html_elements(".tabelle_grafik") |> _[1]

  df_out <- data.frame(
    league_url = as.character(paste0(main_url, tab |> rvest::html_elements(".al a") |> rvest::html_attr("href"))),
    league_name = as.character(tab |> rvest::html_elements(".al a") |> rvest::html_text()),
    league_level = as.character(tab |> rvest::html_elements(".hell .al+ .ac") |> rvest::html_text()),
    num_teams = as.numeric(tab |> rvest::html_elements(".hell .ac:nth-child(4)") |> rvest::html_text() |> as.numeric()),
    num_players = as.numeric(tab |> rvest::html_elements(".hell .ac:nth-child(5)") |> rvest::html_text() |> as.numeric()),
    total_value = tab |> rvest::html_elements(".hell .ac:nth-child(6)") |> rvest::html_text(),
    avg_value = tab |> rvest::html_elements(".hell .ac:nth-child(7)") |> rvest::html_text()
  )

  df_out <- df_out |>
    dplyr::mutate(total_value = mapply(.convert_soccerdonna_value_to_numeric, total_value),
                  avg_value = mapply(.convert_soccerdonna_value_to_numeric, avg_value))

  return(df_out)

}





league_pg <- read_html("https://www.soccerdonna.de/en/a-league-women/startseite/wettbewerb_WL.html")

league_pg |> html_elements("#centerbig div td:nth-child(2) a")
league_pg |> html_elements("#centerbig div td:nth-child(2) a") |> html_attr("title")





