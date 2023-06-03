library(worldfootballR)
library(purrr)
packageVersion("worldfootballR")

try_f <- function(f, ...) {
  fs <- safely(f, quiet = FALSE)
  res <- fs(...)
  if (!is.null(res$error)) {
    print(res$error)
  }
  res$result
}

try_f(tm_matchday_table, country_name="England", start_year="2021", matchday=1)
try_f(tm_league_injuries, country_name = "",league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3")
try_f(tm_player_injury_history, player_urls = "https://www.transfermarkt.com/eden-hazard/profil/spieler/50202")
