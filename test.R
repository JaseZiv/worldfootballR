library(worldfootballR)
packageVersion("worldfootballR")
league_one_injuries <- tm_league_injuries(country_name = "",league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3")
league_one_injuries
hazard_injuries <- tm_player_injury_history(player_urls = "https://www.transfermarkt.com/eden-hazard/profil/spieler/50202")
hazard_injuries
