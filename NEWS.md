# worldfootballR 0.2.4

### New functions
* Added fbref helper functions:
  * `fb_league_urls()` to get fbref league URLs for a given country, gender, season, tier
  * `fb_teams_urls()` to get fbref team URLs for a given league_url
  * `fb_player_urls()` to get fbref player URLs for a given team_url
* Added a function to retrieve a player's scouting report `fb_player_scouting_report()`
* Added a function to get season player stats for a selected player and stay_type `fb_player_season_stats()`

### Improving existing functions
* Additional leagues and cups now available in the following functions; `get_match_urls()`, `get_match_results()`, `get_season_team_stats()`

# worldfootballR 0.2.3

* Added `league_url` argument to `get_player_market_values()` to allow extraction of *non-standard* (leagues not stored in `worldfootballR_data`) leagues

# worldfootballR 0.2.2

* Added player nationalities and positions to output of `get_player_market_values()` data output

# worldfootballR 0.2.1

### General Changes
* Removed internal functions no longer necessary (data is stored at `JaseZiv/worldfootballR_data` instead)
* Usage instructions now in vignettes as opposed to README

### Bugs
* Fixed duplicate column name issue for keeper stats [#13](https://github.com/JaseZiv/worldfootballR/issues/13)

# worldfootballR 0.2.0
* Added `get_player_market_values` to get player valuations from transfermarkt.com

# worldfootballR 0.1.0
* Bug fixes #8
* Package Logo
* Tests written and codecov installed

# worldfootballR 0.0.1
* performance improvements, including limiting page reads where match report data included in output
* error handling

# worldfootballR 0.0.0.9000

* First commit of package
* Added function to get the match results of a given tier-1 league season `get_match_results()`
* Added function to extract match URLs for a given tier-1 league season `get_match_urls()`
* Added function to get team season statistics `get_season_team_stats()`
* Added function to get additional metadata for matches `get_match_report()`
* Added function to get match lineups for selected match urls `get_match_lineups()`
* Added function to get advanced match statistics for players and/or teams `get_advanced_match_statistics()`
* Added function to get match events summary `get_match_summary()`
