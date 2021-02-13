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
* Added function to get the match results of a given tier-1 league season `get_match_results`
* Added function to extract match URLs for a given tier-1 league season `get_match_urls`
* Added function to get team season statistics `get_season_team_stats`
* Added function to get additional metadata for matches `get_match_report`
* Added function to get match lineups for selected match urls `get_match_lineups`
* Added function to get advanced match statistics for players and/or teams `get_advanced_match_statistics`
* Added function to get match events summary `get_match_summary`
