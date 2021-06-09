# worldfootballR 0.3.1


### Improvements

* `get_advanced_match_stats()` now returns advanced match stats for internationals


# worldfootballR 0.3.0.4

### Bugs

* `get_match_shooting()` returning duplicated home shots data and not including away shots
* `get_match_summary()` not returning all Event_Players where special characters used

# worldfootballR 0.3.0.3

### Bugs

* `tm_player_bio()` returning error when player valuations not available on transfermarkt.com


# worldfootballR 0.3.0.1

### Bugs

* `player_transfer_history()` returns results even when player hasn't got a club to go to [#37](https://github.com/JaseZiv/worldfootballR/issues/37)

# worldfootballR 0.3.0

### New functions

Functions to extract understat.com data now available:
* Shots locations data:
  * `understat_league_season_shots()` to get shot location data for all matches in a league season
  * `understat_team_season_shots()` to get shot locations for and against a select team
  * `understat_match_shots()` to get shot locations for a selected match
  * `understat_player_shots()` to get all available shot locations for games played by a selected player
  * `understat_league_match_results()` to get results and match data for all matches played in a season

New transfermarkt.com functions:
* `tm_player_bio()` to get player bios from transfermarkt
* `tm_team_transfers()` to get all arrival and departures for a team season
* `tm_squad_stats()` to get basic squad stats for teams for a season
* Helper functions:
  * `tm_league_team_urls()` to get teams URLs for a league season 
  * `tm_team_player_urls()` to get player URLs for a given team

### Improvements

* `get_player_market_values()` from transfermarkt now includes player heights, their preferred foot, when they joined the club and where from and when their contract expires
* `player_transfer_history()` now includes remaining contract expriry data and remaining contract length (in days) [#34](https://github.com/JaseZiv/worldfootballR/issues/34)
* `get_match_lineups()` now idntifies diamond formations
* **Progress bars now included on all long running functions!**

***

# worldfootballR 0.2.9

### Breaking Changes

* Most Fbref table names will have changed slightly, as they have been cleaned up to not include "__" and "+/-" has been replaced with "Plus_Minus"
* Column names `event_time`, `event_type`, `event_players` and `score_progression` changed for `get_match_summary()`, and additional columns added

### Improvements

* `fb_player_season_stats()` now able to accept multiple `player_url` values
* `get_match_summary()` now contains additional columns:
  * `Is_Pens` to indicate whether the event is from a penalty shootout
  * `Event_Half` to indicate which half the event occurred in (1, 2, 3, 4, 5), with 3 & 4 being extra time and 5 being penalty shootout
  * `Penalty_Number` for penalty shootouts, the penalty taking order 

### Bugs
* Fix issue [#32](https://github.com/JaseZiv/worldfootballR/issues/32) for `get_match_summary()` where event times causing issues in extra time of match half (ie 45+3 as opposed to 46th minute) 

* Fix issue [#33](https://github.com/JaseZiv/worldfootballR/issues/33) for `fb_player_season_stats()` where players not having played a game were previously causing errors 

*** 

# worldfootballR 0.2.8.3

### Bugs
* Fix issue [#31](https://github.com/JaseZiv/worldfootballR/issues/31) for `get_advanced_match_stats()`, `get_match_report()` and `get_match_summary()` not returning results for lower than tier 1 leagues

*** 

# worldfootballR 0.2.8.2

### Bugs
* Fix issue [#30](https://github.com/JaseZiv/worldfootballR/issues/30) for functions reading in seasons data from [worldfootballR_data](https://github.com/JaseZiv/worldfootballR_data)

*** 

# worldfootballR 0.2.8.1

### Bugs
* Fix issue [#29](https://github.com/JaseZiv/worldfootballR/issues/29) where `get_match_results()` not returning data for seasons before 2014-15

*** 

# worldfootballR 0.2.8

### New functions
* Added new fbref functions
  * `fb_player_match_logs()` to get player match logs for a season and stat type (issue [#26](https://github.com/JaseZiv/worldfootballR/issues/26))
  
### Improvements
* `fb_player_scouting_report()` now returns a players **full** scouting report against peers in Men's Big 5 Leagues and European Competition over the last 365 days (issue [27](https://github.com/JaseZiv/worldfootballR/issues/27))
* Removed the country abbreviation in the `Squad` column of the data frame returned using `fb_player_season_stats()`

***

# worldfootballR 0.2.7

### New functions
* Added new fbref functions
  * `get_match_shooting()` to get shot and shot creation specific details (issue [#3](https://github.com/JaseZiv/worldfootballR/issues/3))
  * `get_team_match_results()` to get match results for a given team or teams in a season
  
### Improvements
* `get_match_lineups()` now returns additional summary player performance data (shots, goals, cards, etc) to the original lineups output

### Bugs
* Fixed issue where `get_season_player_stats()` not returning results for players who have only played in domestic comps (not cups, internationals, etc) (issue [#22](https://github.com/JaseZiv/worldfootballR/issues/22))
* Fixed issue where `fb_big5_advanced_season_stats()` not returning results for 'playing_time' stat_type (issue [#23](https://github.com/JaseZiv/worldfootballR/issues/23))

*** 

# worldfootballR 0.2.6

### New functions
* Added new transfermarkt functions:
  * `tm_team_transfer_balances()` to get team total transfer income and expenditure data
  * `tm_matchday_table()` to get league table after each specified matchday(s)

### Improvements
* Various transfermarkt functions refactored to have consistent column names

***

# worldfootballR 0.2.5.3

### Bugs
Fix duplicated results in some functions

***

# worldfootballR 0.2.5.2

### Bugs
Fix duplicating league URLs in various season-level functions as a result of including "Big 5" competition issue [#20](https://github.com/JaseZiv/worldfootballR/issues/20)

***

# worldfootballR 0.2.5.1

### Bugs
Fix duplicating league URLs in `fb_league_urls()` as a result of including "Big 5" competition issue [#19](https://github.com/JaseZiv/worldfootballR/issues/19)

***

# worldfootballR 0.2.5

### New functions
* Added `fb_big5_advanced_season_stats()` to get fbref season stats for all players or teams stats in the big five Euro leagues

***

# worldfootballR 0.2.4

### New functions
* Added fbref helper functions:
  * `fb_league_urls()` to get fbref league URLs for a given country, gender, season, tier
  * `fb_teams_urls()` to get fbref team URLs for a given league_url
  * `fb_player_urls()` to get fbref player URLs for a given team_url
* Added a function to retrieve a player's scouting report `fb_player_scouting_report()`
* Added a function to get season player stats for a selected player and stay_type `fb_player_season_stats()`

### Improvements
* Additional leagues and cups now available in the following functions; `get_match_urls()`, `get_match_results()`, `get_season_team_stats()`

***

# worldfootballR 0.2.3

* Added `league_url` argument to `get_player_market_values()` to allow extraction of *non-standard* (leagues not stored in `worldfootballR_data`) leagues

***

# worldfootballR 0.2.2

* Added player nationalities and positions to output of `get_player_market_values()` data output

***

# worldfootballR 0.2.1

### General Changes
* Removed internal functions no longer necessary (data is stored at `JaseZiv/worldfootballR_data` instead)
* Usage instructions now in vignettes as opposed to README

### Bugs
* Fixed duplicate column name issue for keeper stats [#13](https://github.com/JaseZiv/worldfootballR/issues/13)

***

# worldfootballR 0.2.0
* Added `get_player_market_values` to get player valuations from transfermarkt.com

***

# worldfootballR 0.1.0
* Bug fixes #8
* Package Logo
* Tests written and codecov installed

***

# worldfootballR 0.0.1
* performance improvements, including limiting page reads where match report data included in output
* error handling

***

# worldfootballR 0.0.0.9000

* First commit of package
* Added function to get the match results of a given tier-1 league season `get_match_results()`
* Added function to extract match URLs for a given tier-1 league season `get_match_urls()`
* Added function to get team season statistics `get_season_team_stats()`
* Added function to get additional metadata for matches `get_match_report()`
* Added function to get match lineups for selected match urls `get_match_lineups()`
* Added function to get advanced match statistics for players and/or teams `get_advanced_match_statistics()`
* Added function to get match events summary `get_match_summary()`
