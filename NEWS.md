# worldfootballR 0.5.2.2000

### Bugs

* `player_transfer_history()` updated after html changes on Transfermarkt caused function to return zero row data frame [#120](https://github.com/JaseZiv/worldfootballR/issues/120)

***

# worldfootballR 0.5.2.1000

### Bugs

* `tm_player_bio()` addresses where some data points don't exist for some players and returns NAs
* `understat_team_stats_breakdown()` now resturns the correct `season_start_year` value [#119](https://github.com/JaseZiv/worldfootballR/issues/119)
* tests and vignettes for fotmob stat functions changed to lowercase for second word in `stat_name` [#118](https://github.com/JaseZiv/worldfootballR/issues/118)

***

# worldfootballR 0.5.2

### Improvements

* All FBref functions now contain a user defined pause (`time_pause`) before each page load to abide by their new rate limiting rules. See [here](https://www.sports-reference.com/bot-traffic.html). Default is set to `2` seconds
* Internal function`.get_each_season_results()` exported now

***

# worldfootballR 0.5.1.6000

### Bugs

* `fotmob_get_seasons_stats` failed for non-domestic leagues, even when setting `cached=TRUE`. Fix was to add logic such that the latest season for a given league is found from the "See More" links on a generic stats page.

### Improvements / Breaking Changes

* Additional processing logic added such that `fotmob_get_league_tables` works properly for international tournaments.
* The `stat_type` argument in `fotmob_get_seasons_stats` renamed to `stat_name` to reflect the different set of values that it takes. Valid values are now equivalent to the options that can be found on the stats page in the browser, e.g. [the Liverpool player stats page](https://www.fotmob.com/leagues/47/stats/season/16390/players/goals/team/8650/liverpool-players). Previously the values came from [a custom, pre-saved dataframe](https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/fotmob-stats/stat_types.csv).

***

# worldfootballR 0.5.1.5000

### Bugs

* `fotmob_get_match_players` failed for non-domestic leagues because the `team` element does id not exist under the `table` element. Fix is to have more robust element for assigning team ids for players.

***

# worldfootballR 0.5.1.4000


### Improvements / Breaking Changes

* `tm_player_bio()` now also returns the player's maximum valuation (`max_player_valuation`) and the date that max valuation was recorded (`max_player_valuation_date`). **Note:** there will now be an additional two columns to the output


### Bugs

* `tm_player_bio` returning player number instead of name and also NAs for valuation [#109](https://github.com/JaseZiv/worldfootballR/issues/109)


***

# worldfootballR 0.5.1.3000

### Improvements

* `fotmob_get_season_stats` gains `stat_league_name` and can now be used for all leagues since seasons are programmatically scraped

***

# worldfootballR 0.5.1.2000

### Improvements

* `fotmob_get_league_matches` and `fotmob_get_league_tables` now check 2 places for the league endpoint since it changes occassionally.
* `fotmob_get_match_players` gets 3 additional columns: `match_id`, `team_id`, `team_name`. [#105](https://github.com/JaseZiv/worldfootballR/issues/105)

***

# worldfootballR 0.5.1.1000

* Set minimum R version to 4.0.0

***

# worldfootballR 0.5.1

* Modifications based on CRAN feedback to updates for CRAN submission

# worldfootballR 0.5.0

### New functions

* `fb_team_match_log_stats()` - to get match logs of selected stat types for a team(s) for all matches played in a season
* `fotmob_get_league_ids()` - to get Fotmob's league ids, which can be specified as an alternative to `country` and `league_name` in `fotmob_get_league_matches()` and `fotmob_get_league_tables()`
* `fotmob_get_season_stats()` - to get stats for one or more leagues, at team or player level
* `fotmob_get_league_matches()` - to select matches for one or more leagues
* `fotmob_get_league_tables()` - to get standings for one or more leagues
* `fotmob_get_matches_by_date()` - to select matches occurring on specific day(s)
* `fotmob_get_match_details()` - to get shooting locations for an individual match
* `fotmob_get_match_players()` - extract player statistics from matches

### Improvements

* `.clean_table_names()` (internal function) now able to clean tab names in `fb_team_match_log_stats()`
* * Added documentation for `fotmob_get_match_players` and `fotmob_get_match_players()`


### Bugs

* `fotmob_get_league_matches` and `fotmob_get_league_tables` updated to address new Fotmob league endpoint.
* `fotmob_get_match_players()` no longer returning the identical home and away team IDs [#93](https://github.com/JaseZiv/worldfootballR/issues/93)
* * Fixed multiple rows returned for single player in `stats` column returned in `fotmob_get_match_players()`
* `fotmob_get_league_matches` and `fotmob_get_league_tables` updated to address new Fotmob league endpoint.


***

# worldfootballR 0.4.10

### Improvements

* `tm_squad_stats()` now returns team, league information and also player URLs

### Bugs

* `fb_player_scouting_report()` was returning incorrect position comparisons in some cases [#85](https://github.com/JaseZiv/worldfootballR/issues/85)
* `get_player_market_values()` was returning no team URLs for when there was no erroneous URLs in the initial scraped list, and some columns not returning values due to HTML changes on transfermarkt
* `tm_league_team_urls()` was returning no team URLs for when there was no erroneous URLs in the initial scraped list
* `get_match_results()` was returning additional (incorrect) transfermarkt player URLs [#82](https://github.com/JaseZiv/worldfootballR/issues/80)

***

# worldfootballR 0.4.9

### New functions

* `tm_league_debutants()` to be able to extract league debutants
* `tm_expiring_contracts()` to be able to extract expiring contracts for a selected league
* `tm_league_injuries()` to get all current injuries from a selected league
* `tm_player_injury_history()` to get the full player injury history on transfermarkt for a selected player(s)


### Improvements

* `fb_player_scouting_report()` had an issue with player listed as three positions [#80](https://github.com/JaseZiv/worldfootballR/issues/80)
* `get_match_lineups()` now also returns match and player URLs [#78](https://github.com/JaseZiv/worldfootballR/issues/78)
* `get_match_results()` now includes match URLs [#78](https://github.com/JaseZiv/worldfootballR/issues/78)
* `get_match_report()` now includes yellow and red card counts [#2](https://github.com/JaseZiv/worldfootballR/issues/2)

### Bugs

* `tm_team_player_urls()` was returning additional (incorrect) transfermarkt player URLs

***

# worldfootballR 0.4.7

### New functions

* `tm_team_staff_urls()` allows users to extract URLs of selected team staff members based on staff role (ie 'Manager', 'Assistant Manager', etc)
* `tm_team_staff_history()` allows users to get all people who have held the selected role in a team's history and some summary statistics
* `tm_staff_job_history()` allows users to get all roles a selected staff member(s) has held and performance data in that role (wins, draws, losses, etc)

***

# worldfootballR 0.4.6.1

### Bugs

* Fixed for rare case of duplicate socials in `tm_player_bio()` coercing other values to lists

***

# worldfootballR 0.4.6

### New functions

* `fb_team_player_stats()` allows users to get all stats for a team(s) player season stats

### Improvements

* `get_season_team_stats()` now returns an additional column for MLS called `Conference` for when `stat_type = "league_table"` and `stat_type = "league_table_home_away"`

# worldfootballR 0.4.5 (CRAN)

### Improvements

* CRAN Submission Feedback:
  * Remove errant comment in examples for `fb_player_scouting_report()`

# worldfootballR 0.4.4

### Improvements

* CRAN Submission Feedback:
  * Wrap Data sources in single quotes in DESCRIPTION file (title and/or description)
  * Fixed `Warning: Unexecutable code in man/fb_player_scouting_report.Rd: pos_versus = "primary")`
  * Wrote `.pkg_message()` to allow users to easily suppress messages outputted to the console by setting `options(mypackage.verbose = FALSE)`
  * Removed unnecessary examples
  
* Results of using `understat_` functions now return the correct full season details (ie for `season_start_year = 2019`, the resulting `season` column in the df will be '2019/2020')

# worldfootballR 0.4.3

### Improvements

* `understat_team_players_stats()` gets season-long player stats for selected teams
* `understat_team_stats_breakdown()` gets team season shooting data broken down into game states
* `understat_team_meta()` created to allow for the extraction of team season URLs

***

# worldfootballR 0.4.2

### Bugs

* Fixed various transfermarkt functions that were broken due to HTML changes on the site

***

# worldfootballR 0.4.1

### Breaking Changes

* Remove FotMob functions

***

# worldfootballR 0.4.0

### Improvements

* `fotmob_get_matches_by_date()` and `fotmob_get_match_details()` added  [#61](https://github.com/JaseZiv/worldfootballR/issues/61)

***

# worldfootballR 0.3.6

### Improvements / Breaking Changes

* `fb_player_scouting_report()` now contains an additional column in the output (`scouting_period`) that allows the user to filter on the period they need the scouting report for

***

# worldfootballR 0.3.5.3

### Bugs

* Fixed `tm_player_bio()` throwing errors due to another change in html on Transfermarkt

***

# worldfootballR 0.3.5.2

### Bugs

* Fixed `tm_player_bio()` throwing errors due to change in html on Transfermarkt [#57](https://github.com/JaseZiv/worldfootballR/issues/57)

***

# worldfootballR 0.3.5.1

### Bugs

* Fixed encoding issue on Windows OS for player names in `player_dictionary_mapping()` [#56](https://github.com/JaseZiv/worldfootballR/issues/56)
* Fixed issue with `get_match_summary()` and `get_match_lineups()` not returning results for games that were canceled/postponed [#55](https://github.com/JaseZiv/worldfootballR/issues/55)

***


# worldfootballR 0.3.5

### Improvements

* `tm_team_transfers()` now includes which transfer window the transfer occurred in [#53](https://github.com/JaseZiv/worldfootballR/issues/53)


***

# worldfootballR 0.3.4

### Improvements

* `tm_team_transfers()` now also includes player URLs from Transfermarkt to allow for joining with player market values [#51](https://github.com/JaseZiv/worldfootballR/issues/51)

### Bugs

* `fb_player_season_stats()` was not returning results for some players even though stats were available on FBref [#52](https://github.com/JaseZiv/worldfootballR/issues/52)
* `tm_player_bio()` now returns player market valuations [#50](https://github.com/JaseZiv/worldfootballR/issues/50)

***

# worldfootballR 0.3.3.1

### Bugs

* `get_player_market_values()` now returning the correct current (`current_club`) and previous (`previous_club`) clubs for players

***

# worldfootballR 0.3.3

### New functions

* `player_dictionary_mapping()` returns a data frame of players (names and FBref URLs) who have played in the top 5 Euro leagues and their respective Transfermarkt URL

### Improvements

* `fb_big5_advanced_season_stats()` now returns player URLs from FBref

***

# worldfootballR 0.3.2.2

### Bugs

* `fb_player_season_stats()` handles with print statement when stat types are not present for players [#33](https://github.com/JaseZiv/worldfootballR/issues/33)

***

# worldfootballR 0.3.2.1

### Improvements

Transfermarkt functions `tm_team_transfers()` and `player_transfer_history()` returning transfer prices now differentiate between unknown (returning `NA`) to *free transfers* (returning `0`) [#45](https://github.com/JaseZiv/worldfootballR/issues/45)

### Bugs

* `get_match_summary()` no longer throws errors where only one team (home or away) didn't have a recorded event [#46](https://github.com/JaseZiv/worldfootballR/issues/46)

***

# worldfootballR 0.3.2

### Breaking Changes

* The `Team` in the data frame column returned by `get_match_shooting()` no longer exists, with `Squad` being elevated in to it's place instead.

### Bugs

* `fb_player_scouting_report()` now returns results for Goalkeepers also [#42](https://github.com/JaseZiv/worldfootballR/issues/42)
* `get_match_shooting()` now handles for when only one team (home or away) record any shots [#43](https://github.com/JaseZiv/worldfootballR/issues/43)

***

# worldfootballR 0.3.1.3

### Bugs

* `get_match_shooting()` handles for extra time periods

***

# worldfootballR 0.3.1.2

### Bugs

* `tm_team_transfers()` handles for missing values for some teams/leagues

***

# worldfootballR 0.3.1.1

### Bugs

* `get_player_market_values()` returning errors for seasons prior to current season [#41](https://github.com/JaseZiv/worldfootballR/issues/41)

***

# worldfootballR 0.3.1

### Improvements

The following functions now return results for international matches fro FBref:

* `get_match_urls()`
* `get_match_results()`
* `get_match_report()`
* `get_match_summary()`
* `get_match_lineups()`
* `get_match_shooting()`
* `get_advanced_match_stats()`

The following transfermarkt function has been improved:

* `tm_team_transfers()` now includes a column for `season`

***

# worldfootballR 0.3.0.4

### Bugs

* `get_match_shooting()` returning duplicated home shots data and not including away shots
* `get_match_summary()` not returning all Event_Players where special characters used

***

# worldfootballR 0.3.0.3

### Bugs

* `tm_player_bio()` returning error when player valuations not available on transfermarkt.com

***

# worldfootballR 0.3.0.1

### Bugs

* `player_transfer_history()` returns results even when player hasn't got a club to go to [#37](https://github.com/JaseZiv/worldfootballR/issues/37)

***

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
* `player_transfer_history()` now includes remaining contract expiry data and remaining contract length (in days) [#34](https://github.com/JaseZiv/worldfootballR/issues/34)
* `get_match_lineups()` now identifies diamond formations
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
