# worldfootballR (development version)

### Bugs

### Breaking Changes

### Improvements

* `tm_get_suspensions()` and `tm_get_risk_of_suspensions()` added. (0.6.7.0000) ([#411](https://github.com/JaseZiv/worldfootballR/issues/411))
* `fb_player_info()` added. (0.6.7.0001) ([#47](https://github.com/JaseZiv/worldfootballR/issues/47))
* `tm_player_bio()` now includes three new columns. `picture_url` containing the URL of the player's picture from Transfermarkt, `squad_number` containing the current worn squad number of the player and `player_id` that that contains the player ID on Transfermarkt. (0.6.7.0002) ([#141](https://github.com/JaseZiv/worldfootballR/issues/141))

***

# worldfootballR 0.6.7

* `tm_expiring_contracts()` returns additional `date_of_birth` column. (0.6.6.0001) [#355](https://github.com/JaseZiv/worldfootballR/issues/397)

***

# worldfootballR 0.6.6

* `fb_league_stats()` not returning `opponent` table. (0.6.5.0001) [#355](https://github.com/JaseZiv/worldfootballR/issues/355)
* `tm_player_bio()` not returning values in the `player_valuation`, `max_player_valuation` and `max_player_valuation_date` fields. Unfortunately, `max_player_valuation` and `max_player_valuation_date` fields are no able to be scraped at this release (0.6.5.0002) [#357](https://github.com/JaseZiv/worldfootballR/issues/357)
* `fb_league_stats()` not returning `player` table when hidden on page load. (0.6.5.0003) [#351](https://github.com/JaseZiv/worldfootballR/issues/351)
* Fix parameter mis-sepcification in fbref vignette. (0.6.5.0005) [#385](https://github.com/JaseZiv/worldfootballR/issues/385)
* `fb_season_team_stats()` failing due to change in FBRef table name. (0.6.5.0007, 0.6.5.0008) [#395](https://github.com/JaseZiv/worldfootballR/issues/389)
* In addressing the issue with `tm_player_injury_history()` in [#375](https://github.com/JaseZiv/worldfootballR/issues/375), the previously names column `club` has been renamed `club_missed_games_for` to better represent that this column will contain the games the player missed games for, as previously this column could have been misunderstood to be who they were playing for when they were injured (0.6.5.0004)
* `understat_match_players` and `understat_match_stats` added. (0.6.5.0006) [#386](https://github.com/JaseZiv/worldfootballR/issues/386)
* `fb_league_stats()` unreliable for `team_or_player = "player"`. (0.6.6) [#395](https://github.com/JaseZiv/worldfootballR/issues/395)

# worldfootballR 0.6.5

### Bugs

* `fb_league_stats()` failing for `playing_time`. (0.6.4.0001) [#314](https://github.com/JaseZiv/worldfootballR/issues/314)
* `fb_advanced_match_stats()` throwing errors when there were no stat tables available for matches (0.6.4.0002) [#315](https://github.com/JaseZiv/worldfootballR/issues/315)
* `fb_player_match_logs()` failing for players who have played on multiple teams/leagues in the same season (0.6.4.0006) [#327](https://github.com/JaseZiv/worldfootballR/issues/327)
* `fb_league_stats(team_or_player = "player")` returning duplicate player hrefs (0.6.4.0008) [#331](https://github.com/JaseZiv/worldfootballR/issues/331)
* `fb_league_stats(team_or_player = "player")` returning wrong season's data for Australian league (0.6.4.0009) [#333](https://github.com/JaseZiv/worldfootballR/issues/333)
* `tm_player_market_values()` returing the player name and valuation on separate rows in the `player_name` column [#338] (https://github.com/JaseZiv/worldfootballR/issues/338) and also returning `NA`s for the `player_age` column (0.6.4.0010) [#336](https://github.com/JaseZiv/worldfootballR/issues/336)
* `fb_match_results()` returns `NA` goals due to inconsistent `iconv()` behavior on different systems (0.6.4.0011) [#326](https://github.com/JaseZiv/worldfootballR/issues/326)
* `tm_team_player_urls()` fixed after change to server-side loading (0.6.4.0013) [#342](https://github.com/JaseZiv/worldfootballR/issues/342)
* `fb_teams_urls()` fixed to remove lower division teams being returned as a result of playoff promotion games (0.6.4.0014) [#344](https://github.com/JaseZiv/worldfootballR/issues/344)
* `tm_player_bio()` column name and data structure change for player date of birth (0.6.4.0014)


### Improvements
* `fb_player_season_stats()` now includes the ability to get a player's national team stats [https://github.com/JaseZiv/worldfootballR/pull/310/files] (0.6.4.0002)
* `fb_league_stats()` now correctly scrapes non domestic competitions [https://github.com/JaseZiv/worldfootballR/pull/325] (0.6.4.0003)
* `fb_team_match_stats()` and `understat_available_teams()` (0.6.4.0004)
* `fb_match_shooting()`, `fb_advanced_match_stats()`, `fb_league_stats(team_or_player = "player")` gain `Player_Href` column (0.6.4.0005)
* `load_fb_advanced_stats()` and `load_fb_match_summary()` added (0.6.4.0007)
* `tm_get_player_absence()` now available to get a list of absences through suspension for players from transfermarkt


***

# worldfootballR 0.6.4

### Breaking changes

* All fotmob functions removed due to an update to fotmob [terms of service](https://www.fotmob.com/tos.txt) prohibiting "the use of automatic services (robots, spiders, indexing etc.) as well as other methods for systematic or regular use."


### Bugs

* `fotmob_get_match_details()` failing due to change in `teamColors` JSON element (0.6.3.0001) [#271](https://github.com/JaseZiv/worldfootballR/issues/271)
* `fotmob_get_league_ids()` failing due to addition of `localizedName` JSON element (0.6.3.0003) [#275](https://github.com/JaseZiv/worldfootballR/issues/275)
* `fotmob_get_match_details()` failing (again) due to change in `teamColors` JSON element (0.6.3.0003)
* `fotmob_get_match_players()` failing due to addition of nested JSON elements in `stat` element (0.6.3.0004) [#277](https://github.com/JaseZiv/worldfootballR/issues/277)
* `fb_season_team_stats()` failing to get the correct home/away league table on some unusual layout league pages (0.6.3.0006) [#282](https://github.com/JaseZiv/worldfootballR/issues/282)
* `fotmob_get_match_players()` failing due mismatch in `list` and `data.frame` types for internal `stats` column before unnesting (0.6.3.0007) [#291](https://github.com/JaseZiv/worldfootballR/issues/291)
* `fotmob_get_match_stats()` failing due to Fotmob additional nesting (0.6.3.0008) [#295](https://github.com/JaseZiv/worldfootballR/issues/295)
* `fotmob_get_match_players()` returning nested list elements for `stat` columns (0.6.3.0010) [#298](https://github.com/JaseZiv/worldfootballR/issues/298)
* `tm_player_injury_history()` returning empty value in the `club` value for when a player didn't miss any days through injury [#293](https://github.com/JaseZiv/worldfootballR/issues/293) (0.6.3.0011)
* `tm_team_player_urls()` returning wrong player URLs due to html changes (0.6.3.0012) [#305](https://github.com/JaseZiv/worldfootballR/issues/305)
* `tm_player_injury_history()` returning empty value in the `club` value for when a player didn't miss any days through injury [#293](https://github.com/JaseZiv/worldfootballR/issues/293) (0.6.3.0012)
* `fotmob_get_match_players()` returning repeated stats and failing for NULL case (0.6.3.0011) [#298](https://github.com/JaseZiv/worldfootballR/issues/298)

### Improvements

* `fotmob_get_match_momentum()` added (0.6.2.0002)
* `fotmob_get_league_tables()` returns form table (0.6.3.0005) [#279](https://github.com/JaseZiv/worldfootballR/issues/279)
* `fotmob_get_match_stats()`: suppress error messages for seasons without data (0.6.3.0009) [#297](https://github.com/JaseZiv/worldfootballR/issues/297)
* `tm_player_injury_history()` removes the player number from the `player_name` variable [#268](https://github.com/JaseZiv/worldfootballR/issues/268) (0.6.3.0012)

***

# worldfootballR 0.6.3

### Breaking Changes

* `tm_player_transfer_history()` now contains an additional column in the returned dataframe (`transfer_type`). This column differentiate between regular transfers, free transfers, loans and paid loans and transfers due to players returning from loan.
* `fb_player_scouting_report()` now accepts an additional argument called `league_comp_name` that allows the user to only return reports for specific leagues/comps. not passing anything to this results in all reports being returned

### Bugs

* `fotmob_get_match_players()` was failing for upcoming matches due to a missing `stats` column (0.6.2.1000) [#226](https://github.com/JaseZiv/worldfootballR/issues/226)
* `tm_team_transfer_balances()` failing because of empty team boxes being collected (0.6.2.1000) [#228](https://github.com/JaseZiv/worldfootballR/issues/228)
* `fotmob_get_league_matches()` was failing due to an extra `purrr::map_dfr` that is no longer needed (0.6.2.2000) [#229](https://github.com/JaseZiv/worldfootballR/issues/229)
* All understat functions were failing due to a new cookie requirement (0.6.2.5000) [#239](https://github.com/JaseZiv/worldfootballR/issues/239)
* `fb_player_scouting_report()` failing due to HTML changes on FBRef (0.6.2.7000) [#242](https://github.com/JaseZiv/worldfootballR/issues/242)
* `fb_league_stats(team_or_player = "player", stat_type = "standard", ...)` failing since `"standard"` should be translated to `"stats"` (0.6.2.9100) [#252](https://github.com/JaseZiv/worldfootballR/issues/252)
* `fotmob_get_match_details()` returned 0 rows instead of 1 when there are no shots available (0.6.2.9300)
* `fotmob_get_league_matches()` changed to use `matches.allMatches` JSON element (0.6.2.9400) [#258](https://github.com/JaseZiv/worldfootballR/issues/258)
* All fotmob functions updated to use `httr::GET()` with appropriate post-processing of JSON instead of `jsonlite::fromJSON()` with raw URLs (0.6.3.0000) [#262](https://github.com/JaseZiv/worldfootballR/issues/262)

### Improvements

* `check_league_name()` removes repeated code. It checks that the name of the leagues is correct (0.6.2.3000) [#232](https://github.com/JaseZiv/worldfootballR/pull/232)
* `LEAGUES` replaces **if** statement with list (0.6.2.3000) [#232](https://github.com/JaseZiv/worldfootballR/pull/232)
* `tm_player_transfer_history()` added more information, added possibility to skip extraction of info that requires additional page load time. (0.6.2.4000) [#235](https://github.com/JaseZiv/worldfootballR/pull/235)
* `fb_league_stats()` added. Gets season stats for all teams / players in a selected league from FBref in a single scrape. (0.6.2.6000) [#243](https://github.com/JaseZiv/worldfootballR/pull/243)
* `fb_league_stats()` uses `rvest::htm_table()` if `team_or_player = "table"` (faster and more reliable), and only uses chromote if `team_or_player = "player"`. (0.6.2.7100)
* Use `quiet = FALSE` in all `purrr::possibly()` calls internally. Improve messaging for unexpected outcomes in `fotmob_get_matches_by_date()` and `fotmob_get_match_info()`. (0.6.2.8000) [#244](https://github.com/JaseZiv/worldfootballR/pull/244)
* `load_fb_match_shooting()` added. (0.6.2.9000) [#249](https://github.com/JaseZiv/worldfootballR/pull/249)
* `fotmob_get_league_matches()` and `fotmob_get_league_tables` gain a `season` parameter. (0.6.2.9200) [#256](https://github.com/JaseZiv/worldfootballR/pull/256)
* `fotmob_get_season_stats()` internals improved (0.6.3.0000)

# worldfootballR 0.6.2

### Bugs

* `tm_player_market_values()` returning `NAs` in the `squad` column
* `tm_` functions were missing the `country` and `comp_name` columns due to a change in the html on transfermarkt
* `fb_match_shooting()` throwing errors for duplicate column names after switch from StatsBomb to Opta on FBRef
* `fb_team_goal_logs` and `fb_player_goal_logs` gave errors when columns were not present
* `fotmob_get_season_stats` returning lexical error [#201](https://github.com/JaseZiv/worldfootballR/issues/201)
* `fotmob_get_season_stats` internals changed to handle new data format
* `tm_staff_job_history()` code changed due to HTML changes on transfermarkt.com
* All FBRef functions calling internal function `.get_match_report_page()` were returning 'Major League Soccer' as the away team [#216](https://github.com/JaseZiv/worldfootballR/issues/216)
* `fb_match_shooting()` was returning the wrong home or away designation in games where team names were inconsistent on the page [#219](https://github.com/JaseZiv/worldfootballR/issues/219)
* `fb_match_summary()`, `fb_match_shooting()` not returning returning Away Team name for international matches
* `fb_match_lineups()` returning 'Major League Soccer' as the away team for international matches [#221](https://github.com/JaseZiv/worldfootballR/issues/221)


### Improvements

* `load_fb_big5_advanced_season_stats()` now loads FBRef data provided by Stats Perform (Opta) via Github releases
* `load_match_results()` now loads FBRef data provided by Stats Perform (Opta) via Github releases
* `load_match_comp_results()` now loads FBRef data provided by Stats Perform (Opta) via Github releases
* `load_understat_league_shots()` now loads data from GitHub releases


***

# worldfootballR 0.6.1

### New functions

* `fb_team_goal_logs` - returns the team's season goals for and goals against logs
* `fb_player_goal_logs` - returns the player's career goal and assist logs

### Improvements

* Fotmob functions now pass user agent to headers under the hood

### Bugs

* `tm_squad_stats()` throwing differing number of rows error [#187](https://github.com/JaseZiv/worldfootballR/issues/187)
* `.get_match_report_page()` internal function used in a number of `fb_` functions throwing errors for cup tie legs [#174](https://github.com/JaseZiv/worldfootballR/issues/174)
* `.clean_table_names()` change to internal function that will include '*_performance*' to some column names in `fb_team_match_log_stats()` to address duplicated column names for `stat_type = 'keeper'` [#184](https://github.com/JaseZiv/worldfootballR/issues/184)


### Breaking Changes

Some column names in output of `fb_team_match_log_stats()` will now include *_Performance* at the end of the name. See [#184](https://github.com/JaseZiv/worldfootballR/issues/184) for more details

***

# worldfootballR 0.6.0

### New functions (and deprecated old ones)

* `fb_advanced_match_stats()` replaces `get_advanced_match_stats`
* `fb_match_lineups()` replaces `get_match_lineups`
* `fb_match_results()` replaces `get_match_results`
* `fb_match_report()` replaces `get_match_report`
* `fb_match_shooting()` replaces `get_match_shooting`
* `fb_match_summary()` replaces `get_match_summary`
* `fb_match_urls()` replaces `get_match_urls`
* `fb_season_team_stats()` replaces `get_season_team_stats`
* `fb_team_match_results()` replaces `get_team_match_results`
* `tm_player_market_values()` replaces `get_player_market_values`
* `tm_player_transfer_history` replaces `player_transfer_history`
* Improve fotmob tests by checking for column names instead of just number of columns.
* `load_fotmob_matches_by_date()`, `load_fotmob_match_details()`: Functions to load pre stored match ids and match details.
* `fotmob_get_match_details()`, `fotmob_get_matches_by_date()`, and `fotmob_get_match_players()`: Nested list columns are unnested by default. The lone exception is `shotmap` in the output for `fotmob_get_match_players()`
* `fotmob_get_match_info()` added. ([#166](https://github.com/JaseZiv/worldfootballR/issues/166))
* `fb_squad_wages()` designed to get player wage estimates from FBref via Capology


### Bugs

* `load_fb_big5_advanced_season_stats()` returning zero row df when no season selected. Now rectified
* `fotmob_get_season_stats()`: Fixed bug in `.fotmob_get_single_season_stats()` where season ids with dashes, e.g. `"17709-Apertura"` for Liga MX 2022/2023 would error.
* `fb_big5_advanced_season_stats()` throwing error for team stats. Also updated to only have one page load, hopefully minimising the risk of being blocked for excessive page loads

Elements changed on FBref somewhere in the same time `v 0.5.11` was being released!

  * `fb_advanced_match_stats()`
  * `fb_match_summary()`
  * `fb_big5_advanced_season_stats()` throwing error as `Matches played` column now names `MP` [#158](https://github.com/JaseZiv/worldfootballR/issues/158)
* `fotmob_get_league_tables()`: Fix unnesting to accommodate additional nesting and identical names at different levels. This bug affected `fotmob_get_season_stats()`, which calls `fotmob_get_league_tables()` [#155](https://github.com/JaseZiv/worldfootballR/issues/155)
* `fotmob_get_match_players()` returns stats as characters [#150](https://github.com/JaseZiv/worldfootballR/issues/150)
* `fotmob_get_season_stats()`: Address logic for extracting season ids from season stats pages that was failing due to blank stats pages in the offseason for a league.
* `get_match_lineups()` wasn't returning the away team name [#147](https://github.com/JaseZiv/worldfootballR/issues/147)
* `understat_league_season_shots()` would error when passing in a new `season_start_year` value for seasons that haven't yet started but match fixtures are available on Understat [#148](https://github.com/JaseZiv/worldfootballR/issues/148)


### Improvements

* FBref functions now pass user agent to headers under the hood
* `fotmob_get_match_team_stats()` added.
The following functions now no longer return league/season metadata, including columns `League`, `Gender`, `Country`, `Season`:

* `fb_match_summary()`
* `fb_advanced_match_stats()`
* `fb_match_report()`

### New functions (and deprecated old ones)

* `fb_advanced_match_stats()` replaces `get_advanced_match_stats`
* `fb_match_lineups()` replaces `get_match_lineups`
* `fb_match_results()` replaces `get_match_results`
* `fb_match_report()` replaces `get_match_report`
* `fb_match_shooting()` replaces `get_match_shooting`
* `fb_match_summary()` replaces `get_match_summary`
* `fb_match_urls()` replaces `get_match_urls`
* `fb_season_team_stats()` replaces `get_season_team_stats`
* `fb_team_match_results()` replaces `get_team_match_results`
* `tm_player_market_values()` replaces `get_player_market_values`
* `tm_player_transfer_history` replaces `player_transfer_history`
* Improve fotmob tests by checking for column names instead of just number of columns.
* `load_fotmob_matches_by_date()`, `load_fotmob_match_details()`: Functions to load pre stored match ids and match details.
* `fotmob_get_match_details()`, `fotmob_get_matches_by_date()`, and `fotmob_get_match_players()`: Nested list columns are unnested by default. The lone exception is `shotmap` in the output for `fotmob_get_match_players()`

***

# worldfootballR 0.5.7

### Improvements

* `load_understat_league_shots()` Functions to load pre stored shooting locations for all matches played since the 2014/15 season for the five leagues available on the Understat site.
* `load_match_comp_results()` Functions to load pre stored match results from domestic and international cups/competitions


### Bugs

* `fotmob_get_season_stats()` no longer errors out when stats are queried for a league in its offseason [#136](https://github.com/JaseZiv/worldfootballR/issues/136)
* `player_transfer_history()` no longer throwing errors for players without a club [#137](https://github.com/JaseZiv/worldfootballR/issues/137)
* Increase sleep times in tests for FBref functions

***

# worldfootballR 0.5.6

### Improvements

To respect FBref's rate limiting rules, all functions have now been updated to have a default pause of three seconds between page loads, up from two seconds which was originally requested.

### Bugs

* `get_match_report()` and `get_advanced_match_stats()` - now reporting the match date and away team name again. Additionally, `Home_Goals` and `Away_Goals` text strings now clean strings (tabs and line breaks removed) [#128](https://github.com/JaseZiv/worldfootballR/issues/128)
* `fotmob_get_league_ids()` - now uses new endpoint for league ids (data in script element no longer has data). This function is internally used by `fotmob_get_league_matches()`, `fotmob_get_league_tables()`, and `fotmob_get_season_stats()`, which would have been broken if `cached=FALSE` was specified.
* `player_transfer_history()` no longer throwing errors for retired players and also addressed changed HTML on transfermarkt returning no data [#127](https://github.com/JaseZiv/worldfootballR/issues/127)


***

# worldfootballR 0.5.3

### Improvements

Now starting to have load functions:

* `load_match_results()` - function to load pre stored data similar to `get_match_results()`
* `load_fb_big5_advanced_season_stats()` - function to load pre stored data similar to `fb_big5_advanced_season_stats()`


### Bugs

* `tm_player_bio()` addresses where some data points don't exist for some players and returns NAs
* `understat_team_stats_breakdown()` now returns the correct `season_start_year` value [#119](https://github.com/JaseZiv/worldfootballR/issues/119)
* tests and vignettes for fotmob stat functions changed to lowercase for second word in `stat_name` [#118](https://github.com/JaseZiv/worldfootballR/issues/118)
* `player_transfer_history()` updated after html changes on Transfermarkt caused function to return zero row data frame [#120](https://github.com/JaseZiv/worldfootballR/issues/120)
* `fotmob_get_league_matches()` and `fotmob_get_league_tables()` after changes to names in JSON response (`fixtures` -> `matches`, `tableData` -> `table`) [#121](https://github.com/JaseZiv/worldfootballR/issues/121), [#122](https://github.com/JaseZiv/worldfootballR/issues/122)
* Various fotmob functions affected by addition of `api/` in URL
* New names to player stats outdated docs for `fotmob_get_season_stats()`


***

# worldfootballR 0.5.2

### Improvements

* `fotmob_get_league_matches` and `fotmob_get_league_tables` now check 2 places for the league endpoint since it changes occasionally.
* `fotmob_get_match_players` gets 3 additional columns: `match_id`, `team_id`, `team_name`. [#105](https://github.com/JaseZiv/worldfootballR/issues/105)
* `fotmob_get_season_stats` gains `stat_league_name` and can now be used for all leagues since seasons are programmatically scraped
* `tm_player_bio()` now also returns the player's maximum valuation (`max_player_valuation`) and the date that max valuation was recorded (`max_player_valuation_date`). **Note:** there will now be an additional two columns to the output
* Additional processing logic added such that `fotmob_get_league_tables` works properly for international tournaments.
* The `stat_type` argument in `fotmob_get_seasons_stats` renamed to `stat_name` to reflect the different set of values that it takes. Valid values are now equivalent to the options that can be found on the stats page in the browser, e.g. [the Liverpool player stats page](https://www.fotmob.com/leagues/47/stats/season/16390/players/goals/team/8650/liverpool-players).
* All FBref functions now contain a user defined pause (`time_pause`) before each page load to abide by their new rate limiting rules. See [here](https://www.sports-reference.com/bot-traffic.html). Default is set to `2` seconds
* Internal function`.get_each_season_results()` exported now


### Bugs

* `tm_player_bio` returning player number instead of name and also NAs for valuation [#109](https://github.com/JaseZiv/worldfootballR/issues/109)
* `fotmob_get_match_players` failed for non-domestic leagues because the `team` element does id not exist under the `table` element. Fix is to have more robust element for assigning team ids for players.
* `fotmob_get_seasons_stats` failed for non-domestic leagues, even when setting `cached=TRUE`. Fix was to add logic such that the latest season for a given league is found from the "See More" links on a generic stats page.

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

### Bugs

* Fixed for rare case of duplicate socials in `tm_player_bio()` coercing other values to lists

***

# worldfootballR 0.4.6

### New functions

* `fb_team_player_stats()` allows users to get all stats for a team(s) player season stats

### Improvements

* `get_season_team_stats()` now returns an additional column for MLS called `Conference` for when `stat_type = "league_table"` and `stat_type = "league_table_home_away"`

# worldfootballR 0.4.5

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


### Bugs

* Fixed encoding issue on Windows OS for player names in `player_dictionary_mapping()` [#56](https://github.com/JaseZiv/worldfootballR/issues/56)
* Fixed issue with `get_match_summary()` and `get_match_lineups()` not returning results for games that were canceled/postponed [#55](https://github.com/JaseZiv/worldfootballR/issues/55)
* Fixed `tm_player_bio()` throwing errors due to change in html on Transfermarkt [#57](https://github.com/JaseZiv/worldfootballR/issues/57)
* Fixed `tm_player_bio()` throwing errors due to another change in html on Transfermarkt

***

# worldfootballR 0.3.5

### Improvements

* `tm_team_transfers()` now includes which transfer window the transfer occurred in [#53](https://github.com/JaseZiv/worldfootballR/issues/53)


***

# worldfootballR 0.3.4

### Improvements

* `tm_team_transfers()` now also includes player URLs from Transfermarkt to allow for joining with player market values [#51](https://github.com/JaseZiv/worldfootballR/issues/51)

### Bugs

* `get_player_market_values()` now returning the correct current (`current_club`) and previous (`previous_club`) clubs for players
* `fb_player_season_stats()` was not returning results for some players even though stats were available on FBref [#52](https://github.com/JaseZiv/worldfootballR/issues/52)
* `tm_player_bio()` now returns player market valuations [#50](https://github.com/JaseZiv/worldfootballR/issues/50)


***

# worldfootballR 0.3.3

### New functions

* `player_dictionary_mapping()` returns a data frame of players (names and FBref URLs) who have played in the top 5 Euro leagues and their respective Transfermarkt URL

### Improvements

Transfermarkt functions `tm_team_transfers()` and `player_transfer_history()` returning transfer prices now differentiate between unknown (returning `NA`) to *free transfers* (returning `0`) [#45](https://github.com/JaseZiv/worldfootballR/issues/45)
* `fb_big5_advanced_season_stats()` now returns player URLs from FBref

### Bugs

* `get_match_summary()` no longer throws errors where only one team (home or away) didn't have a recorded event [#46](https://github.com/JaseZiv/worldfootballR/issues/46)
* `fb_player_season_stats()` handles with print statement when stat types are not present for players [#33](https://github.com/JaseZiv/worldfootballR/issues/33)


***

# worldfootballR 0.3.2

### Breaking Changes

* The `Team` in the data frame column returned by `get_match_shooting()` no longer exists, with `Squad` being elevated in to it's place instead.

### Bugs

* `get_player_market_values()` returning errors for seasons prior to current season [#41](https://github.com/JaseZiv/worldfootballR/issues/41)
* `tm_team_transfers()` handles for missing values for some teams/leagues
* `get_match_shooting()` handles for extra time periods
* `fb_player_scouting_report()` now returns results for Goalkeepers also [#42](https://github.com/JaseZiv/worldfootballR/issues/42)
* `get_match_shooting()` now handles for when only one team (home or away) record any shots [#43](https://github.com/JaseZiv/worldfootballR/issues/43)


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

### Bugs

* `player_transfer_history()` returns results even when player hasn't got a club to go to [#37](https://github.com/JaseZiv/worldfootballR/issues/37)
* `tm_player_bio()` returning error when player valuations not available on transfermarkt.com
* `get_match_shooting()` returning duplicated home shots data and not including away shots
* `get_match_summary()` not returning all Event_Players where special characters used

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

* Fix issue [#29](https://github.com/JaseZiv/worldfootballR/issues/29) where `get_match_results()` not returning data for seasons before 2014-15
* Fix issue [#30](https://github.com/JaseZiv/worldfootballR/issues/30) for functions reading in seasons data from [worldfootballR_data](https://github.com/JaseZiv/worldfootballR_data)
* Fix issue [#31](https://github.com/JaseZiv/worldfootballR/issues/31) for `get_advanced_match_stats()`, `get_match_report()` and `get_match_summary()` not returning results for lower than tier 1 leagues
* Fix issue [#32](https://github.com/JaseZiv/worldfootballR/issues/32) for `get_match_summary()` where event times causing issues in extra time of match half (ie 45+3 as opposed to 46th minute)
* Fix issue [#33](https://github.com/JaseZiv/worldfootballR/issues/33) for `fb_player_season_stats()` where players not having played a game were previously causing errors


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

### Bugs

* Fix duplicating league URLs in `fb_league_urls()` as a result of including "Big 5" competition issue [#19](https://github.com/JaseZiv/worldfootballR/issues/19)
* Fix duplicating league URLs in various season-level functions as a result of including "Big 5" competition issue [#20](https://github.com/JaseZiv/worldfootballR/issues/20)
* Fix duplicated results in some functions

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
