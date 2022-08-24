## Release summary

This is a re-submission.

**New Functions**

* `fotmob_get_match_info()` added. ([#166](https://github.com/JaseZiv/worldfootballR/issues/166))
* `fb_squad_wages()` designed to get player wage estimates from FBref via Capology

**Bugs**

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


**Improvements**

* FBref functions now pass user agent to headers under the hood
* `fotmob_get_match_team_stats()` added.
The following functions now no longer return league/season metadata, including columns `League`, `Gender`, `Country`, `Season`:

* `fb_match_summary()`
* `fb_advanced_match_stats()`
* `fb_match_report()`

**New functions (and deprecated old ones)**

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


## Test environments
* local R installation, R 4.2.0
* ubuntu 20.04.4 (on GitHub Actions), R 4.2.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note








