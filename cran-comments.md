## Release summary

This is a minor release that adds some new functions, improves on existing functions and addresses a minor bug in one function:

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


## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
