## Release summary

This is a re-submission.

**Improvements**

* `load_understat_league_shots()` Functions to load pre stored shooting locations for all matches played since the 2014/15 season for the five leagues available on the Understat site.
* `load_match_comp_results()` Functions to load pre stored match results from domestic and international cups/competitions

**Bugs**

* `fotmob_get_season_stats()` no longer errors out when stats are queried for a league in its offseason [#136](https://github.com/JaseZiv/worldfootballR/issues/136)
* `player_transfer_history()` no longer throwing errors for players without a club [#137](https://github.com/JaseZiv/worldfootballR/issues/137)
* Increase sleep times in tests for FBref functions


## Test environments
* local R installation, R 4.2.0
* ubuntu 20.04.4 (on GitHub Actions), R 4.2.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
