## Release summary

This is a major release that has a fotmob integrated and also addresses some minor bugs and improvements:


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

### Bugs

* `fotmob_get_league_matches` and `fotmob_get_league_tables` updated to address new Fotmob league endpoint.
* `fotmob_get_match_players()` no longer returning the identical home and away team IDs [#93](https://github.com/JaseZiv/worldfootballR/issues/93)


## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
