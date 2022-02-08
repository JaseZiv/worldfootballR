## Release summary

This is a minor release that addresses some minor bugs and improvements:


### Improvements

* `tm_squad_stats()` now returns team, league information and also player URLs

### Bugs

* `get_match_results()` was returning additional (incorrect) transfermarkt player URLs [#82](https://github.com/JaseZiv/worldfootballR/issues/80)
* `get_player_market_values()` was returning no team URLs for when there was no erroneous URLs in the initial scraped list, and some columns not returning values due to HTML changes on transfermarkt
* `tm_league_team_urls()` was returning no team URLs for when there was no erroneous URLs in the initial scraped list
* `fb_player_scouting_report()` was returning incorrect position comparisons in some cases [#85](https://github.com/JaseZiv/worldfootballR/issues/85)


## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
