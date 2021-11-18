## Release summary

This is a minor fix to address CRAN submission feedback.

### What changed?

* Wrap Data sources in single quotes in DESCRIPTION file (title and/or description)
* Fixed `Warning: Unexecutable code in man/fb_player_scouting_report.Rd: pos_versus = "primary")` by uncommenting the line above `pos_versus = "primary"`
* Removed print statements that users cannot suppress

## Test environments
* local R installation, R 4.1.1
* ubuntu 16.04 (on travis-ci), R 4.1.1
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
