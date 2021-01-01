
<!-- README.md is generated from README.Rmd. Please edit that file -->
worldfootballR
==============

Overview
--------

This package is designed to allow users to extract various world football results and player statistics data from fbref.com

Installation
------------

You can install the `worldfootballR` package from github with:

``` r
# install.packages("devtools")
devtools::install_github("JaseZiv/worldfootballR")
```

Usage
-----

The functions available in this package are designed to enable the extraction of world football data.

``` r
# function to extract chess.com game data
serieA_2020 <- get_match_results(country = "ITA", gender = "M", season_end_year = "2020")
```
