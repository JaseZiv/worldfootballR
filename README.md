
<!-- README.md is generated from README.Rmd. Please edit that file -->
worldfootballR
==============

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/JaseZiv/worldfootballR.svg?branch=main)](https://travis-ci.org/JaseZiv/worldfootballR) <!-- badges: end -->

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

``` r
library(worldfootballR)
library(tidyverse)
```

Usage
-----

The functions available in this package are designed to enable the extraction of world football data.

There are three main categories of data extract functions in this package:

-   Match-level statistics (team and player)
-   Season-level statistics (team and player)
-   League / Team metadata

### Match-level statistics

#### Get match results

To get the match results (and additional metadata) for all games for a tier-1 league season, the following function can be used:

``` r
# function to extract chess.com game data
serieA_2020 <- get_match_results(country = "ITA", gender = "M", season_end_year = 2020)
#> [1] "Scraping match results"
#> [1] "Match results finished scraping"
glimpse(serieA_2020)
#> Rows: 380
#> Columns: 20
#> $ Competition_Name <chr> "Serie A", "Serie A", "Serie A", "Serie A", "Serie A…
#> $ Gender           <chr> "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M…
#> $ Country          <chr> "ITA", "ITA", "ITA", "ITA", "ITA", "ITA", "ITA", "IT…
#> $ Season_End_Year  <int> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020…
#> $ Season_Name      <chr> "2019-2020 Serie A", "2019-2020 Serie A", "2019-2020…
#> $ Round            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ Wk               <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2…
#> $ Day              <chr> "Sat", "Sat", "Sun", "Sun", "Sun", "Sun", "Sun", "Su…
#> $ Date             <date> 2019-08-24, 2019-08-24, 2019-08-25, 2019-08-25, 201…
#> $ Time             <chr> "18:00", "20:45", "18:00", "20:45", "20:45", "20:45"…
#> $ Home             <chr> "Parma", "Fiorentina", "Udinese", "Torino", "SPAL", …
#> $ HomeGoals        <dbl> 0, 3, 1, 2, 2, 3, 1, 0, 0, 4, 1, 1, 4, 1, 2, 1, 0, 1…
#> $ Home_xG          <dbl> 0.4, 1.7, 1.0, 1.2, 1.6, 1.9, 0.2, 1.0, 0.8, 1.7, 2.…
#> $ Away             <chr> "Juventus", "Napoli", "Milan", "Sassuolo", "Atalanta…
#> $ AwayGoals        <dbl> 1, 4, 0, 1, 3, 3, 1, 1, 3, 0, 0, 0, 3, 1, 3, 2, 1, 3…
#> $ Away_xG          <dbl> 1.3, 2.0, 0.5, 1.5, 1.7, 1.3, 1.6, 1.5, 2.3, 0.7, 0.…
#> $ Attendance       <dbl> 20073, 33614, 24584, 16536, 11706, 38779, 16324, 160…
#> $ Venue            <chr> "Stadio Ennio Tardini", "Stadio Artemio Franchi", "D…
#> $ Referee          <chr> "Fabio Maresca", "Davide Massa", "Fabrizio Pasqua", …
#> $ Notes            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
```

**More than one league season**

The `get_match_results` function can be used to get data for multiple seasons/leages/genders/etc also:

``` r
big_5_2020_results <- get_match_results(country = c("ENG", "ESP", "ITA", "GER", "FRA"),
                                        gender = "M", season_end_year = 2020)
#> [1] "Scraping match results"
#> [1] "Match results finished scraping"
```

#### Get match report

This function will return similar results to that of `get_match_results()`, however `get_match_report()` will provide some additional information. It will also only provide it for a single match, not the whole season:

``` r
# function to extract chess.com game data
liv_mci_2020 <- get_match_report(match_url = "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League")
#> Scraping Liverpool vs. Manchester City Match Report – Sunday November 10, 2019
glimpse(liv_mci_2020)
#> Rows: 1
#> Columns: 14
#> $ League         <chr> "Premier League"
#> $ Match_Date     <chr> "Sunday November 10, 2019"
#> $ Matchweek      <chr> "Premier League (Matchweek 12)"
#> $ Home_Team      <chr> "Liverpool"
#> $ Home_Formation <chr> "4-3-3"
#> $ Home_Score     <dbl> 3
#> $ Home_xG        <dbl> 1
#> $ Home_Goals     <chr> "\n\t\t\n\t\t\tFabinho · 6&rsquor; \n\t\t\n\t\t\tMoham…
#> $ Away_Team      <chr> "Manchester City"
#> $ Away_Formation <chr> "4-2-3-1"
#> $ Away_Score     <dbl> 1
#> $ Away_xG        <dbl> 1.3
#> $ Away_Goals     <chr> "\n\t\t\n\t\t\t Bernardo Silva · 78&rsquor;\n\t\t\n\t"
#> $ Game_URL       <chr> "https://fbref.com/en/matches/47880eb7/Liverpool-Manch…
```

#### Get match lineups

This function will return a dataframe of all players listed for that match, including whether they started on the pitch, or on the bench.

``` r
# function to extract chess.com game data
liv_mci_2020_lineups <- get_match_lineups(match_url = "https://fbref.com/en/matches/47880eb7/Liverpool-Manchester-City-November-10-2019-Premier-League")
#> Scraping lineups for Liverpool vs. Manchester City Match Report – Sunday November 10, 2019
glimpse(liv_mci_2020_lineups)
#> Rows: 36
#> Columns: 6
#> $ Matchday    <chr> "Liverpool vs. Manchester City Match Report – Sunday Nove…
#> $ Team        <chr> "Liverpool", "Liverpool", "Liverpool", "Liverpool", "Live…
#> $ Formation   <chr> "4-3-3", "4-3-3", "4-3-3", "4-3-3", "4-3-3", "4-3-3", "4-…
#> $ Player_Num  <chr> "1", "3", "4", "5", "6", "9", "10", "11", "14", "26", "66…
#> $ Player_Name <chr> "Alisson", "Fabinho", "Virgil van Dijk", "Georginio Wijna…
#> $ Starting    <chr> "Pitch", "Pitch", "Pitch", "Pitch", "Pitch", "Pitch", "Pi…
```

------------------------------------------------------------------------

### Season-level statistics

#### Get Season Team Stats

The `get_season_team_stats` function allows the user to return a data frame of different stat types for all teams in tier-1 league seasons.

Note, some stats may not be available for all leagues. The big five European leagues should have all of these stats.

The following stat types can be selected:

-   *league\_table*
-   *league\_table\_home\_away*
-   *standard*
-   *keeper*
-   *keeper\_adv*
-   *shooting*
-   *passing*
-   *goal\_shot\_creation*
-   *defense*
-   *possession*
-   *playing\_time*
-   *misc*

``` r
# function to extract season teams stats
prem_2020_shooting <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = "2020", stat_type = "shooting")
#> Scraping season shooting stats
glimpse(prem_2020_shooting)
#> Rows: 40
#> Columns: 25
#> $ Competition_Name         <chr> "Premier League", "Premier League", "Premier…
#> $ Gender                   <chr> "M", "M", "M", "M", "M", "M", "M", "M", "M",…
#> $ Country                  <chr> "ENG", "ENG", "ENG", "ENG", "ENG", "ENG", "E…
#> $ Season_End_Year          <int> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 20…
#> $ Squad                    <chr> "Arsenal", "Aston Villa", "Bournemouth", "Br…
#> $ Team_or_Opponent         <chr> "team", "team", "team", "team", "team", "tea…
#> $ Num_Players              <dbl> 29, 28, 27, 25, 22, 27, 25, 24, 24, 24, 24, …
#> $ `90s`                    <dbl> 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, …
#> $ Gls_Standard             <dbl> 56, 40, 38, 35, 41, 69, 29, 42, 65, 83, 100,…
#> $ Sh_Standard              <dbl> 401, 453, 384, 456, 384, 619, 372, 465, 533,…
#> $ SoT_Standard             <dbl> 144, 146, 116, 137, 124, 210, 116, 155, 181,…
#> $ SoT_percent_Standard     <dbl> 35.9, 32.2, 30.2, 30.0, 32.3, 33.9, 31.2, 33…
#> $ Sh_per_90_Standard       <dbl> 10.55, 11.92, 10.11, 12.00, 10.11, 16.29, 9.…
#> $ SoT_per_90_Standard      <dbl> 3.79, 3.84, 3.05, 3.61, 3.26, 5.53, 3.05, 4.…
#> $ G_per_Sh_Standard        <dbl> 0.13, 0.09, 0.09, 0.07, 0.10, 0.10, 0.07, 0.…
#> $ G_per_SoT_Standard       <dbl> 0.37, 0.27, 0.29, 0.25, 0.31, 0.30, 0.22, 0.…
#> $ Dist_Standard            <dbl> 15.8, 16.9, 16.3, 17.1, 15.6, 16.1, 16.4, 15…
#> $ FK_Standard              <dbl> 19, 17, 21, 11, 17, 27, 15, 20, 20, 18, 27, …
#> $ PK_Standard              <dbl> 3, 1, 4, 1, 3, 7, 3, 1, 5, 5, 6, 10, 0, 2, 1…
#> $ PKatt_Standard           <dbl> 3, 3, 4, 2, 3, 7, 3, 1, 7, 5, 11, 14, 1, 2, …
#> $ xG_Expected              <dbl> 49.2, 40.1, 42.7, 41.2, 43.9, 66.6, 34.0, 49…
#> $ npxG_Expected            <dbl> 46.9, 37.7, 39.7, 39.7, 41.6, 61.7, 31.9, 48…
#> $ npxG_per_Sh_Expected     <dbl> 0.12, 0.08, 0.11, 0.09, 0.11, 0.10, 0.09, 0.…
#> $ G_minus_xG_Expected      <dbl> 6.8, -0.1, -4.7, -6.2, -2.9, 2.4, -5.0, -7.3…
#> $ `np:G_minus_xG_Expected` <dbl> 6.1, 1.3, -5.7, -5.7, -3.6, 0.3, -5.9, -7.5,…
```

**More than one league season**

The `get_season_team_stats` function can be used to get data for multiple seasons/leages/genders/etc.

Important to note, this function can only be used for one `stat-type` at a time, however all other parameters can have multiple values:

``` r
big_5_2020_possessions <- get_season_team_stats(country = c("ENG", "ESP", "ITA", "GER", "FRA"),
                                        gender = "M", season_end_year = 2020, stat_type = "possession")
#> Scraping season possession stats
```
