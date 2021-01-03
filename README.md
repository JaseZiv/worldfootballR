
<!-- README.md is generated from README.Rmd. Please edit that file -->

\#worldfootballR

## Overview

This package is designed to allow users to extract various world
football results and player statistics data from fbref.com

## Installation

You can install the `worldfootballR` package from github with:

``` r
# install.packages("devtools")
devtools::install_github("JaseZiv/worldfootballR", ref = "main")
#> Skipping install of 'worldfootballR' from a github remote, the SHA1 (35f31c73) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

``` r
library(worldfootballR)
```

## Usage

The functions available in this package are designed to enable the
extraction of world football data.

### Get match results

To get the match results (and additional metadata) for all games for a
tier-1 league season, the following function can be used:

``` r
# function to extract chess.com game data
serieA_2020 <- get_match_results(country = "ITA", gender = "M", season_end_year = "2020")
#> [1] "Scraping ITA fist division for the 2020 season (gender = M)"
head(serieA_2020, 10)
#>    Season Round Wk Day       Date  Time          Home HomeGoals Home_xG
#> 1    2020    NA  1 Sat 2019-08-24 18:00         Parma         0     0.4
#> 2    2020    NA  1 Sat 2019-08-24 20:45    Fiorentina         3     1.7
#> 3    2020    NA  1 Sun 2019-08-25 18:00       Udinese         1     1.0
#> 4    2020    NA  1 Sun 2019-08-25 20:45        Torino         2     1.2
#> 5    2020    NA  1 Sun 2019-08-25 20:45          SPAL         2     1.6
#> 6    2020    NA  1 Sun 2019-08-25 20:45          Roma         3     1.9
#> 7    2020    NA  1 Sun 2019-08-25 20:45      Cagliari         0     1.0
#> 8    2020    NA  1 Sun 2019-08-25 20:45 Hellas Verona         1     0.2
#> 9    2020    NA  1 Sun 2019-08-25 20:45     Sampdoria         0     0.8
#> 10   2020    NA  1 Mon 2019-08-26 20:45         Inter         4     1.7
#>        Away AwayGoals Away_xG Attendance                          Venue
#> 1  Juventus         1     1.3      20073           Stadio Ennio Tardini
#> 2    Napoli         4     2.0      33614         Stadio Artemio Franchi
#> 3     Milan         0     0.5      24584                    Dacia Arena
#> 4  Sassuolo         1     1.5      16536      Stadio Olimpico di Torino
#> 5  Atalanta         3     1.7      11706             Stadio Paolo Mazza
#> 6     Genoa         3     1.3      38779                Stadio Olimpico
#> 7   Brescia         1     1.5      16099                 Sardegna Arena
#> 8   Bologna         1     1.6      16324  Stadio Marc'Antonio Bentegodi
#> 9     Lazio         3     2.3      19500 Stadio Comunale Luigi Ferraris
#> 10    Lecce         0     0.7      64188         Stadio Giuseppe Meazza
#>                 Referee Notes
#> 1         Fabio Maresca    NA
#> 2          Davide Massa    NA
#> 3       Fabrizio Pasqua    NA
#> 4      Maurizio Mariani    NA
#> 5  Gianluca Manganiello    NA
#> 6   Giampaolo Calvarese    NA
#> 7    Eugenio Abbattista    NA
#> 8          Antonio Giua    NA
#> 9       Gianluca Rocchi    NA
#> 10    Federico La Penna    NA
```

### Get Season Team Stats

The `get_season_team_stats` function allows the user to return a data
frame of different stat types for all teams in a tier-1 league season.

Note, some stats may not be available for all leagues. The big five
European leagues should have all of these stats.

The following stat types can be selected:

  - *league\_table*
  - *league\_table\_home\_away*
  - *standard*
  - *keeper*
  - *keeper\_adv*
  - *shooting*
  - *passing*
  - *goal\_shot\_creation*
  - *defense*
  - *possession*
  - *playing\_time*
  - *misc*

<!-- end list -->

``` r
# function to extract season teams stats
prem_2020_shooting <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = "2020", stat_type = "shooting")
prem_2020_shooting
#>                 Squad Team_or_Opponent Num_Players 90s Gls_Standard Sh_Standard
#> 1             Arsenal             team          29  38           56         401
#> 2         Aston Villa             team          28  38           40         453
#> 3         Bournemouth             team          27  38           38         384
#> 4            Brighton             team          25  38           35         456
#> 5             Burnley             team          22  38           41         384
#> 6             Chelsea             team          27  38           69         619
#> 7      Crystal Palace             team          25  38           29         372
#> 8             Everton             team          24  38           42         465
#> 9      Leicester City             team          24  38           65         533
#> 10          Liverpool             team          24  38           83         585
#> 11    Manchester City             team          24  38          100         730
#> 12     Manchester Utd             team          29  38           65         528
#> 13      Newcastle Utd             team          28  38           38         397
#> 14       Norwich City             team          30  38           25         409
#> 15      Sheffield Utd             team          26  38           36         353
#> 16        Southampton             team          25  38           51         497
#> 17          Tottenham             team          29  38           56         439
#> 18            Watford             team          26  38           35         410
#> 19           West Ham             team          29  38           49         414
#> 20             Wolves             team          21  38           49         453
#> 21         vs Arsenal         opponent          29  38           48         545
#> 22     vs Aston Villa         opponent          28  38           66         602
#> 23     vs Bournemouth         opponent          27  38           65         546
#> 24        vs Brighton         opponent          25  38           52         497
#> 25         vs Burnley         opponent          22  38           49         535
#> 26         vs Chelsea         opponent          27  38           51         316
#> 27  vs Crystal Palace         opponent          25  38           48         507
#> 28         vs Everton         opponent          24  38           53         427
#> 29  vs Leicester City         opponent          24  38           39         367
#> 30       vs Liverpool         opponent          24  38           32         340
#> 31 vs Manchester City         opponent          24  38           34         274
#> 32  vs Manchester Utd         opponent          29  38           35         385
#> 33   vs Newcastle Utd         opponent          28  38           56         561
#> 34    vs Norwich City         opponent          30  38           72         600
#> 35   vs Sheffield Utd         opponent          26  38           39         430
#> 36     vs Southampton         opponent          25  38           59         445
#> 37       vs Tottenham         opponent          29  38           45         531
#> 38         vs Watford         opponent          26  38           61         488
#> 39        vs West Ham         opponent          29  38           59         493
#> 40          vs Wolves         opponent          21  38           39         393
#>    SoT_Standard SoT_percent_Standard Sh/90_Standard SoT/90_Standard
#> 1           144                 35.9          10.55            3.79
#> 2           146                 32.2          11.92            3.84
#> 3           116                 30.2          10.11            3.05
#> 4           137                 30.0          12.00            3.61
#> 5           124                 32.3          10.11            3.26
#> 6           210                 33.9          16.29            5.53
#> 7           116                 31.2           9.79            3.05
#> 8           155                 33.3          12.24            4.08
#> 9           181                 34.0          14.03            4.76
#> 10          222                 37.9          15.39            5.84
#> 11          246                 33.7          19.21            6.47
#> 12          200                 37.9          13.89            5.26
#> 13          127                 32.0          10.45            3.34
#> 14          124                 30.3          10.76            3.26
#> 15          109                 30.9           9.29            2.87
#> 16          169                 34.0          13.08            4.45
#> 17          155                 35.3          11.55            4.08
#> 18          114                 27.8          10.79            3.00
#> 19          157                 37.9          10.89            4.13
#> 20          142                 31.3          11.92            3.74
#> 21          189                 34.7          14.34            4.97
#> 22          175                 29.1          15.84            4.61
#> 23          182                 33.3          14.37            4.79
#> 24          164                 33.0          13.08            4.32
#> 25          159                 29.7          14.08            4.18
#> 26          115                 36.4           8.32            3.03
#> 27          169                 33.3          13.34            4.45
#> 28          145                 34.0          11.24            3.82
#> 29          126                 34.3           9.66            3.32
#> 30          107                 31.5           8.95            2.82
#> 31          107                 39.1           7.21            2.82
#> 32          125                 32.5          10.13            3.29
#> 33          190                 33.9          14.76            5.00
#> 34          201                 33.5          15.79            5.29
#> 35          141                 32.8          11.32            3.71
#> 36          163                 36.6          11.71            4.29
#> 37          172                 32.4          13.97            4.53
#> 38          166                 34.0          12.84            4.37
#> 39          178                 36.1          12.97            4.68
#> 40          120                 30.5          10.34            3.16
#>    G/Sh_Standard G/SoT_Standard Dist_Standard FK_Standard PK_Standard
#> 1           0.13           0.37          15.8          19           3
#> 2           0.09           0.27          16.9          17           1
#> 3           0.09           0.29          16.3          21           4
#> 4           0.07           0.25          17.1          11           1
#> 5           0.10           0.31          15.6          17           3
#> 6           0.10           0.30          16.1          27           7
#> 7           0.07           0.22          16.4          15           3
#> 8           0.09           0.26          15.6          20           1
#> 9           0.11           0.33          16.5          20           5
#> 10          0.13           0.35          16.0          18           5
#> 11          0.13           0.38          15.6          27           6
#> 12          0.10           0.28          18.2          33          10
#> 13          0.10           0.30          18.4          14           0
#> 14          0.06           0.19          17.3          16           2
#> 15          0.10           0.32          14.7           4           1
#> 16          0.10           0.29          17.3          27           2
#> 17          0.12           0.34          17.7          21           3
#> 18          0.07           0.25          16.2          18           7
#> 19          0.11           0.29          15.5          13           4
#> 20          0.10           0.32          16.7          12           4
#> 21          0.08           0.22          17.3          17           7
#> 22          0.10           0.35          16.0          21           5
#> 23          0.11           0.33          17.1          15           5
#> 24          0.10           0.30          15.7          20           2
#> 25          0.08           0.28          17.4          21           4
#> 26          0.16           0.43          16.1          15           2
#> 27          0.09           0.28          16.7          23           0
#> 28          0.12           0.34          16.4          23           3
#> 29          0.08           0.25          16.7          21           8
#> 30          0.09           0.29          16.7           5           1
#> 31          0.11           0.29          16.1          18           3
#> 32          0.08           0.26          17.2          23           3
#> 33          0.10           0.28          15.8          19           2
#> 34          0.11           0.34          15.9          18           4
#> 35          0.09           0.27          16.9          23           1
#> 36          0.13           0.34          15.9          12           3
#> 37          0.08           0.23          16.7          23           5
#> 38          0.11           0.32          17.2          28           8
#> 39          0.11           0.30          15.6          13           5
#> 40          0.10           0.32          16.7          12           1
#>    PKatt_Standard xG_Expected npxG_Expected npxG/Sh_Expected G-xG_Expected
#> 1               3        49.2          46.9             0.12           6.8
#> 2               3        40.1          37.7             0.08          -0.1
#> 3               4        42.7          39.7             0.11          -4.7
#> 4               2        41.2          39.7             0.09          -6.2
#> 5               3        43.9          41.6             0.11          -2.9
#> 6               7        66.6          61.7             0.10           2.4
#> 7               3        34.0          31.9             0.09          -5.0
#> 8               1        49.3          48.5             0.11          -7.3
#> 9               7        61.6          56.2             0.11           3.4
#> 10              5        71.5          67.7             0.12          11.5
#> 11             11        93.0          84.6             0.12           7.0
#> 12             14        59.4          48.8             0.10           5.6
#> 13              1        33.1          32.3             0.08           4.9
#> 14              2        38.0          36.6             0.09         -13.0
#> 15              1        41.5          40.8             0.12          -5.5
#> 16              5        53.9          50.0             0.10          -2.9
#> 17              4        46.1          43.0             0.10           9.9
#> 18              8        45.1          39.1             0.10         -10.1
#> 19              4        46.2          43.2             0.11           2.8
#> 20              4        47.1          44.1             0.10           1.9
#> 21              8        56.6          50.4             0.10          -8.6
#> 22              6        65.9          61.3             0.11           0.1
#> 23              6        57.5          53.0             0.10           7.5
#> 24              2        54.5          52.9             0.11          -2.5
#> 25              5        48.3          44.5             0.08           0.7
#> 26              2        37.9          36.4             0.12          13.1
#> 27              1        51.3          50.6             0.10          -3.3
#> 28              4        48.4          45.4             0.11           4.6
#> 29             10        44.5          37.0             0.11          -5.5
#> 30              1        40.0          39.3             0.12          -8.0
#> 31              3        34.7          32.9             0.12          -0.7
#> 32              3        37.4          35.2             0.09          -2.4
#> 33              2        58.3          56.8             0.10          -2.3
#> 34              7        61.9          56.7             0.10          10.1
#> 35              3        47.9          45.6             0.11          -8.9
#> 36              4        53.1          50.0             0.12           5.9
#> 37              7        52.0          46.9             0.09          -7.0
#> 38              9        57.3          50.3             0.11           3.7
#> 39              6        61.1          56.5             0.12          -2.1
#> 40              3        34.8          32.4             0.08           4.2
#>    np:G-xG_Expected
#> 1               6.1
#> 2               1.3
#> 3              -5.7
#> 4              -5.7
#> 5              -3.6
#> 6               0.3
#> 7              -5.9
#> 8              -7.5
#> 9               3.8
#> 10             10.3
#> 11              9.4
#> 12              6.2
#> 13              5.7
#> 14            -13.6
#> 15             -5.8
#> 16             -1.0
#> 17             10.0
#> 18            -11.1
#> 19              1.8
#> 20              0.9
#> 21             -9.4
#> 22             -0.3
#> 23              7.0
#> 24             -2.9
#> 25              0.5
#> 26             12.6
#> 27             -2.6
#> 28              4.6
#> 29             -6.0
#> 30             -8.3
#> 31             -1.9
#> 32             -3.2
#> 33             -2.8
#> 34             11.3
#> 35             -7.6
#> 36              6.0
#> 37             -6.9
#> 38              2.7
#> 39             -2.5
#> 40              5.6
```
