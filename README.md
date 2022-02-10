
<!-- README.md is generated from README.Rmd. Please edit that file -->

# worldfootballR <img src="man/figures/logo.png" align="right" width="181" height="201"/>

<!-- badges: start -->

[![R build
status](https://github.com/JaseZiv/worldfootballR/workflows/R-CMD-check/badge.svg)](https://github.com/JaseZiv/worldfootballR/actions)
[![codecov](https://codecov.io/gh/JaseZiv/worldfootballR/branch/main/graph/badge.svg?token=WGLU5J34VL)](https://app.codecov.io/gh/JaseZiv/worldfootballR)
<!-- badges: end -->

## Overview

This package is designed to allow users to extract various world
football results and player statistics from the following popular
football (soccer) data sites:

-   [FBref](https://fbref.com/en/)
-   [Transfermarkt](https://www.transfermarkt.com/)
-   [Understat](https://understat.com/)

## Installation

You can install the `worldfootballR` package from github with:

``` r
# install.packages("devtools")
devtools::install_github("JaseZiv/worldfootballR")
```

``` r
library(worldfootballR)
```

------------------------------------------------------------------------

## Usage

Package vignettes have been built to help you get started with the
package.

-   For functions to extract data from FBref, see
    [here](https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html)
-   For functions to extract data from Transfermarkt, see
    [here](https://jaseziv.github.io/worldfootballR/articles/extract-transfermarkt-data.html)
-   For functions to extract data from Understat, see
    [here](https://jaseziv.github.io/worldfootballR/articles/extract-understat-data.html)
-   For functions to extract data for international matches from FBref,
    see
    [here](https://jaseziv.github.io/worldfootballR/articles/fbref-data-internationals.html)

------------------------------------------------------------------------

## Leagues and Seasons

### Fbref

For FBref.com data (match and season data), a list of leagues and
seasons included in the package can be found in the
`worldfootballR_data` repository and can be found
[here](https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/all_leages_and_cups/all_competitions.csv)

### Transfermarkt

For transfermarkt.com data (valuations and transfers), a list of leagues
and seasons included in the package can be found in the
`worldfootballR_data` repository and can be found
[here](https://github.com/JaseZiv/worldfootballR_data/blob/master/raw-data/transfermarkt_leagues/main_comp_seasons.csv)

### Understat

The following leagues are currently supported by Understat (these values
can be passed in to the `league` arguments of most `understat_`
functions):

-   “EPL”
-   “La liga”
-   “Bundesliga”
-   “Serie A”
-   “Ligue 1”
-   “RFPL”

------------------------------------------------------------------------

## Attribute the Source

When using the functions in the package, please ensure you attribute the
source of the data based on the function you use.

Data providers are listed below:

-   [StatsBomb via FBref](https://fbref.com/en/)
-   [Transfermarkt](https://www.transfermarkt.com/)
-   [Understat](https://understat.com/)

### Acknowledgements

Special mention goes out to [Ewan
Henderson’s](https://github.com/ewenme) awesome
[`understatr`](https://github.com/ewenme/understatr) library for the
inspiration and internal code for the `understat_` functions contained
in this package.

------------------------------------------------------------------------

## Contributing

### Issues and Improvements

When creating an issue, please include:

-   Reproducible examples
-   A brief description of what the expected results are
-   If applicable, the fbref.com, transfermarkt.com or understat.com
    page the observed behaviour is occuring on
-   For improvement suggestions, what features are being requested and
    their purpose

Feel free to get in touch via email or twitter
<https://twitter.com/jaseziv> if you aren’t able to create an issue.

### Show your support

Follow me on Twitter ([jaseziv](https://twitter.com/jaseziv)) for
updates

If this package helps you, all I ask is that you star this repo
