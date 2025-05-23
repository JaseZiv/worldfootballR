% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_league_injuries.R
\name{tm_league_injuries}
\alias{tm_league_injuries}
\title{Get league injuries}
\usage{
tm_league_injuries(country_name, league_url = NA)
}
\arguments{
\item{country_name}{the country of the league's players}

\item{league_url}{league url from transfermarkt.com. To be used when country_name not available in main function}
}
\value{
returns a dataframe of injured players in the selected league
}
\description{
Returns a data frame of all currently injured players for a selected league
}
