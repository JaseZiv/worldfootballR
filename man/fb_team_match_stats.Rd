% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fb_team_match_stats.R
\name{fb_team_match_stats}
\alias{fb_team_match_stats}
\title{Get FBref match team stats}
\usage{
fb_team_match_stats(match_url, time_pause = 3)
}
\arguments{
\item{match_url}{the fbref.com URL for the required match}

\item{time_pause}{the wait time (in seconds) between page loads}
}
\value{
returns a dataframe with the match team stats for a selected match
}
\description{
Returns match team stats for selected matches.
}
\examples{
\dontrun{
try({
match <- fb_match_urls(country = "AUS", gender = "F", season_end_year = 2021, tier = "1st")[1]
df <- fb_team_match_stats(match_url = match)
})
}
}
