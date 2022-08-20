
url <- "https://fbref.com/en/comps/Big5/2021-2022/stats/players/2021-2022-Big-5-European-Leagues-Stats"
upper1 <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

prefix <- if(interactive() & rstudioapi::isAvailable()) {
  version_info <- rstudioapi::versionInfo()
  sprintf(
    "RStudio %s (%s)",
    upper1(version_info$mode),
    version_info$version
  )
} else {
  "RStudio Desktop (2022.7.1.554)"
}

session_info <- utils::sessionInfo()
r_version <- session_info$R.version
agent <- sprintf(
  "%s; R (%s.%s %s %s %s)",
  prefix,
  r_version$major,
  r_version$minor,
  r_version$platform,
  r_version$arch,
  r_version$arch
)

ua <- httr::user_agent(agent)
session <- rvest::session(url = url, ua)
page <- xml2::read_html(session)
print(page)
