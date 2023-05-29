
#' @importFrom rstudioapi isAvailable versionInfo
#' @importFrom utils sessionInfo
.onLoad <- function(libname, pkgname) {
  desktop_agent_option <- getOption("worldfootballR.desktop_agent")
  if (is.null(desktop_agent_option)) {
    prefix <- if(interactive() & rstudioapi::isAvailable()) {
      version_info <- rstudioapi::versionInfo()
      substr(version_info$mode, 1, 1) <- toupper(substr(version_info$mode, 1, 1))
      sprintf(
        "RStudio %s (%s)",
        version_info$mode,
        version_info$version
      )
    } else {
      "RStudio Desktop (2023.3.1.446); R (4.2.0 x86_64-w64-mingw32 x86_64 x86_64)"
    }
    session_info <- utils::sessionInfo()
    r_version <- session_info$R.version
    desktop_agent <- sprintf(
      "%s; R (%s.%s %s %s %s)",
      prefix,
      r_version$major,
      r_version$minor,
      r_version$platform,
      r_version$arch,
      r_version$arch
    )
    options("worldfootballR.desktop_agent" = desktop_agent)

  }

  browser_agent_option <- getOption("worldfootballR.browser_agent")
  if (is.null(browser_agent_option)) {
    ## TODO: Figure out how to programmatically identify the browser agent
    browser_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36"
    options("worldfootballR.browser_agent" = browser_agent)
  }
}
