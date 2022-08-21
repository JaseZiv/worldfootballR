
#' @importFrom rstudioapi isAvailable versionInfo
#' @importFrom utils sessionInfo
.onLoad <- function(libname, pkgname) {
  agent_option <- getOption("worldfootballR.agent")
  if (is.null(agent_option)) {
    prefix <- if(interactive() & rstudioapi::isAvailable()) {
      version_info <- rstudioapi::versionInfo()
      substr(version_info$mode, 1, 1) <- toupper(substr(version_info$mode, 1, 1))
      sprintf(
        "RStudio %s (%s)",
        version_info$mode,
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
    options("worldfootballR.agent" = agent)
  }
}
