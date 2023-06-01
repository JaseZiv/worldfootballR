
#' @importFrom Randomuseragent random_useragent
.onLoad <- function(libname, pkgname) {
  agent_option <- getOption("worldfootballR.agent")
  if (is.null(agent_option)) {
    agent <- Randomuseragent::random_useragent(software_type = "browser")
    options("worldfootballR.agent" = agent)
  }
}
