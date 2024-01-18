
#' @source <https://github.com/tidyverse/rvest/blob/chromote/R/chromote.R>
#' @importFrom rlang check_installed
#' @noRd
worldfootballr_chromote_session <- function(url) {
  rlang::check_installed(c("chromote", "R6"))
  WorldfootballRDynamicPage$new(url)
}

WorldfootballRDynamicPage <- R6::R6Class("WorldfootballRDynamicPage", public = list(
  session = NULL,
  root_id = NULL,

  initialize = function(url) {
    self$session <- chromote::ChromoteSession$new()

    p <- self$session$Page$loadEventFired(wait_ = FALSE)
    self$session$Page$navigate(url, wait_ = FALSE, timeout_ = 5)
    self$session$wait_for(p)

    self$root_id <- self$session$DOM$getDocument(0)$root$nodeId
  },

  find_nodes = function(css) {
    unlist(self$session$DOM$querySelectorAll(self$root_id, css)$nodeIds)
  },

  call_node_method = function(node_id, method, ...) {
    js_fun <- paste0("function() { return this", method, "}")
    obj_id <- self$object_id(node_id)
    self$session$Runtime$callFunctionOn(js_fun, objectId = obj_id, ...)
  },

  object_id = function(node_id) {
    self$session$DOM$resolveNode(node_id)$object$objectId
  }

))

#' @importFrom rvest html_table
#' @importFrom purrr map_chr
#' @importFrom xml2 xml_children read_html
#' @noRd
worldfootballr_html_player_table <- function(session) {
  stopifnot(identical(class(session), c("WorldfootballRDynamicPage", "R6")))

  ## find element "above" commented out table
  node_id0 <- session$find_nodes("#stats_shooting_sh")
  ## skip 1 for the div "placeholder"
  node_id <- node_id0 + 2L

  elements <- session$call_node_method(node_id, ".textContent")[["result"]][["value"]]
  n_elements <- length(elements)
  if (n_elements != 1) {
    warning(sprintf("Did not find the expected number of tables on the page (3). Found %s.", n_elements))
    return(NULL)
  }

  html <- paste0("<html>", paste0(elements, collapse = "\n"), "</html>")
  xml2::read_html(html)
}
