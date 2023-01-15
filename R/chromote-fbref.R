
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

  call_node_method = function(node_id) {
    js_fun <- paste0("function() { return this.outerHTML}")
    obj_id <- self$object_id(node_id)
    self$session$Runtime$callFunctionOn(js_fun, objectId = obj_id)
  },

  object_id = function(node_id) {
    self$session$DOM$resolveNode(node_id)$object$objectId
  }

))

#' @importFrom rvest html_table
#' @importFrom purrr map_chr
#' @importFrom xml2 xml_children read_html
#' @noRd
worldfootballr_html_table <- function(x) {
  stopifnot(identical(class(x), c("WorldfootballRDynamicPage", "R6")))
  nodes <- x$find_nodes("table")

  elements <- purrr::map_chr(nodes, function(node_id) {
    json <- x$call_node_method(node_id)
    json$result$value
  })
  html <- paste0("<html>", paste0(elements, collapse = "\n"), "</html>")
  res <- xml2::xml_children(xml2::xml_children(xml2::read_html(html)))
  rvest::html_table(res)
}
