
makeAttrs <- function(node) {
  attrs <- xmlAttrs(node)
  names(attrs) %>%
    Map(function (name) {
      val <- attrs[[name]]
      paste0(name, ' = ', if (val == "") "NA" else paste0('"', val, '"'))
    }, .)
}

Keep <- function(fun, xs) Map(fun, xs) %>% Filter(Negate(is.null), .)

renderNode <- function(node, indent = 0, prefix = FALSE) {
  if (xmlName(node) == "text") {
    txt <- xmlValue(node)
    if (nchar(trimws(txt)) > 0) {
      paste0('"', trimws(txt), '"')
    }
  } else {
    tagName <- if (prefix) paste0("tags$", xmlName(node)) else xmlName(node)
    newIndent <- indent + length(tagName) + 1
    xmlChildren(node) %>%
      Keep(partial(renderNode, indent = newIndent, prefix = prefix), .) %>%
      append(makeAttrs(node), .) %>%
      paste(collapse = str_pad(",\n", width = newIndent, side = c("right"))) %>%
      trimws(which = c("left")) %>%
      paste0(tagName, "(", ., ")")
  }
}

html2R <- function(htmlStr, prefix = FALSE) {
  message("adopted from https://github.com/alandipert/html2r")
  library(shiny)
  library(XML)
  library(magrittr)
  library(purrr)
  library(stringr)

  htmlStr %>%
    htmlParse %>%
    getNodeSet("/html/body/*") %>%
    `[[`(1) %>%
    renderNode(prefix = prefix)
}
