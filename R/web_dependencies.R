#' Create a list of dependencies to choose
#'
#' @return
#' @export
#'
#' @examples none
html_dependencies <- function(){
  list(
    materialise = materialise,
    jquery = jquery
  )
}

materialise <- function() {
  mtl_cdn <- "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/"
  mtl_css <- "css/materialize.min.css"
  mtl_js <- "js/materialize.min.js"

  htmltools::htmlDependency(
    name = "materialise",
    version = "1.0.0",
    src = c(href = mtl_cdn),
    stylesheet = mtl_css,
    script = mtl_js,
    meta = list(viewport="width=device-width, initial-scale=1.0"),
    head = '<link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">'
  )
}
jquery <- function(){
  jquery_cdn <- "https://code.jquery.com"
  jquery_script <- "jquery-3.6.0.min.js"
  htmltools::htmlDependency(
    name="jquery",
    version="3.6.0",
    src=c(href=jquery_cdn),
    script = list(
      src=jquery_script,
      integrity = "sha256-/xUj+3OJU5yExlq6GSYGSHk7tPXikynS7ogEvDej/m4=",
      crossorigin="anonymous"
    )
  )

}
