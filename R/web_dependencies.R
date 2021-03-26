materialise <- function() {
  mtl_cdn <- "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/"
  mtl_css <- "css/materialize.min.css"
  mtl_js <- "js/materialize.min.js"

  htmltools::htmlDependency(
    name = "mtl",
    version = "1.0",
    src = c(href = mtl_cdn),
    stylesheet = mtl_css,
    script = mtl_js
  )
}
