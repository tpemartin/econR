#' Input a plotly object to create a pily instance for convenient plotly exploration
#'
#' @param plotlyObject A plotly object
#'
#' @return
#' @export
#'
#' @examples
Pily <- function(plotlyObject){
  assertthat::assert_that(
    is(plotlyObject, "plotly"),
    msg="Input object should be a plotly object."
  )
  pily <- new.env()
  pily$.self <- plotlyObject

  pily$get_json <- get_json(pily = pily)

  .GlobalEnv$pily
}

# helpers -----------------------------------------------------------------

get_json <- function(pily){
  function(){
    plotly::plotly_json(pily$.self)

  }
}
