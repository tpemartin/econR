#' Initiate a web instance
#'
#' @return
#' @export
#'
#' @examples None.
initiate_web <- function(){
  .GlobalEnv$drake <- rmd2drake:::Drake()
  .GlobalEnv$drake$source_plan()
  .GlobalEnv$drake$makePlan()
  .GlobalEnv$web <- econR::Web2()
}

