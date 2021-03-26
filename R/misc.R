#' Generate a output path referring to the sibling folder of the project folder
#'
#' @param foldername a character, default="docs" for github page purpose 
#'
#' @return
#' @export
#'
#' @examples
output_path <- function(foldername="docs"){
  .root <- rprojroot::is_rstudio_project$make_fix_file()
  outputPath <- file.path(dirname(.root()), foldername)
  if(!dir.exists(outputPath)) dir.create(outputPath)
  return(outputPath)
} 
