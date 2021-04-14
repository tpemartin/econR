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

addBackTick2ChineseCharacter <- function()
{
  require(dplyr)
  rstudioapi::getSourceEditorContext() %>%
    .$path -> filename
  readLines(
    filename
  ) -> rlines
  stringr::str_extract_all(rlines, "(?<![[\u4E00-\u9FFF]\"\'``])[\u4E00-\u9FFF]+") %>%
    unlist() %>% unique() -> string2beReplaced
  for(word in string2beReplaced){
    stringr::str_replace_all(
      rlines,
      glue::glue("(?<![[\u4E00-\u9FFF]\"\'``]){word}"),
      paste0("`",word,"`")
    ) -> rlines
  }
  xfun::write_utf8(
    rlines, con=filename
  )
}
