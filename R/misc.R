#' Generate a output path referring to the sibling folder of the project folder
#'
#' @param foldername a character, default="docs" for github page purpose
#'
#' @return
#' @export
#'
#' @examples None
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
#' Download file from url with output path set to the root
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples None
download_file <- function(url){
  .root <- rprojroot::is_rstudio_project$make_fix_file()
  url <- "https://www.dropbox.com/s/2desfhsqbqmq71j/plt_taiwanElection_partyColor.Rdata?dl=1"
  filename <- stringr::str_remove(basename(url),"\\?.+")
  outputfile= file.path(
    .root(), filename
  )
  xfun::download_file(url, output=outputfile, mode="wb")
  return(outputfile)
}

#' Parse dropbox link
#'
#' @param imglink a character of dropbox url starting with www.dropbox.com
#'
#' @return a character of dropbox link that works for <img src=...>
#' @export
#'
#' @examples None
parse_dropboxlink <- function(imglink){
  validlink <- stringr::str_replace(imglink,
                                    "www.dropbox.com",
                                    "dl.dropboxusercontent.com")
  return(validlink)
}
