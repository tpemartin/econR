#' build file.path
#'
#' @param x a path
#' @param y a path extention
#'
#' @return
#' @export
#'
#' @examples x %//% y
`%//%` <- function(x, y){
  tryCatch({
    !is.character(x)
  },
    error=function(e){
      TRUE
    }) -> flag_notCharacter
  if(flag_notCharacter){
    sym_x <- rlang::ensym(x)
    if(sym_x == as.name(".")){
      .root <- rprojroot::is_rstudio_project$make_fix_file()
      x <- .root()
    }
  }
  newpath <- file.path(x,y)
  flag_file <-
    stringr::str_detect(
      basename(newpath),
      "\\.[:alnum:]*$")
  if(!flag_file){
    if(!dir.exists(newpath)) dir.create(newpath, recursive = T)
  }
  return(newpath)
}


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
#' Convert active Rmd to R script file in a purl folder under project root
#'
#' @return
#' @export
#'
#' @examples None
convert2script <- function(){
  rstudioapi::getSourceEditorContext() -> sourceX

  .root <- rprojroot::is_rstudio_project$make_fix_file()
  purlfolder <-
    file.path(
      .root(),"purl"
    )

  # create purl folder
  if(!dir.exists(purlfolder)) dir.create(purlfolder)

  scriptfilepath <-
    file.path(
      purlfolder,
      basename(stringr::str_replace(
        sourceX$path,
        "\\.[Rr][Mm][Dd]$",
        ".R"
      ))
    )

  knitr::purl(
    input = sourceX$path,
    output = scriptfilepath
  )
  file.edit(
    scriptfilepath
  )
}

#' Take user to the repo in github that corresponds to the current rstudio project
#'
#' @param user
#'
#' @return
#' @export
#'
#' @examples none
take_me2github <- function(){
  Sys.getenv("GITHUB_USER") -> user
  if(user==""){
    warning("Please add\nGITHUB_USER='your github username'\nto the pop up file. Then save it and restart your RStudio.")
    usethis::edit_r_environ()
    stop()
  }
  require(rprojroot)
  rprojroot::is_rstudio_project-> pj
  pj$make_fix_file() -> root

  require(dplyr)
  require(stringr)
  repo = {
    root() %>%
      str_extract("[^/]*$")
  }
  browseURL(
    glue::glue(
      "https://github.com/{user}/{repo}"
    )
  )

}


initiate_Renv <- function(){

  file.edit(
    file.path(r_home, ".Renviron")
  )
}
githublink_download_open <- function(){
  link=clipr::read_clip()
  assertthat::assert_that(
    stringr::str_detect(link, "^http"),
    msg=paste(link, " is no a valid url.")
  )
  require(econR)
  stringr::str_extract_all(
    link, "(?<=https://github.com/).+(?=/blob)|(?<=blob/)[^#?&]*"
  ) -> linkInfo
  do.call("file.path", list("https://raw.githubusercontent.com", linkInfo[[1]][[1]], linkInfo[[1]][[2]])) -> rawlink

  filename = basename(rawlink)
  xfun::download_file(
    rawlink, mode="wb",
    output= tempdir() %//% filename
  )
  file.edit(tempdir() %//% filename)
}
