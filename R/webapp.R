#' web instance generator
#'
#' @return
#' @export
#'
#' @examples None
Web <- function(){
  web <- new.env()
  # web$convertHTML2RTags <- convertHTML2RTags

  web$htmlDependencies <- list()
  web$htmlDependencies <-
    append(
      web$htmlDependencies,
      html_dependency()
    )
  # web$htmlDependencies$materialise <- materialise

  web$output_filepath <- web_output_filepath(web)

  web$browse <- browse_generator(web)

  web$translate_HTML2rTags <- convertHTML2RTags

  web$translate_HTML_fromClipboard <- translate_HTML_fromClipboard(web)

  return(web)
}

#' Convert a string of HTML tag into htmltools::tags
#'
#' @param string A string
#'
#' @return invisible of a string of htmltools::tags format. Clipboard will always be written in the converted result for the convenience of pasting back to R script.
#' @export
#'
#' @examples None
convertHTML2RTags <- function(string){
  require(dplyr)
  string %>%
    stringr::str_replace_all("<(?!/)", "tags$") %>%
    stringr::str_replace_all("</[a-z]+[1-6]?>", ")") %>%
    stringr::str_replace_all('(?<=tags\\$[a-z]{1,10}[1-6]?)\\s', "(") %>%
    stringr::str_replace_all('(?<=\\"[:alnum:]{1,15}\\")[\\s]+(?=[:alpha:])', ", ") %>%
    stringr::str_replace_all(">", ", ") %>%
    add_charQuotation() %>%
    fix_tagHasNoLeftParenthesis() %>%
    fix_inputAfterQuotationHasNoComma() -> output
  output %>% clipr::write_clip()
  invisible(output)
}
add_charQuotation <- function(txt){
  txt_extracted <- unlist(stringr::str_extract_all(txt, '(?<=[\\(,])[[:alnum:]\\s]+(?=[,\\)])'))
  txt2bReplaced <- paste0("\"", txt_extracted,"\"")

  for(i in seq_along(txt_extracted)){
    stringr::str_replace_all(
      txt,
      pattern = paste0('(?<=[\\(,])(', txt_extracted[[i]] ,')(?=[,\\)])'),
      txt2bReplaced[[i]]
    ) -> txt
  }
  return(txt)
}
revise_by_case <- function(txt, txt_extracted, txt2bReplaced){

  for(i in seq_along(txt_extracted)){
    stringr::str_replace_all(
      txt,
      pattern = txt_extracted[[i]],
      txt2bReplaced[[i]]
    ) -> txt
  }
  return(txt)
}
fix_tagHasNoLeftParenthesis <- function(txt){
  # txt <- 'tags$li, tags$a(class="grey-text text-lighten-3" href="#!"," Link 1"))'
  txt <- unlist(stringr::str_replace_all(
    txt,
    "(?<=tags\\$[:alpha:]{1,10}[1-6]?),","("
  ))
  return(txt)
}
fix_inputAfterQuotationHasNoComma <- function(txt){
  # txt <- 'tags$li( tags$a(class="grey-text text-lighten-3" href="#!"," Link 1"))'
  stringr::str_replace_all(txt, "[\"'](?=([\\s]{0,10}[:alpha:]{1,10}=))","\",")
}
browse_generator <- function(web){
  function(){
    assertthat::assert_that(
      exists("foldername", envir=web),
      msg="Please set up web$foldername, like web$foldername <- 'docs'."
    )
    assertthat::assert_that(
      exists("html_filename", envir=web),
      msg="Please set up web$html_filename, like web$html_filename <- 'index.html'."
    )
    # htmlfilepath <- file.path(
    #   web$output_path(web$foldername),
    #   web$html_filename
    # )
    browseURL(web$output_filepath())

  }
}
web_output_filepath <- function(web){
  function(){
    .root <- rprojroot::is_rstudio_project$make_fix_file()
    output_folder <-
      file.path(dirname(.root()), web$foldername)
    if(!dir.exists(output_folder)) dir.create(output_folder)
    output_filepath <-
      file.path(
        output_folder, web$html_filename
      )
    return(output_filepath)
  }
}
translate_HTML_fromClipboard <- function(web){
  function(){
    web$translate_HTML2rTags(clipr::read_clip())
  }
}
