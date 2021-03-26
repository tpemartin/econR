#' web instance generator
#'
#' @return
#' @export
#'
#' @examples None
Web <- function(){
  web <- new.env()
  web$convertHTML2RTags <- convertHTML2RTags

  web$htmlDependencies <- list()
  web$htmlDependencies$materialise <- materialise

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
    stringr::str_replace_all("</[a-z]+>", ")") %>%
    stringr::str_replace_all('(?<=tags\\$[a-z]{1,10})\\s', "(") %>%
    stringr::str_replace_all('(?<=\\"[:alnum:]{1,15}\\")[\\s]+(?=[:alpha:])', ", ") %>%
    stringr::str_replace_all(">", ", ") %>%
    add_charQuotation() -> output
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