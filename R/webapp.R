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

  # web$browse <- browse_generator(web)

  web$translate_HTML2rTags <- convertHTML2RTags

  web$translate_HTML_fromClipboard <- translate_HTML_fromClipboard(web)

  web$browse <- function(){
    httwX <- servr::httw(
      dir=dirname(.GlobalEnv$web$output_filepath()),
      baseurl=.GlobalEnv$web$html_filename)
    .GlobalEnv$web$server <- httwX
  }

  web$config_cssJsPath <- config_cssJsPath_generator(web)
  web$config_cssJsPath()

  web$update_css_js <- update_css_js(web)

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
  string %>% paste0(collapse = "\n") %>%
    translate_imgTag() %>%
    translate_inputTag() %>%
    stringr::str_replace_all("<(?!/)", "tags$") %>%
    stringr::str_replace_all("</[a-z]+[1-6]?>", ")") %>%
    stringr::str_replace_all('(?<=tags\\$[a-z]{1,10}[1-6]?)\\s', "(") %>%
    stringr::str_replace_all('(?<=\\"[:alnum:]{1,15}\\")[\\s]+(?=[:alpha:])', ", ") %>%
    stringr::str_replace_all(">", ", ") %>%
    add_charQuotation() %>%
    fix_tagHasNoLeftParenthesis() %>%
    fix_inputAfterQuotationHasNoComma() -> tempOutput
  add_quote2attributeNameWithDash(tempOutput) -> output
  output %>% fix_smallThings() -> output
  output %>% fix_stringUnquoted() -> output

  # change tags$!--...-- to #--...--
  stringr::str_replace_all(output,
                           stringr::fixed("tags$!"),"#") -> output

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
#' Generate a browse function
#'
#' @param html_complete A shiny tag list
#'
#' @return
#' @export
#'
#' @examples None
attachHtmlToolsBrowsable <- function(html_complete){
  function(){
    htmltools::browsable(html_complete)
  }
}
config_cssJsPath_generator <- function(web){
  function(cssJsPath=NULL){
    if(!is.null(cssJsPath)){
      web$cssJsPath <- cssJsPath
    } else {
      .root <- rprojroot::is_rstudio_project$make_fix_file()
      # file.path(
      #   .root(),
      #   {
      #     rstudioapi::getSourceEditorContext() -> currentSource
      #     dirname(currentSource$path)
      #     basename(dirname(currentSource$path))
      #   }) -> web$cssJsPath
      rstudioapi::getSourceEditorContext() -> currentSource
      dirname(currentSource$path) -> web$cssJsPath
    }
  }
}
add_quote2attributeNameWithDash <- function(txt){
  stringr::str_extract_all(txt,
                           "[:alpha:]{1,10}-[:alpha:]{1,10}(?=\\=)") -> extractedTxt
  unlist(extractedTxt) -> extractedTxt
  paste0(", `",extractedTxt,"`") -> replacedTxt
  revise_by_case(txt, extractedTxt, replacedTxt)
}
translate_imgTag <- function(txt){
  count = 0
  maxCount = 100
  stringr::str_locate(txt, "<img") -> whichLocsHaveImgTag
  flag_continue = !all(is.na(whichLocsHaveImgTag))

  while(flag_continue && count <= maxCount){
    loc_startOpeningArrow <- whichLocsHaveImgTag[1, "start"]
    loc_endOpeningArrow <- whichLocsHaveImgTag[1, "end"]

    stringr::str_locate_all(txt, ">")  %>% {.[[1]][,1]} -> whichLocsHaveClosingArrow
    loc_pairedClosingArrow <-
      whichLocsHaveClosingArrow[min(which(whichLocsHaveClosingArrow > loc_startOpeningArrow))]
    txt2bReplaced <- stringr::str_sub(txt, loc_startOpeningArrow, loc_pairedClosingArrow)
    stringr::str_replace_all(txt2bReplaced, "<img ", "tags$img(") %>%
      stringr::str_replace_all(">",")") -> txt2bcome
    stringr::str_replace_all(
      txt,
      stringr::fixed(txt2bReplaced),
      txt2bcome
    ) -> txt

    stringr::str_locate(txt, "<img") -> whichLocsHaveImgTag
    flag_continue = !all(is.na(whichLocsHaveImgTag))

  }
  return(txt)
}
update_css_js <- function(web){
  function(){
    cssFolder <-
      file.path(web$cssJsPath,"css")
    jsFolder <-
      file.path(web$cssJsPath,"js")

    .GlobalEnv$drake$loadTarget$myDependency()
    scriptFrom =
      file.path(myDependency$src,
                myDependency$script)
    styleFrom =
      file.path(myDependency$src,
                myDependency$stylesheet)
    libName = paste0(myDependency$name,"-",myDependency$version)
    libPath = file.path(
      dirname(web$output_filepath()),"lib",
      libName
    )
    scriptTo =
      file.path(libPath,
                myDependency$script)
    styleTo =
      file.path(libPath,
                myDependency$stylesheet)

    file.copy(
      from = scriptFrom,
      to = scriptTo,
      overwrite = T
    )
    file.copy(
      from = styleFrom,
      to = styleTo,
      overwrite = T
    )
  }

}
translate_nonPairedTag <- function(tagName){
  function(txt){
    count = 0
    maxCount = 100
    stringr::str_locate(txt, glue::glue("<{tagName}")) -> whichLocsHaveImgTag
    flag_continue = !all(is.na(whichLocsHaveImgTag))

    while(flag_continue && count <= maxCount){
      loc_startOpeningArrow <- whichLocsHaveImgTag[1, "start"]
      loc_endOpeningArrow <- whichLocsHaveImgTag[1, "end"]

      stringr::str_locate_all(txt, ">")  %>% {.[[1]][,1]} -> whichLocsHaveClosingArrow
      loc_pairedClosingArrow <-
        whichLocsHaveClosingArrow[min(which(whichLocsHaveClosingArrow > loc_startOpeningArrow))]
      txt2bReplaced <- stringr::str_sub(txt, loc_startOpeningArrow, loc_pairedClosingArrow)
      stringr::str_replace_all(txt2bReplaced,
                               glue::glue("<{tagName} "),
                               glue::glue("tags${tagName}(")) %>%
        stringr::str_replace_all(">",")") -> txt2bcome
      stringr::str_replace_all(
        txt,
        stringr::fixed(txt2bReplaced),
        txt2bcome
      ) -> txt

      stringr::str_locate(txt, glue::glue("<{tagName}")) -> whichLocsHaveImgTag
      flag_continue = !all(is.na(whichLocsHaveImgTag))

    }

    return(txt)
  }
}
translate_inputTag <-
  translate_nonPairedTag("input")

fix_smallThings <- function(txt){
  stringr::str_replace_all(txt,
                           stringr::fixed("/)"), ")") -> txt
  fix_inputTagArgsHaveNoValue(txt) -> txt
  return(txt)
}
fix_inputTagArgsHaveNoValue <- function(string){
  flag_hasArgsInsideInputTag <-
    stringr::str_detect(
      string,
      "(?<=(tags\\$input\\())[^\\(\\)]+(?=(\\)))"
    )
  if(flag_hasArgsInsideInputTag)
  {
    stringr::str_extract_all(
      string,
      "(?<=(tags\\$input\\())[^\\(\\)]+(?=(\\)))"
    ) -> allInputArgs
    # .x =1
    for(.x in seq_along(allInputArgs[[1]]))
    {
      inputArgsX <- allInputArgs[[1]][[.x]]
      flag_someArgsHaveNoValue <-
        {
          stringr::str_detect(
            inputArgsX,
            "\\b[[a-zA-Z0-9]-]+\\b(?![\\=\"\'[:graph:]])" #"\\b[[:alpha:]\\-]+\\b(?![\\=\"\'])"
          )
        }
      if(!flag_someArgsHaveNoValue){
        next
      } else {
        # browser()
        argStringHasNoValue <-
          stringr::str_extract(
            inputArgsX,
            "\\b[[a-zA-Z0-9]-]+\\b(?![\\=\"\'[:graph:]])"
          )
        # browser()
        argStringFixed <-
          paste0(
            "`",argStringHasNoValue,"`=NA"
          )
        stringr::str_replace(
          inputArgsX, stringr::fixed(argStringHasNoValue), argStringFixed
        ) -> inputArgsXFixed
      }

      stringr::str_replace(
        string,
        stringr::fixed(inputArgsX),
        inputArgsXFixed
      ) -> string
    }
  }


  return(string)
}
get_string2beQuoted <- function(txt2){
  stringr::str_extract_all(txt2,
                           '(?<=([\\),\"]))[^\\),\"]*(?=\\))') -> txt3
  txt3 <- unlist(txt3)
  whichIsNotEmpty <- which(txt3 != "")
  txt4 <- txt3[whichIsNotEmpty]
  stringr::str_subset(
    txt4, "(\n[\\s]*)", negate=T
  ) -> txt5
  return(txt5)
}

fix_stringUnquoted <- function(txt2){
  get_string2beQuoted(txt2) -> txt3

  for(.x in seq_along(txt3))
  {
    strExtracedX <- txt3[[.x]]
    str2beReplacedX <- paste0("\"", txt3[[.x]], "\"")
    revise_by_case(txt2,
                   stringr::fixed(strExtracedX), str2beReplacedX) -> txt2

  }

  stringr::str_replace_all(
    txt2,
    "(?<=(\\)))[\\s]*[\"]", ", \""
  ) -> txt4
  stringr::str_replace_all(
    txt4,
    "(?<=(\\)))[\\s]*[\']", ", \'"
  ) -> txt5
  return(txt5)
}
