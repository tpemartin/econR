#' web instance generator
#'
#' @return
#' @export
#'
#' @examples None
Web <- function(){
  # assertthat::assert_that(
  #   exists("drake", envir=.GlobalEnv),
  #   msg="There is no drake in global environment. Please initiate drake."
  # )
  if(!exists("drake", envir=.GlobalEnv)) warning("There is no drake in global environment. Please initiate drake.")
  web <- new.env()
  # web$convertHTML2RTags <- convertHTML2RTags

  web$rmdfilename <- .GlobalEnv$drake$activeRmd$filenames

  web$htmlDependencies <- list()
  web$htmlDependencies <-
    append(
      web$htmlDependencies,
      html_dependencies()
    )
  # web$htmlDependencies$materialise <- materialise

  flag_noWebpageNeeded <- !exists("foldername", envir=web) || !exists("html_filename", envir=web)
  # if(flag_noWebpageNeeded){
  #   dependencies <- getDependenciesFromDrake()
  #   web$browse <-
  #     generate_drakebrowsable2(dependencies)
  #
  # } else
  {
    attachMethodsRelated2outputfilepath(web)
  }


  # web$browse <- browse_generator(web)

  web$translate_HTML2rTags <- function(string){
    html2R(string, T)
  }

  web$translate_HTML_fromClipboard <- translate_HTML_fromClipboard(web)

  # web$translate_js_chunk <- web_translateJsChunk2RChunk(web)
  web$update <- web_update(web)
  web$update_hard <- web_update_hard(web)

  web$browsable <- web_browsable

  web$assets <- list()
  web$assets$generate_dependency <- generate_assets_dependency(web)


  web$img <- list()
  web$img$set_image_path <- set_image_path(web)

  web$tools$htmlDependencyPath <- htmlDependency_path

  # React
  web$react <-
    list(
      create_script = function(){
        web$react$script <-
          create_reactiveScript()
        web$react$clipboard <- function(){
          clipr::write_clip(
            web$react$script
          )
        }
      }
    )

  web$generate_browsableWithDependencies <- BrowseWithDependencies

  web$dependencyTool <- list(
    srcFileAbsolute = dependencyFileAbsolute
  )
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
    .root <- find_webappRoot
      # rprojroot::is_rstudio_project$make_fix_file()
    output_folder <-
      file.path(dirname(.root()), .GlobalEnv$web$foldername)
    if(!dir.exists(output_folder)) dir.create(output_folder)
    output_filepath <-
      file.path(
        output_folder, .GlobalEnv$web$html_filename
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
      .root <- find_webappRoot#rprojroot::is_rstudio_project$make_fix_file()
      # file.path(
      #   .root(),
      #   {
      #     rstudioapi::getSourceEditorContext() -> currentSource
      #     dirname(currentSource$path)
      #     basename(dirname(currentSource$path))
      #   }) -> web$cssJsPath
      rstudioapi::getSourceEditorContext() -> currentSource
      rootCssJs <- file.path(currentSource$path,"css")
      rootJs <- file.path(currentSource$path, "js")
      if(dir.exists(rootJs) && dir.exists(rootCss)){
        dirname(currentSource$path) -> web$cssJsPath
      } else {

          file.path(dirname(currentSource$path),"assets") -> web$cssJsPath
      }
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
    if(!is.null(web$dependencies)){
      web$output_filepath()
      save_html(
        web$dependencies,
        file = dirname(web$output_filepath()) %//% "temp.html"
      )
      file.remove(
        dirname(web$output_filepath()) %//% "temp.html"
      )

    } else {
      .GlobalEnv$drake$loadTarget$html_complete()
      htmltools::save_html(
        html_complete,
        file = web$output_filepath(),
        libdir="lib"
      )
    }
    # {
    #   cssFolder <-
    #     file.path(web$cssJsPath,"css")
    #   jsFolder <-
    #     file.path(web$cssJsPath,"js")
    #
    #   .GlobalEnv$drake$loadTarget$myDependency()
    #   scriptFrom =
    #     file.path(myDependency$src,
    #       myDependency$script)
    #   styleFrom =
    #     file.path(myDependency$src,
    #       myDependency$stylesheet)
    #   libName = paste0(myDependency$name,"-",myDependency$version)
    #   libPath = file.path(
    #     dirname(web$output_filepath()),"lib",
    #     libName
    #   )
    #   scriptTo =
    #     file.path(libPath,
    #       myDependency$script)
    #   styleTo =
    #     file.path(libPath,
    #       myDependency$stylesheet)
    #
    #   file.copy(
    #     from = scriptFrom,
    #     to = scriptTo,
    #     overwrite = T
    #   )
    #   file.copy(
    #     from = styleFrom,
    #     to = styleTo,
    #     overwrite = T
    #   )
    #
    # }

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
  # stringr::str_view_all(txt2,
  #                          '(?<=([\\),\"]))[^\\),\"]*(?=\\))')
  #

  txt3 <- unlist(txt3)
  stringr::str_remove_all(txt3, "\\s") -> txt3noSpace

  whichIsNotEmpty <- which(txt3noSpace != "")

  whichIsNotBackTicks <- stringr::str_which(txt3noSpace, "^`", negate=T)

  whichIs2BeProcessed <- dplyr::intersect(whichIsNotEmpty, whichIsNotBackTicks)
  txt4 <- txt3[whichIs2BeProcessed]
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
createTextRmd <- function(web)
{
  function(){
    require(dplyr)
    # .activeFile <- rstudioapi::getSourceEditorContext() %>% .$path
    # stringr::str_replace(
    #   .activeFile, "\\.[Rr][Mm][Dd]", "_text.Rmd"
    # ) -> textRmdfilename
    if(!file.exists(web$json$textRmdfilename)){
      xfun::write_utf8(
        "", con=web$json$textRmdfilename
      )
    }
    print(glue::glue("textRmd is created at \n{web$json$textRmdfilename}"))
    web$json$rmd_file.edit <- function(){
      file.edit(web$json$textRmdfilename)
    }
    # web$textRmd$.autopsy <-
    #   export_textRmdlinesAsTibble(web)
  }

}

export_textRmdlinesAsTibble <- function(web){
  # function(){
    require(dplyr)
    # .activeFile <- rstudioapi::getSourceEditorContext() %>% .$path
    filelines <- readLines(web$json$textRmdfilename)
    stringr::str_which(filelines, "^#") -> lineBreaks
    lineBreakIds <- stringr::str_extract(filelines[lineBreaks], "(?<=\\{#)[^#]+(?=\\})")
    whichLineBreakHasId <- which(!is.na(lineBreakIds))

    whichLevelsNeedChange2Ids <- lineBreaks[whichLineBreakHasId]
    cut(
      seq_along(filelines),
      breaks=c(0, lineBreaks, Inf), right=F
    ) -> cut_filelines
    levelNames <- levels(cut_filelines)
    which(as.integer(stringr::str_extract(
      levelNames,"(?<=\\[)[0-9]+")) %in% whichLevelsNeedChange2Ids
    ) -> pickLevelNames
    levelNames[pickLevelNames] <-
      na.omit(lineBreakIds)
    levels(cut_filelines) <- levelNames
    # cut_filelines
    tibble::tibble(
      lines=filelines,
      cut=cut_filelines
    ) -> tbX
    stringr::str_remove(tbX$lines,"\\s") %>% {.!=""} -> pick_nonEmptylines
    tbX[pick_nonEmptylines,]-> tbXX

    tbXX %>%
      mutate(
        line_noSpace = stringr::str_trim(lines, side="left"),
        lines = if_else(stringr::str_detect(line_noSpace, "^(#|</?[a-z]+>)"), lines, paste0("<p>",lines, "</p>"))) -> tbXX
    tbXX %>%
      group_by(
        cut
      ) %>%
      summarise(
        txt=paste0(lines[-1], collapse = "\n")
      ) %>%
      ungroup() -> web$json$tibble
  # }

}

create_textJson <- function(web){
  function(){
    require(dplyr)

    # web$json$tibble <-
      export_textRmdlinesAsTibble(web)

    # cutLevels <- levels(web$textRmd$tibble$cut)

    templist <-
      setNames(
        as.list(web$json$tibble$txt),
        web$json$tibble$cut)
    # idLevels <- stringr::str_subset(cutLevels, "^[^\\[].*[^\\)]$")
    web$json$list <- templist
    jsonlite::toJSON(
      web$json$list, auto_unbox = T
    ) -> web$json$object

    # jsonfilename <-
    #   stringr::str_replace(
    #     web$textRmd$filename,"\\.[Rr][Mm][Dd]",".json")
    # web$textRmd$jsonfilename <-
    #   file.path(
    #     dirname(web$output_filepath()),
    #     basename(jsonfilename)))

    .root <- find_webappRoot
      #rprojroot::is_rstudio_project$make_fix_file()

    file.path(
      dirname(.root()),
      web$foldername,
      basename(stringr::str_replace(web$jsonRmdfilename,
                         "\\.Rmd",
                         ".json"))) -> web$json$filename

    xfun::write_utf8(
      web$json$object, con=web$json$filename
    )
  }
}

#' Initiate external JSON embody
#'
#' @param web An environment initiated by Web with foldername and filename specified
#'
#' @return
#' @export
#'
#' @examples None
initiateJson <- function(web){
  # function(){
    stringr::str_replace(
      web$rmdfilename, "\\.[Rr][Mm][Dd]", "_text.Rmd"
    ) -> web$jsonRmdfilename

    web$json <- list(
      textRmdfilename = web$jsonRmdfilename)
    jsonfilename_wrongPath <-
      stringr::str_replace(
        web$json$textRmdfilname, "\\.[Rr][Mm][Dd]",".json")
    web$json$filename =
      file.path(
        dirname(web$output_filepath()),
        basename(jsonfilename_wrongPath))

    web$json$create_rmd <- createTextRmd(web)

    web$json$translate_rmd2json <- create_textJson(web)
  # }
  return(web)
}
generate_assets_dependency <- function(web){
  function(name, version, relativeSourcePath){
    .root <- find_webappRoot
      #rprojroot::is_rstudio_project$make_fix_file()
    .htmlDependency <-
      htmltools::htmlDependency(
        name=name,
        version=version,
        src=c(filepath=file.path(.root(),relativeSourcePath)),
        all_files=T
      )
    .src_path <- file.path("lib", paste0(name, "-", version))


    web$assets$dependency = .htmlDependency
    web$assets$src_path = .src_path

  }

}
#' Get script path after htmltools::htmlDependency is resolved
#'
#' @param htmldep An htmlDependency object
#'
#' @return
#' @export
#'
#' @examples none
htmlDependency_path <- function(htmldep){
  # htmldep <- googleSheets$dependency
  output <- list()

  if(!is.null(htmldep$script)){
    subdir <- stringr::str_remove(htmldep$script,"/$")
    purrr::map(
      subdir,
      ~file.path(
        "lib",
        paste0(htmldep[c("name","version")], collapse="-"),
        .x
      )
    ) -> src_path

    setNames(src_path, subdir) -> output$script
  }
  if(!is.null(htmldep$attachment)){
    subdir <- stringr::str_remove(htmldep$attachment,"/$")
    purrr::map(
      subdir,
      ~file.path(
        "lib",
        paste0(htmldep[c("name","version")], collapse="-"),
        .x
      )
    ) -> att_path

    setNames(att_path, subdir) -> output$attachment
  }
  if(!is.null(htmldep$stylesheet)){
    subdir <- stringr::str_remove(htmldep$stylesheet,"/$")
    purrr::map(
      subdir,
      ~file.path(
        "lib",
        paste0(htmldep[c("name","version")], collapse="-"),
        .x
      )
    ) -> css_path

    setNames(css_path, subdir) -> output$stylesheet
  }
  return(output)
}
find_webappRoot <- function(){
  rstudioapi::getSourceEditorContext() -> source
  source$path
  rprojroot::find_rstudio_root_file(path=dirname(source$path))
}
getDependenciesFromDrake <- function(){
  dependenciesName <-
    .GlobalEnv$drake$activeRmd$frontmatter$dependencies
  eval(
    expression(
      .GlobalEnv$drake$loadTarget[[dependenciesName]]()),
    envir = .GlobalEnv)
  sym_dependencies <- sym(dependenciesName)
  expr_dep <- rlang::expr(
    dependencies <- !!sym_dependencies
  )
  rlang::eval_bare(
    expr_dep
  )
  return(dependencies)
}
attachMethodsRelated2outputfilepath <- function(web)
{

  web$output_filepath <- web_output_filepath(web)
  web$browse <- function(port=8880){
    if(exists("server", web)){
      web$server$stop_server()
    }

    httwX <- tryCatch({
      servr::httw(
        dir=dirname(.GlobalEnv$web$output_filepath()),
        baseurl=.GlobalEnv$web$html_filename,
        port=port)

    },
      error=function(e){
        servr::daemon_stop()
        servr::httw(
          dir=dirname(.GlobalEnv$web$output_filepath()),
          baseurl=.GlobalEnv$web$html_filename,
          port=port)

      })
    .GlobalEnv$web$server <- httwX
    rstudioapi::viewer(
      stringr::str_replace(
        .GlobalEnv$web$server$url, "(?<=)[0-9\\.]+","localhost"
      )
    )
  }

  web$config_cssJsPath <- config_cssJsPath_generator(web)
  web$config_cssJsPath()

  web$update_dependencies <- update_css_js(web)
}
generate_drakebrowsable2 <- function(dependencies){

  function(ui){
    sym_ui <- rlang::ensym(ui)
    paste0("drake$loadTarget$", as.character(sym_ui),"()") -> chr_expr
    eval(parse(text=chr_expr), envir = .GlobalEnv)

    rlang::expr(
      htmltools::browsable(
        htmltools::attachDependencies(
          !!sym_ui,
          dependencies
        )
      )
    ) -> expr_browsable
    rlang::eval_bare(
      expr_browsable, env=.GlobalEnv
    )
  }
}
BrowseWithDependencies <- function(dep){
  function(x){
    htmltools::browsable(
      htmltools::attachDependencies(
        x,
        dep
      )
    )
  }
}
dependencyFileAbsolute <- function(argList){
  # argList <- list(...)
  # argList <- list(
  #   name="event",
  #   version="1.0.0",
  #   src = c(file="support/assets/event"),
  #   script = "event.js",
  #   stylesheet = "event.css"
  # )

  if("file" %in% names(argList$src)){
    filepath = argList$src[["file"]]
    rootpath = (rprojroot::is_rstudio_project$make_fix_file())()
    if(!stringr::str_detect(
      filepath,paste0("^",rootpath)
    )) {
      filepath2 = file.path(rootpath, filepath)
      argList$src[["file"]] = filepath2}
  }

  effective_dependencies <-
    do.call(
      htmltools::htmlDependency,
      argList
    )
  return(
    effective_dependencies
  )
}
