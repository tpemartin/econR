
#' web instance generator
#'
#' @return
#' @export
#'
#' @examples None
Web2 <- function(){
  assertthat::assert_that(
    exists("drake", envir=.GlobalEnv),
    msg="There is no drake in global environment. Please initiate drake.")

  web <- new.env()
  # web$convertHTML2RTags <- convertHTML2RTags

  web$rmdfilename <- .GlobalEnv$drake$activeRmd$filenames

  drake <- .GlobalEnv$drake

  flag_nofrontmatterDependencies <-
    is.null(drake$activeRmd$frontmatter$dependencies)
  if(flag_nofrontmatterDependencies) warning("no dependencies: 'dep_object_name' set in frontmatter.\n web$browsable will have no default dependency.")

  # attach htmlDependencies and browsable
  if(!flag_nofrontmatterDependencies){
    # throw dependencies to .GlobalEnv
      drake$loadTarget[[drake$activeRmd$frontmatter$dependencies]]()
    web$htmlDependencies <-
      .GlobalEnv[[drake$activeRmd$frontmatter$dependencies]]


    web$browsable <- function(ui){
      tryCatch({
        sym_ui <- rlang::ensym(ui)
        if(
          is.null(.GlobalEnv[[as.character(sym_ui)]])
        ){
          .GlobalEnv$drake$loadTarget[[as.character(sym_ui)]]()
          ui_element = .GlobalEnv[[as.character(sym_ui)]]
        }
        ui_element
      },
        error=function(e){
          ui_element=ui
          ui_element
        }) -> ui_element


      viewer_browsable(
        htmltools::tagList(
          ui_element,
          web$htmlDependencies
        )
      )

    }

  }

  # assertthat::assert_that(
  #   exists("drake", envir=.GlobalEnv),
  #   msg="There is no drake in global environment. Please initiate drake."
  # )



  flag_webpageOutputNeeded <- !is.null(.GlobalEnv$drake$activeRmd$frontmatter$output$html_tag)
  if(flag_webpageOutputNeeded){
    attachMethod_getOutputFilepath(web) -> web
    attachMethodsRelated2outputfilepath2(web) -> web
  }



  # web$browse <- browse_generator(web)

  web$translate_HTML2rTags <- convertHTML2RTags

  web$translate_HTML_fromClipboard <- translate_HTML_fromClipboard(web)




  # web$translate_js_chunk <- web_translateJsChunk2RChunk(web)
  web$update <- web_update(web)
  web$update_hard <- web_update_hard(web)

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

  return(web)
}
attachMethod_getOutputFilepath <- function(web){
  drake <- .GlobalEnv$drake
  parse_frontmatter(drake$activeRmd$frontmatter$output$html_tag$dirpath) -> dirpath
  parse_frontmatter(
    drake$activeRmd$frontmatter$output$html_tag$filename
  ) -> filename
  parse_frontmatter(
    drake$activeRmd$frontmatter$output$html_tag$object
  ) -> objectname

  web$html_filename <- filename

  web$output_filepath <- function(){
    dirpath %//% filename
  }

  # load saving object and save
  drake$loadTarget[[objectname]]()
  htmltools::save_html(
    htmltools::tagList(
      .GlobalEnv[[objectname]],
      web$htmlDependencies
      ),
    file = web$output_filepath(),
    libdir="lib")

  return(web)
}

attachMethodsRelated2outputfilepath2 <- function(web)
{
  web$port <- 8880
  web$browse <- function(port=NULL){
    if(!is.null(port)) .GlobalEnv$web$port = port
    port=web$port

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

  return(web)
}
parse_frontmatter <- function(string){

  # drake$activeRmd$frontmatter$output$html_tag$dirpath
  # string <- "`r library(econR); . %//% 'docs'`"
  if(stringr::str_detect(string, "`r")){
    string_expr <- stringr::str_extract(
      string, "(?<=`r ).*(?=`)"
    )
    string_parsed <- rlang::parse_exprs(string_expr)
    eval(string_parsed[[1]])
    eval(string_parsed[[2]])
    purrr::map(
      string_parsed,
      eval
    ) -> list_returns
    return(list_returns[[length(list_returns)]])
  } else {
    return(string)
  }

}
viewer_browsable <- function(ui_browsable){
  output_filepath <- tempdir() %//% "index.html"
  htmltools::save_html(
    ui_browsable, file=output_filepath
  )
  rstudioapi::viewer(url=output_filepath)
}
