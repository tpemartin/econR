
#' web instance generator
#'
#' @return
#' @export
#'
#' @examples None
Web2 <- function() {
  assertthat::assert_that(
    exists("drake", envir = .GlobalEnv),
    msg = "There is no drake in global environment. Please initiate drake."
  )

  web <- new.env()
  # web$convertHTML2RTags <- convertHTML2RTags

  web$rmdfilename <- .GlobalEnv$drake$activeRmd$filenames

  drake <- .GlobalEnv$drake

  flag_nofrontmatterDependencies <-
    is.null(drake$activeRmd$frontmatter$dependencies)
  assertthat::assert_that(!flag_nofrontmatterDependencies,
    msg = "no dependencies: 'dep_object_name' set in frontmatter.\n web$browsable will have no default dependency."
  )

  # Decide where to source dependencies object to the global environment
  if (!is.null(.GlobalEnv$drake$activeRmd$frontmatter$output$html_tag$dependencyRscriptFilePath)) {
    .GlobalEnv$drake$activeRmd$frontmatter$output$html_tag$dependencyRscriptFilePath %>%
      parse_frontmatter() %>%
      source(local = .GlobalEnv)
  } else
  if (!is.null(.GlobalEnv$drake$activeRmd$frontmatter$dependencyRscriptFilePath)) {
    .GlobalEnv$drake$activeRmd$frontmatter$dependencyRscriptFilePath %>%
      parse_frontmatter() %>%
      source(local = .GlobalEnv)
  } else {
    drake$loadTarget[[drake$activeRmd$frontmatter$dependencies]]()
  }
  web$dependencies <-
    .GlobalEnv[[drake$activeRmd$frontmatter$dependencies]]

  web$browsable <- function(ui, reload = T) {
    tryCatch(
      {
        sym_ui <- rlang::ensym(ui)
        if (
          is.null(.GlobalEnv[[as.character(sym_ui)]]) || reload
        ) {
          .GlobalEnv$drake$loadTarget[[as.character(sym_ui)]]()
          ui_element <- .GlobalEnv[[as.character(sym_ui)]]
        }
        ui_element
      },
      error = function(e) {
        ui_element <- ui
        ui_element
      }
    ) -> ui_element


    viewer_browsable(
      htmltools::attachDependencies(
        ui_element,
        web$dependencies
      )
    )
  }


  flag_webpageOutputNeeded <- !is.null(.GlobalEnv$drake$activeRmd$frontmatter$output$html_tag)
  if (flag_webpageOutputNeeded) {
    attachMethod_getOutputFilepath(web) -> web
    attachMethodsRelated2outputfilepath2(web) -> web
  }



  # web$browse <- browse_generator(web)

  web$translate_HTML2rTags <- function(string, prefix = T) {
    html2R(string, prefix = prefix)
  }

  web$translate_HTML_fromClipboard <- function(prefix = T) {
    html2R(clipr::read_clip(), prefix = prefix) -> translatedTags
    clipr::write_clip(translatedTags)
  }


  web$merge <- web_merge

  # web$translate_js_chunk <- web_translateJsChunk2RChunk(web)
  web$update_dependencies <- update_css_js(web)

  web$update_output <- update_htmloutput

  update <- web_update(web)

  web$update <- function() {
    update()
    update_htmloutput()
  }

  update_hard <- web_update_hard(web)

  web$update_hard <- function() {
    update_hard()
    update_htmloutput()
  }


  web$assets <- list()
  web$assets$generate_dependency <- generate_assets_dependency(web)


  web$img <- list()
  web$img$set_image_path <- set_image_path(web)

  web$tools$htmlDependencyPath <- htmlDependency_path

  # React
  web$react <-
    list(
      create_script = function() {
        web$react$script <-
          create_reactiveScript()
        web$react$clipboard <- function() {
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

  output_filepath <- dirpath %//% filename
  web$output_filepath <- function(){
    output_filepath
  }

  # load saving object and save
  assertthat::assert_that(
    !is.null(drake$loadTarget[[objectname]]),
    msg=glue::glue("frontmatter output object '{objectname}' does not exist in drake targets.")
  )
  drake$loadTarget[[objectname]]()
  htmltools::save_html(
    htmltools::attachDependencies(
      .GlobalEnv[[objectname]],
      web$dependencies
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
    # eval(string_parsed[[1]])
    # eval(string_parsed[[2]])
    purrr::map(
      string_parsed,
      eval, envir=rlang::current_env()
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

obtain_activeSourceEditorFrontmatter <- function(){
  fileX <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id=fileX$id)
  frontmatters <- rmarkdown::yaml_front_matter(fileX$path)
  return(frontmatters)
}
obtain_htmlOutputSetup <- function(){
  frontmatters <- obtain_activeSourceEditorFrontmatter()
  html_tagSetup <- parse_htmlTagSetup(frontmatters$output$html_tag)
  return(html_tagSetup)
}

parse_htmlTagSetup <- function(html_tagSetup){
  purrr::map(html_tagSetup, parse_frontmatter) -> html_tagSetup
  return(html_tagSetup)
}


update_htmloutput <- function(){
  outputSetup <-  obtain_htmlOutputSetup()
  objectname <- outputSetup$object

  .GlobalEnv$drake$loadTarget[[objectname]]()
  htmltools::save_html(
    htmltools::attachDependencies(
      .GlobalEnv[[objectname]],
      web$dependencies
    ),
    # htmltools::tagList(
    #   web$dependencies,
    #   .GlobalEnv[[objectname]]
    # ),
    file=web$output_filepath()
  )
}
