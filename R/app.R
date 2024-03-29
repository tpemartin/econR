initiate_app <- function(){
  # library(econR)
  library(dplyr)
  appSystem <- get_currentRmd()
  appSystem <- check_yaml(appSystem)
  appSystem %>%
    check_Rproject()

  .GlobalEnv$drake <- rmd2drake:::Drake()

  attach_appSystem2drake(appSystem)

  check_RmdType() -> rmdType

  setup_app(appSystem)

  # resolveAppDependencies()
  # attachDependencies2UIandSave2www()

  switch(
    rmdType,
    "server"={
      AppServer()
      },
    "ui"={
      .GlobalEnv$drake$source_plan()
      .GlobalEnv$drake$makePlan()

      attach_UIfrontmatterDependencies()
      AppUI()
      attachDependencies2UIandSave2www()
      message("UI building process is done.")
      },
    {stop("This is neither a server.Rmd nor ui.Rmd file")}
  )

  .GlobalEnv$app$run <- function(){
    launch_withLog(.GlobalEnv$app$appPath)
  }

  .GlobalEnv$app$log$show <- function(){
    shiny::reactlogShow()
  }

  .GlobalEnv$app$log$reset <- function(){
    shiny::reactlogReset()
  }

  flag_serverRexists <-
    file.exists(
      .GlobalEnv$app$appPath %//% "server.R"
    )
  flag_wwwFolderExists <-
    file.exists(
      file.path(
        .GlobalEnv$app$appPath, "www/index.html"
      ))
  if(flag_serverRexists && flag_wwwFolderExists) create_appR()

  create_appProject()

  # # if app completes then,
  # rstudioapi::initializeProject(.GlobalEnv$app$appPath)
  # rstudioapi::openProject(.GlobalEnv$app$appPath,T)

  .root <- (rprojroot::is_rstudio_project$make_fix_file())()
  appymlfilepath <- list.files(.root, pattern="_shiny.yml", full.names = T)

  assertthat::assert_that(
    length(appymlfilepath)==1,
    msg=glue::glue(
      "There is no _shiny.yml in {.root}"
    )
  )

  .GlobalEnv$app$merge <- app_merge(appymlfilepath)

  message("Current build is complete.")
}

create_serverFunction2 <- function(){
  assertthat::assert_that(
    exists("drake"),
    msg="There is no drake."
  )
  assertthat::assert_that(
    !is.null(drake$.planEnvironment),
    msg="Please run drake$source_plan() first."
  )

  if(is.null(.GlobalEnv$app))
  { app <- new.env()} else
  { app <- .GlobalEnv$app }

  app_update()
  filepathbasename = "server.R"

  .root <- rprojroot::is_rstudio_project$make_fix_file()

  filepath=file.path(.root(),filepathbasename)

  xfun::write_utf8(
    text=create_serverScript(),
    con=filepath
  )
  clipr::write_clip(
    create_serverScript()
  )

}

#' Shiny app server instance generator
#'
#' @return
#' @export
#'
#' @examples None
AppServer <- function(){

  assertthat::assert_that(
    exists("drake", envir=.GlobalEnv),
    msg="There is no drake in global environment. Please initiate drake.")

  .GlobalEnv$app$server$filepath <-
    .GlobalEnv$app$appPath %//% "server.R"

  updateServer_fromDrake()
  # serverText <- econR:::create_serverScript()
  serverText <- produce_serverFunctionScript()
  xfun::write_utf8(
    text=serverText,
    con=app$server$filepath
  )

  app$server$file.edit <- function(){
    file.edit(
      app$server$filepath
    )
  }

  # move supporting files
  file.copy(app$supporting_dir, app$appPath, overwrite=T, recursive = T)

  app$copy2dir <- function(from, ...){
    to = app$appPath %//% basename(from)
    file.copy(from, to, ...)
  }

  .GlobalEnv$app <- app

  message("Server building process is done.")
}


initiate_appUI <- function(){
  .GlobalEnv$drake <- rmd2drake:::Drake()
  .GlobalEnv$drake$source_plan()
  .GlobalEnv$drake$makePlan()
  econR::AppUI()
}


#' Shiny app UI instance generator
#'
#' @return
#' @export
#'
#' @examples None
AppUI <- function(){
  assertthat::assert_that(
    exists("drake", envir=.GlobalEnv),
    msg="There is no drake in global environment. Please initiate drake.")

  app$ui$rmdfilename <- .GlobalEnv$drake$activeRmd$filenames

  drake <- .GlobalEnv$drake


  # econR:::saveUIRds(.GlobalEnv$app) -> .GlobalEnv$app

  # create browsable
  .GlobalEnv$app$ui$browsable <-
    econR:::generate_uibrowsable(.GlobalEnv$app$ui$dependencies)

  .GlobalEnv$app$ui$browse_ui <- function(){
    .GlobalEnv$app$ui$browsable(ui)
  }

  # save_ui()

  .GlobalEnv$app$ui$browse_ui <- function(reload=T){
    .GlobalEnv$app$ui$browsable(ui, reload)
  }

  # web$browse <- browse_generator(web)


  .GlobalEnv$app$translate_HTML2rTags <- econR:::convertHTML2RTags

  .GlobalEnv$app$translate_HTML_fromClipboard <-
    function(){
      .GlobalEnv$app$translate_HTML2rTags(
        clipr::read_clip()
      )
    }


}

#' Create invisible action button automatically clicked to prevent timeout of an app.
#'
#' @param seconds numeric. seconds to automatically click the action button
#'
#' @return
#' @export
#'
#' @examples none
timeoutPrevention <- function(seconds=10){
  require(htmltools)
  tagList(
    shiny::actionButton("timeoutbtn", "An action button", style="display: none;"),
    tags$script(glue::glue('$(function(){$actbtn = $("#timeoutbtn");
setInterval(
  function(){
      $actbtn.click()
  }, <<seconds*1000>>)})', .open="<<", .close=">>"))
  )
}

# helpers -----------------------------------------------------------------


updateUI_fromDrake <- function(){
  .GlobalEnv$app$ui$rmdfilename <-
    .GlobalEnv$drake$activeRmd$filenames

  .GlobalEnv$drake$loadTarget[[.GlobalEnv$drake$activeRmd$frontmatter$dependencies]]()

  .GlobalEnv$app$ui$dependencies <-
    .GlobalEnv[[.GlobalEnv$drake$activeRmd$frontmatter$dependencies]]


  .GlobalEnv$app$ui_browsable <- generate_uibrowsable(.GlobalEnv$app$ui$dependencies)

}

saveUIRds <- function(app){
  drake <- .GlobalEnv$drake
  econR:::parse_frontmatter(drake$activeRmd$frontmatter$output$shiny_app$appPath) -> appPath
  econR:::parse_frontmatter(
    drake$activeRmd$frontmatter$output$shiny_app$ui
  ) -> uiname

  app$ui$Rds_filepath <- file.path(appPath, paste0(uiname,".Rds"))


  # load saving object and save
  assertthat::assert_that(
    !is.null(drake$loadTarget[[uiname]]),
    msg=glue::glue("frontmatter output object '{uiname}' does not exist in drake targets.")
  )
  drake$loadTarget[[uiname]]()
  saveRDS(
    htmltools::attachDependencies(
      .GlobalEnv[[uiname]],
      app$dependencies
    ),
    file = app$ui$Rds_filepath)

  return(app)
}
checkDependencyName <- function(dependencies){

  purrr::map_chr(
    dependencies,
    ~{ .x$name}
  )

}
generate_uibrowsable <- function(dependencies){
  function(ui, reload=T){
    tryCatch({
      sym_ui <- rlang::ensym(ui)
      if(
        is.null(.GlobalEnv[[as.character(sym_ui)]]) || reload
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
      htmltools::attachDependencies(
        ui_element,
        dependencies
      )
    )

  }
}

app_update <- function(){

  activeFile <- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id=activeFile$id)

  .GlobalEnv$drake$update()

  flag_isUIrmd <- !is.null(.GlobalEnv$drake$activeRmd$frontmatter$output$shiny_app$ui)
  assertthat::assert_that(
    flag_isUIrmd,
    msg="This is not a UI rmd"
  )

  updateUI_fromDrake()

}
updateServer_fromDrake <- function(){
  .GlobalEnv$app$server$rmdfilename <-
    .GlobalEnv$drake$activeRmd$filenames

}

get_input_output_labels <- function(.currentSource){
  unlist(.currentSource$code) %>%
    stringr::str_extract_all(
      "\\b(input|output)_[^\\s]*\\b"
    ) %>%
    unlist() -> input_output_labels

  input_output_labels_divided <-
    list(
      input=stringr::str_subset(
        input_output_labels,
        "^input"
      ) %>% unique(),
      output=stringr::str_subset(
        input_output_labels,
        "^output"
      ) %>% unique()
    )
  return(input_output_labels_divided)
}

#' Extract input output labels from the active Rmd
#'
#' @return
#' @export
#'
#' @examples None
extract_outputInputLabels <- function(){
  .currentSource <- rstudioapi::getSourceEditorContext()
  require(dplyr)
  .currentSource$rmd$rmd <-
    parsermd::parse_rmd(.currentSource$path)

  .currentSource$rmd$rmd %>%
    parsermd::rmd_node_code() ->
    .currentSource$code

  .outputInputLabels <<-
    get_input_output_labels(.currentSource)

  print(.outputInputLabels)
  return(.outputInputLabels)
}

launch_withLog <- function(appPath){
  library(shiny)
  library(reactlog)
  reactlog::reactlog_enable()

  shiny::shinyAppDir(
    appPath
  )
}

# launch_withLog <- function(appPath){
#   library(shiny)
#   library(reactlog)
#
#   # tell shiny to log all reactivity
#   reactlog_enable()
#
#   # once app has closed, display reactlog from shiny
#
#   # run a shiny app
#   # app <- system.file(package = "shiny")
#   runApp(
#     appPath
#   )
#
#
# }

