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
    "server"={AppServer()},
    "ui"={
      .GlobalEnv$drake$source_plan()
      .GlobalEnv$drake$makePlan()

      attach_UIfrontmatterDependencies()
      AppUI()
      attachDependencies2UIandSave2www()
      },
    {stop("This is neither a server.Rmd nor ui.Rmd file")}
  )
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

  .r <- rprojroot::is_rstudio_project$make_fix_file()
  appymlfilepath <- list.files(.r(), pattern="_shiny.yml", full.names = T)

  .GlobalEnv$app$merge <- app_merge(appymlfilepath)
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
