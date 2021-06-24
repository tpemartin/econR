initiate_app <- function(){
  .GlobalEnv$drake <- rmd2drake:::Drake()
  .GlobalEnv$drake$source_plan()
  .GlobalEnv$drake$makePlan()
  flag_serverRmd <-
    is.null(.GlobalEnv$drake$activeRmd$frontmatter$output$shiny_app$ui)
  if(flag_serverRmd){
    econR::AppServer()
  } else {
    econR::AppUI()
  }
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

  if(is.null(.GlobalEnv$app))
  { app <- new.env()} else
  { app <- .GlobalEnv$app }

  .GlobalEnv$drake$update()

  flag_isUIrmd <- !is.null(.GlobalEnv$drake$activeRmd$frontmatter$output$shiny_app$ui)
  assertthat::assert_that(
    !flag_isUIrmd,
    msg="This a UI rmd, not server rmd"
  )
  app$directory <- econR:::parse_frontmatter( .GlobalEnv$drake$activeRmd$frontmatter$output$shiny_app$dirpath)
  app$server$filepath <-
    app$directory %//% "server.R"

  app$support_dir <- econR:::parse_frontmatter(
    .GlobalEnv$drake$activeRmd$frontmatter$output$shiny_app
    $supporting_dir
  )

  updateServer_fromDrake()

  xfun::write_utf8(
    text=econR:::create_serverScript(),
    con=app$server$filepath
  )

  app$server$file.edit <- function(){
    file.edit(
      app$server$filepath
    )
  }

  # move supporting files
  from = app$support_dir
  to = app$directory
  file.copy(
    from, to, overwrite=T, recursive=T
  )

  app$copy2dir <- function(from, ...){
    to = app$directory %//% basename(from)
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

  if(is.null(.GlobalEnv$app))
  { app <- new.env()} else
  { app <- .GlobalEnv$app }

  app$ui$rmdfilename <- .GlobalEnv$drake$activeRmd$filenames

  drake <- .GlobalEnv$drake

  flag_nofrontmatterDependencies <-
    is.null(drake$activeRmd$frontmatter$dependencies)
  if(flag_nofrontmatterDependencies) warning("no dependencies: 'dep_object_name' set in frontmatter.\n app$ui_browsable will have no default dependency.")

  # attach htmlDependencies and browsable
  if(!flag_nofrontmatterDependencies){
    # throw dependencies to .GlobalEnv
    drake$loadTarget[[drake$activeRmd$frontmatter$dependencies]]()
    app$ui$dependencies <-
      .GlobalEnv[[drake$activeRmd$frontmatter$dependencies]]


    app$ui_browsable <- generate_uibrowsable(app$ui$dependencies)

  }

  # assertthat::assert_that(
  #   exists("drake", envir=.GlobalEnv),
  #   msg="There is no drake in global environment. Please initiate drake."
  # )

  saveUIRds(app) -> app

  app$browse_ui <- function(reload=T){
    app$ui_browsable(ui, reload)
  }

  # web$browse <- browse_generator(web)


  app$translate_HTML2rTags <- econR:::convertHTML2RTags

  app$translate_HTML_fromClipboard <- econR:::translate_HTML_fromClipboard(app)

  app$ui$update <- function(){

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

  temphtml_filepath <- file.path(
    app$directory,"temp.html")
  .GlobalEnv$drake$loadTarget$dependencies()
  htmltools::save_html(
    htmltools::attachDependencies(ui, dependencies),
    file=temphtml_filepath
  )
  file.remove(temphtml_filepath)

  .GlobalEnv$app <- app
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
  econR:::parse_frontmatter(drake$activeRmd$frontmatter$output$shiny_app$dirpath) -> dirpath
  econR:::parse_frontmatter(
    drake$activeRmd$frontmatter$output$shiny_app$ui
  ) -> uiname

  app$ui$Rds_filepath <- file.path(dirpath, paste0(uiname,".Rds"))


  # load saving object and save
  assertthat::assert_that(
    !is.null(drake$loadTarget[[uiname]]),
    msg=glue::glue("frontmatter output object '{uiname}' does not exist in drake targets.")
  )
  drake$loadTarget[[uiname]]()
  saveRDS(
    htmltools::attachDependencies(
      .GlobalEnv[[uiname]],
      app$ui$dependencies
    ),
    file = app$ui$Rds_filepath)

  return(app)
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


  .GlobalEnv$app$server$dependencies <-
    .GlobalEnv[[.GlobalEnv$drake$activeRmd$frontmatter$dependencies]]


  .GlobalEnv$app$server_browsable <- econR:::generate_uibrowsable(.GlobalEnv$app$server$dependencies)

}
