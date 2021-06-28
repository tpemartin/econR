create_appProject <- function(appSystem){
    # flag_hasRproject <-
    list.files(.GlobalEnv$app$appPath) -> pathFiles

    flag_hasRproject <-
      any(stringr::str_detect(
        pathFiles, stringr::regex(".Rproj", ignore_case=T)
      ))

    if(!flag_hasRproject){
      rstudioapi::initializeProject(
        .GlobalEnv$app$appPath
      )
      .GlobalEnv$app$launchProject <- function(){
        rstudioapi::openProject(
          .GlobalEnv$app$appPath, T
        )
      }
    }

}


create_serverScript2 <- function(){
  filename = basename(.GlobalEnv$drake$activeRmd$filenames)
  planname =
    paste0("plan_", stringr::str_extract(filename, ".+(?=\\.)"))
  assertthat::assert_that(!is.null(.GlobalEnv$drake$.planEnvironment[[planname]]),
    msg="Drake plan does not exist. Please source_plan and makePlan.")

  drakePlan = .GlobalEnv$drake$.planEnvironment[[planname]]
  #
  econR:::generate_reactExpressionsFromDrakePlan(drakePlan) -> reactExprs
  purrr::map_chr(
    reactExprs,
    ~{
      paste0(deparse(.x), collapse="\n")
    }
  ) -> reactScripts

  makecondition <- .GlobalEnv$drake$process2get$codes$makecondition

  stringr::str_subset(
    makecondition, "input_", negate = T
  ) -> makeconditionServer
  serverScripts <-
    c(
      "# makecondition -----------\n",
      makeconditionServer, "\n",
      "# server --------\n",
      "server <- function(input, output){",
      reactScripts,
      "}"
    )

  stringr::str_replace_all(
    serverScripts,
    c("input_"= "input$",
      "output_"= "output$")
  ) -> serverScripts

  return(serverScripts)
}

save_ui <- function(){
  .GlobalEnv$drake$loadTarget$ui()
  htmltools::save_html(
    htmltools::attachDependencies(
      ui,
      .GlobalEnv$app$dependencies, append=T
    ),
    file= .GlobalEnv$app$appPath %//% "www" %//% "index.html"
  )

}
attachShinyDependencies <- function(){
  # shiny:::shinyDependencies() -> shinyDeps
  # shinyDeps[[2]] -> ultimateShinyDeps
  shiny::fluidPage() %>% findDependencies() -> ultimateShinyDeps

  append(
    .GlobalEnv$app$dependencies, list(ultimateShinyDeps)
  ) -> allDeps
  .GlobalEnv$app$dependencies <- allDeps
  .GlobalEnv$app[["ui"]]$dependencies <- allDeps

}
resolveAppDependencies <- function(){
  plan_attrs <- get_drakePlannameAndPlanType()
  planName = plan_attrs$planname
  planType = plan_attrs$plantype

  flag_hasIndividualDependencies <- !is.null(.GlobalEnv$drake$activeRmd$frontmatter$dependencies)

  if(flag_hasIndividualDependencies)  get_individualDependencies(planType)

  if(planType=="ui"){
    if(.GlobalEnv$app$yaml$dependencies!="shiny") get_appDependenciesFromUIRmd()

    get_appDependenciesFrom_ui_object()
    # attachShinyDependencies()
  }

}

get_appDependenciesFrom_ui_object <- function(){
  .GlobalEnv$drake$loadTarget$ui()
  htmltools::findDependencies(ui) -> uiDeps

  currentDeps <- .GlobalEnv[[.GlobalEnv$app$yaml$dependencies]]

  newDeps <-
    append(
      list(uiDeps), currentDeps
    )

  .GlobalEnv[[.GlobalEnv$app$yaml$dependencies]] <- newDeps
  .GlobalEnv$app$yaml$dependencies <- newDeps

}
get_appDependenciesFromUIRmd <- function(){
  .GlobalEnv$drake$loadTarget[[
    .GlobalEnv$app$yaml$dependencies
  ]]()
  .GlobalEnv$app$dependencies <-
    .GlobalEnv[[.GlobalEnv$app$yaml$dependencies]]
}

get_individualDependencies <- function(planType){
  assertthat::assert_that(
    !is.null(.GlobalEnv$drake$activeRmd$frontmatter$dependencies),
    msg="No individual dependencies in Rmd"
  )
  .GlobalEnv$drake$loadTarget[[
    .GlobalEnv$drake$activeRmd$frontmatter$dependencies
  ]]()
  .GlobalEnv$app[[planType]]$dependencies <-
    .GlobalEnv[[.GlobalEnv$drake$activeRmd$frontmatter$dependencies]]

}
get_drakePlannameAndPlanType <- function(){
  .GlobalEnv$drake$.planEnvironment %>% names() -> names_planEnv
  purrr::map_lgl(
    names_planEnv,
    ~is.data.frame(
      .GlobalEnv$drake$.planEnvironment[[.x]]
    )
  ) %>% which() -> whichIsPlanName
  planName <- names_planEnv[[whichIsPlanName]]
  planType <- stringr::str_extract(planName, "(server|ui)")
  list(
    planname = planName,
    plantype = planType
  )
}





check_isHtmlDependencies <- function(dependencies){
  all(purrr::map_lgl(dependencies, htmltools:::is_html_dependency))
}
setup_app <- function(appSystem){
  if(is.null(.GlobalEnv$app)) app <<- new.env()

  .GlobalEnv$app$yaml <- appSystem$yamlContent

  .GlobalEnv$app$appPath <- appSystem$yamlContent$appPath
  .GlobalEnv$app$supporting_dir <-
    appSystem$yamlContent$supporting_dir

}
check_RmdType <- function(){
  basename(.GlobalEnv$drake$activeRmd$filenames) %>%
    stringr::str_extract(
      "^(server|ui)"
    ) -> rmdType

  return(rmdType)
}
attach_appSystem2drake <- function(appSystem){
  drake <- .GlobalEnv$drake
  drake$activeRmd$frontmatter$output$shiny_app <-
    appSystem$yamlContent
  drake <<- drake
}

check_Rproject <- function(appSystem){
  # flag_hasRproject <-
  list.files(appSystem$folder) -> folderFiles
  flag_hasRproject <-
    any(stringr::str_detect(
      folderFiles, stringr::regex(".Rproj", ignore_case=T)
    ))

  if(!flag_hasRproject){
    rstudioapi::showDialog(
      "No Rproj setup yet",
      "Will create one and relaunch as a R project"
    )
    rstudioapi::initializeProject(
      appSystem$folder
    )
    rstudioapi::openProject(
      appSystem$folder
    )
  }

}

get_currentRmd <- function(){
  rstudioapi::getSourceEditorContext() -> currentRmd
  flag_isRmd <- stringr::str_detect(currentRmd$path, stringr::regex(".Rmd", ignore_case = T))

  assertthat::assert_that(
    flag_isRmd ,
    msg = "This is not an Rmd file."
  )
  appSystem <-
    list(
      folder=dirname(currentRmd$path),
      currentRmd = currentRmd[c('id','path')]
    )
  return(appSystem)
}
check_yaml <- function(appSystem){
  appSystem$folder %//% "_shiny.yml" ->
    appSystem$yaml
  flag_yamlExists <-
    file.exists(
      appSystem$yaml
    )
  assertthat::assert_that(
    flag_yamlExists,
    msg = message(appSystem$yaml, "\ndoes not exist")
  )
  yaml::yaml.load(
    xfun::read_utf8(
      con=appSystem$yaml
    )
  ) -> yamlContent

  purrr::map(
    yamlContent,
    econR:::parse_frontmatter
  ) -> appSystem$yamlContent

  return(appSystem)
}
attachDependencies2UIandSave2www <- function(){
  .GlobalEnv$drake$loadTarget$ui()

  uiDependencies <- htmltools::findDependencies(ui)

  shinyDeps <- htmltools::htmlDependency(
    name="shiny",
    version="1.0.0",
    src=c(href="shared"),
    script="shiny.min.js",
    stylesheet = "shiny.min.css"
  )
  econR::html_dependencies() -> econRdeps

  tagList(
    econRdeps$jquery(),
    shinyDeps, ui
  ) -> uiNew

  htmltools::save_html(
    uiNew,
    file= .GlobalEnv$app$appPath %//% "www/index.html")


}

create_appR <- function(){
  rlines <-
    xfun::read_utf8(
      .GlobalEnv$app$appPath %//% "server.R"
    )

  rlines %>%
    c(
      "
      shinyApp(
  ui=shiny::htmlTemplate('www/index.html'),
  server
)
"
    ) -> appLines

xfun::write_utf8(
  styler::style_text(appLines),
  .GlobalEnv$app$appPath %//% "app.R"
)
}
