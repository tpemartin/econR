addin_createUIRds <- function(){
  rmd2drake:::Drake() -> .GlobalEnv$drake
  .GlobalEnv$drake$source_plan()
  .GlobalEnv$drake$makePlan()
}

addin_createServerScript <- function(){
  rmd2drake:::Drake() -> .GlobalEnv$drake
  .GlobalEnv$drake$source_plan()
  create_serverFunction()
}

create_serverFunction <- function(){
  assertthat::assert_that(
    exists("drake"),
    msg="There is no drake."
  )
  assertthat::assert_that(
    !is.null(drake$.planEnvironment),
    msg="Please run drake$source_plan() first."
  )

  filepathbasename = basename(stringr::str_replace(drake$activeRmd$filenames, "\\.[Rr][Mm][Dd]$",".R"))

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
create_serverScript <- function(){
  filename = basename(.GlobalEnv$drake$activeRmd$filenames)
  planname =
    paste0("plan_", stringr::str_extract(filename, ".+(?=\\.)"))
  assertthat::assert_that(!is.null(.GlobalEnv$drake$.planEnvironment[[planname]]),
    msg="Drake plan does not exist. Please source_plan and makePlan.")

  drakePlan = .GlobalEnv$drake$.planEnvironment[[planname]]

  generate_reactExpressionsFromDrakePlan(drakePlan) -> reactExprs
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

#' Create Reactive script to clipboard
#'
#' @return
#' @export
#'
#' @examples None
create_reactiveClip <- function(){
  assertthat::assert_that(
    exists("drake"),
    msg="There is no drake."
  )
  assertthat::assert_that(
    !is.null(drake$.planEnvironment),
    msg="Please run drake$source_plan() first."
  )

  clipr::write_clip(
    create_reactiveScript()
  )

}

#' Create Reactive script from drake
#'
#' @return
#' @export
#'
#' @examples None
create_reactiveScript <- function(){
  filename = basename(.GlobalEnv$drake$activeRmd$filenames)
  planname =
    paste0("plan_", stringr::str_extract(filename, ".+(?=\\.)"))
  assertthat::assert_that(!is.null(.GlobalEnv$drake$.planEnvironment[[planname]]),
    msg="Drake plan does not exist. Please source_plan and makePlan.")

  drakePlan = .GlobalEnv$drake$.planEnvironment[[planname]]

  generate_reactExpressionsFromDrakePlan(drakePlan) -> reactExprs
  purrr::map_chr(
    reactExprs,
    ~{
      paste0(deparse(.x), collapse="\n")
    }
  ) -> reactScripts

  makecondition <- .GlobalEnv$drake$process2get$codes$makecondition

  reactScripts <-
    c(
      "# Input -----------\n",
      makecondition, "\n",
      "# Conductors and Output --------\n",
      reactScripts
    )

  stringr::str_replace_all(
    reactScripts,
    c("input_"= "input$",
    "output_"= "output$")
  ) -> reactScripts

  return(reactScripts)
}

generate_reactExpressionsFromDrakePlan <- function(drakePlan){
  # inputTargets <- extract_makeconditionInputs()
  # targets <- c(
  #   inputTargets,
  #   drakePlan$target)
  targets <- drakePlan$target
  whichHasValidTargets <- stringr::str_which(targets, "^render_", negate = T)
  whichHasRender <- stringr::str_which(
    targets, "^render_"
  )
  valid <- list(
    targets=targets[whichHasValidTargets],
    drakePlan=drakePlan[whichHasValidTargets,]
  )
  render <- list(
    targets=targets[whichHasRender],
    command=drakePlan$command[whichHasRender]
  )
  render <- get_renderFunctionName(render)

  list_exprs <- vector("list", length(valid$targets))
  for(.i in seq_along(valid$targets)){
    list_exprs[[.i]] <- convert2reactExpression(valid$targets[[.i]], valid$drakePlan[.i,], render)
  }
  return(list_exprs)
}

convert2reactExpression <- function(targets, drakePlanX, render){
  # drakePlanX = drakePlan[4,]
  commandX <- drakePlanX$command[[1]]
  targetX <- drakePlanX$target[[1]]

  commandX <-
    remove_duplicateTargetAssignment(targetX, commandX)
  commandX_react <-
    addReactParenthesis(targets, commandX)
  renderFun <- "reactive("
  if(stringr::str_detect(targetX, "^output_")){
    outputId = stringr::str_remove(targetX, "^output_")
    renderPattern = paste0("render_", outputId)
    if(renderPattern %in% render$targets) {
      renderFun <- paste0(render$renderFunname[[which(render$targets == renderPattern)]],'(')
    }
  }

  reactExprX <-

    rlang::parse_expr(paste0(targetX," <- ", renderFun,commandX_react,")"))

  return(reactExprX)
}

remove_duplicateTargetAssignment <- function(targetX, commandX){
  commandX_deparse <- deparse(commandX)
  if(length(commandX_deparse)==1) return(commandX)
  secondExpr <- stringr::str_remove_all(commandX_deparse[[2]],"\\s")

  pattern = paste0(targetX,"(<-|=)\\{")
  if(stringr::str_detect(secondExpr, pattern)) commandX_deparse <-
    commandX_deparse[ - c(2, length(commandX_deparse)-1)]

  pattern = paste0(targetX,"(<-|=)[^{]")
  if(stringr::str_detect(secondExpr, pattern)) commandX_deparse[[2]] <- stringr::str_replace(secondExpr, paste0(targetX,"(<-|=)"), "")
  commandX <- rlang::parse_expr(paste0(commandX_deparse, collapse="\n"))
  return(commandX)
}
addReactParenthesis <- function(targets, commandX){
  commandX_deparse <- paste0(deparse(commandX), collapse="\n")
  for(.x in targets){
    pattern = .x
    replacement = paste0(.x,"()")
    stringr::str_replace_all(commandX_deparse, pattern, replacement) -> commandX_deparse
  }
  return(commandX_deparse)
}
extract_makeconditionInputs <- function(){

  .GlobalEnv$drake$process2get$codes[["makecondition"]] -> context_makecondition

  stringr::str_extract_all(
    context_makecondition,
    "\\binput[^\\s<=]*\\b"
  ) -> list_inputs
  inputTargets <- unlist(list_inputs)

  return(inputTargets)
}
get_renderFunctionName <- function(render){
  purrr::map(
    seq_along(render$targets),
    ~{
      deparse(render$command[[.x]])[[2]] -> renderDefinition
      stringr::str_remove_all(renderDefinition,"\\s") -> renderDefinition
      stringr::str_remove(renderDefinition,
        paste0(render$targets[[.x]], "(<-|=)")) ->
        renderFun
      renderFun
    }
  ) -> list_renderFun
  render$renderFunname = list_renderFun
  return(render)
}
