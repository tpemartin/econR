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
create_serverScript <- function(){
  filename = basename(.GlobalEnv$drake$activeRmd$filenames)
  planname =
    paste0("plan_", stringr::str_extract(filename, ".+(?=\\.)"))
  assertthat::assert_that(!is.null(.GlobalEnv$drake$.planEnvironment[[planname]]),
    msg="Drake plan does not exist. Please source_plan and makePlan.")

  drakePlan = .GlobalEnv$drake$.planEnvironment[[planname]]
  #
  generate_reactExpressionsFromDrakePlan2(drakePlan) -> reactExprs
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
      "server <- function(input, output, session){",
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

  generate_reactExpressionsFromDrakePlan2(drakePlan) -> reactExprs
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
  conductorTargets <-
    stringr::str_subset(
      valid$targets, "^(input|output)_", negate=T
    )

  list_exprs <- vector("list", length(valid$targets))
  for(.i in seq_along(valid$targets)){

    list_exprs[[.i]] <- convert2reactExpression(valid$targets[[.i]], valid$drakePlan[.i,], render, conductorTargets)
  }
  return(list_exprs)
}

convert2reactExpression <- function(targets, drakePlanX, render, conductorTargets){
  # drakePlanX = drakePlan[4,]
  commandX <- drakePlanX$command[[1]]
  targetX <- drakePlanX$target[[1]]

  commandX_react <-
    remove_duplicateTargetAssignment(targetX, commandX)

  if(length(conductorTargets) !=0){
    commandX_react <-
      addReactParenthesis(conductorTargets, commandX_react)
  }

  renderFun <- "reactive"
  if(stringr::str_detect(targetX, "^output_")){
    outputId = stringr::str_remove(targetX, "^output_")
    renderPattern = paste0("render_", outputId)
    if(renderPattern %in% render$targets) {
      renderFun <- render$renderFunname[[which(render$targets == renderPattern)]]
    }
  }

  renderFunExpr <- rlang::parse_expr(renderFun)
  if(is.character(commandX_react)) commandX_react = rlang::parse_expr(commandX_react)
  commandX_reactExpr <-
  rlang::expr((!!renderFunExpr)(!!commandX_react))

  targetXExpr <- rlang::parse_expr(targetX)
  reactExprX <- expr(
    !!targetXExpr <- !!commandX_reactExpr
  )

  return(reactExprX)
}

remove_duplicateTargetAssignment <- function(targetX, commandX){
  commandX <- removeOuterBracket(commandX)


  commandX_deparse <- deparse(commandX)
  commandX_deparse_targetRemoved <-
    removeTargetAssignment(targetX,
      commandX_deparse)


  # pattern_targetAssignment <-
  #   paste0(targetX, "\\s*(<-|=)\\s*")
  # whichHasTargetAssignment <-
  #   stringr::str_which(commandX_deparse, pattern_targetAssignment)
  #
  # if(length(whichHasTargetAssignment)!=0){
  #   commandX_deparse_targetRemoved <-
  #     commandX_deparse[
  #       -c(whichHasTargetAssignment,
  #         length(commandX_deparse)-whichHasTargetAssignment+1)
  #     ]
  # }

  flag_notStartWithBracket <-
    stringr::str_detect(
      commandX_deparse_targetRemoved[[1]],
      "^\\{", negate=T)
  if(flag_notStartWithBracket){
    commandX_deparse_targetRemoved <-
      c("{", commandX_deparse_targetRemoved, "}")
  }
  commandX <- rlang::parse_expr(paste0(commandX_deparse_targetRemoved, collapse="\n"))
  # # 如果只有一行,
  # if(length(commandX_deparse)==1) return(commandX)
  # secondExpr <- stringr::str_remove_all(commandX_deparse[[2]],"\\s")
  #
  # pattern = stringr::regex(paste0(targetX,"(<-|=)\\{"), ignore_case=T)
  #
  # if(stringr::str_detect(secondExpr, pattern)) commandX_deparse <-
  #   commandX_deparse[ - c(1, length(commandX_deparse))]
  #
  # pattern = paste0(targetX,"(<-|=)[^{]*")
  # if(stringr::str_detect(secondExpr, pattern)) commandX_deparse[[2]] <- stringr::str_replace(secondExpr, paste0(targetX,"(<-|=)"), "")
  # commandX <- rlang::parse_expr(paste0(commandX_deparse, collapse="\n"))
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
      deparse(render$command[[.x]]) -> renderDefinition
      unlist(stringr::str_remove_all(renderDefinition,"[\\s{}]")) -> renderDefinition
      unlist(stringr::str_remove(renderDefinition,
        paste0(render$targets[[.x]], "(<-|=)"))) -> renderDefinition
      stringr::str_subset(renderDefinition, stringr::boundary("word")) ->
        renderFun
      renderFun
    }
  ) -> list_renderFun
  render$renderFunname = list_renderFun
  return(render)
}
removeOuterBracket <- function(commandX){
  # commandX <- rlang::expr({ 2})
  commandX_deparse <- deparse(commandX)
  if(commandX_deparse[[1]]=="{") commandX_deparse <- commandX_deparse[-c(1, length(commandX_deparse))]
  rlang::parse_expr(
    paste0(commandX_deparse, collapse="\n")
  )
}
removeTargetAssignment <- function(targetX, commandX_deparse) {
  pattern_targetAssignmentNoBracket <-
    paste0(targetX, "\\s*(<-|=)\\s*(?!\\{)")
  whichHasTargetAssignmentNoBracket <- stringr::str_which(
    commandX_deparse,
    pattern_targetAssignmentNoBracket
  )
  pattern_targetAssignmentWithBracket <-
    paste0(targetX, "\\s*(<-|=)\\s*(?=\\{)")
  whichHasTargetAssignmentWithBracket <- stringr::str_which(
    commandX_deparse,
    pattern_targetAssignmentWithBracket
  )

  if (length(whichHasTargetAssignmentWithBracket) != 0) {
    commandX_deparse <- commandX_deparse[
      -c(1, length(commandX_deparse))
    ]
  } else
    if (length(whichHasTargetAssignmentNoBracket) != 0) {
      commandX_deparse[whichHasTargetAssignmentNoBracket] <-
        stringr::str_remove(
          commandX_deparse[whichHasTargetAssignmentNoBracket],
          paste0(targetX, "\\s*(<-|=)\\s*")
        )
    }
  return(commandX_deparse)
}

# helper  generate_reactExpressionsFromDrakePlan2-----------------------------------------------------------------


generate_reactExpressionsFromDrakePlan2 <- function(drakePlan){
  drakePlan_reactiveDivided <-
    reactiveDivide_drakePlan(drakePlan)

  drakePlan_reactiveFalse <-
    drakePlan_reactiveDivided$reactiveFalse

  list_exprs_reactiveFalse <-
    generate_reactExpressionsFromDrakePlan_reactiveFalse(drakePlan_reactiveFalse = drakePlan_reactiveFalse)

  list_exprs_reactiveTrue <-
    generate_reactExpressionsFromDrakePlan_reactiveTrue(drakePlan, drakePlan_reactiveDivided)

  reactExprs <- append(list_exprs_reactiveFalse,
    list_exprs_reactiveTrue)

  return(reactExprs)
}
generate_reactExpressionsFromDrakePlan_reactiveFalse <- function(drakePlan_reactiveFalse){
  drakePlan <- drakePlan_reactiveFalse
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
  conductorTargets <-
    stringr::str_subset(
      valid$targets, "^(input|output)_", negate=T
    )

  list_exprs <- vector("list", length(valid$targets))
  for(.i in seq_along(valid$targets)){

    list_exprs[[.i]] <- convert2reactExpression(valid$targets[[.i]], valid$drakePlan[.i,], render, conductorTargets)
  }
  return(list_exprs)
}

generate_reactExpressionsFromDrakePlan_reactiveTrue <- function(drakenPlan, drakePlan_reactiveDivided){
  drakePlan_reactiveFalse <- drakePlan_reactiveDivided$reactiveFalse
  drakePlan_reactiveTrue <- drakePlan_reactiveDivided$reactiveTrue

  validTargets <- drakePlan_reactiveFalse$target
  conductorTargets <-
    stringr::str_subset(
      validTargets, "^(input|output|render)_", negate=T
    )

  commands_reactiveTrue <- drakePlan_reactiveTrue$command

  commands_reactiveTrue <- addParenthesis2conductors(commands_reactiveTrue, conductorTargets)

  return(commands_reactiveTrue)
}

addParenthesis2conductors <- function(commands_react,conductorTargets){
  list_exprs <- commands_react
  if(length(conductorTargets) !=0){
    for(.x in seq_along(commands_react)){
      commandX_react = commands_react[[.x]]
      # add () to conductor in command

      list_exprs[[.x]] <-
        addReactParenthesis(conductorTargets, commandX_react)
    }
  }
  return(list_exprs)
}

reactiveDivide_drakePlan <- function(drakePlan){
  whichHasReactiveT <- get_whichHasReactiveT()

  pick_reactiveT <- rep(F, length(drakePlan$target))

  if(length(whichHasReactiveT)!=0){

    rmdlinesTable_reactiveT <- .GlobalEnv$drake$activeRmd$codeChunkTable[whichHasReactiveT, ]

    rmdlinesTable_reactiveT$engine_label_option %>%
      get_targetsFromEngine_label_option() ->
      targets_reactiveT

    pick_reactiveT <- drakePlan$target %in% targets_reactiveT
  }
  whichPlanTargetReactiveTrue <- which(pick_reactiveT)
  whichPlanTargetReadtiveFalse <- which(!pick_reactiveT)

  list(
    reactiveTrue = drakePlan[whichPlanTargetReactiveTrue, ],
    reactiveFalse = drakePlan[whichPlanTargetReadtiveFalse, ]
  ) -> drakePlan_reactiveDivided
  return(drakePlan_reactiveDivided)
}
get_targetsFromEngine_label_option <- function(engine_label_option){
  engine_label_option %>%
    purrr::map_chr(
      ~{
        # .x = rmdlinesTable_reactiveT$engine_label_option[[1]]
        which(!(.x %in% c("r","python"))  & stringr::str_detect(.x, "=", negate=T))-> whichIsTargetName
        .x[[whichIsTargetName]]
      }
    ) -> targetnames
  return(targetnames)
}
get_whichHasReactiveT <- function(){
  .GlobalEnv$drake$activeRmd$codeChunkTable$engine_label_option %>%
    purrr::map_lgl(
      ~{
        stringr::str_replace_all(.x, "\\s","") -> .xNoSpace
        any(stringr::str_which(.xNoSpace, "reactive=T")) -> flag_hasReactiveT
        flag_hasReactiveT
      }
    ) %>%
    which() -> whichHasReactiveT
  return(whichHasReactiveT)
}
