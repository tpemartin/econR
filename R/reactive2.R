#' Generate server function script from active Rmd
#'
#' @return
#' @export
#'
#' @examples none
produce_serverFunctionScript <- function(){
  .currentSource <- rstudioapi::getSourceEditorContext()
  require(dplyr)
  .currentSource$rmd$rmd <-
    parsermd::parse_rmd(.currentSource$path)
  .currentSource$rmd$tibble <-
    parsermd::as_tibble(
      .currentSource$rmd$rmd
    )

  .currentSource$rmd$tibble %>%
    parsermd::rmd_get_options() ->
    .currentSource$options


  .currentSource$rmd$rmd %>%
    parsermd::rmd_node_code() ->
    .currentSource$code

  .currentSource$rmd$tibble %>%
    pull(label) %>%
    stringr::str_subset("^((render|output|input)_|makecondition)", negate=T) %>%
    stringr::str_subset("") ->
    conductorTargets

  pick <- generate_picks(.currentSource)

  ## modify conductor to conductor()
  .currentSource$code <- add_parenthesis2conductor(.currentSource = .currentSource, conductorTargets)

  ## add reactive(...)
  .currentSource$code <- add_reactive(.currentSource, pick)

  ## change input_id to input$id
  .currentSource$code <- replaceInputDashWithDollar(.currentSource, pick)

  ## change output_id to output$id
  .currentSource$output_render_labels <-
    validate_render_output_labels(.currentSource, pick)

  .currentSource$code <-
    processOutputChunk(.currentSource, pick)

  serverFunScript <-
    assemble_serverFunctionScript(.currentSource, pick)

  return(serverFunScript)
}



# helpers -----------------------------------------------------------------

get_currentSource <- function(){
  .currentSource <<- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id=.currentSource$id)
}

addConductorParenthesis <- function(code,label, conductorTargets){
  for(conductorX in conductorTargets){
    conductorX
    pattern=glue::glue("{conductorX}(?!\\()")
    replacement=glue::glue("{conductorX}()")
    for(.x in seq_along(code)){
      if(is.null(code[[.x]]) || label[[.x]]==conductorX){
        next
      }
      code[[.x]] <-
        stringr::str_replace_all(
          code[[.x]],
          pattern, replacement
        )
    }
    return(code)
  }
}
generate_picks <- function(.currentSource){
  pick <- list()
  # drake=F
  pick$drakeF <- {
    .currentSource$options %>%
      purrr::map_lgl(
        ~{
          any(
            !is.null(.x$drake) &&
              stringr::str_detect(.x$drake,"F")
          )
        }
      )
  }

  pick$reactiveT <- {
    .currentSource$options %>%
      purrr::map_lgl(
        ~{
          any(
            !is.null(.x$reactive) &&
              stringr::str_detect(.x$reactive,"T")
          )
        }
      )
  }

  pick$makecondition <- {
    .currentSource$rmd$tibble$label %>%
      {!is.na(.) & stringr::str_detect(
        ., "^makecondition"
      )}
  }
  pick$render <- {
    .currentSource$rmd$tibble$label %>%
      {!is.na(.) & stringr::str_detect(
        ., "^render"
      )}
  }
  pick$input <- {
    .currentSource$rmd$tibble$label %>%
      {!is.na(.) & stringr::str_detect(
        ., "^input_"
      )}
  }
  pick$output <- {
    .currentSource$rmd$tibble$label %>%
      {!is.na(.) & stringr::str_detect(
        ., "^output"
      )}
  }
  pick$nolabel <- {
    .currentSource$rmd$tibble$label %>%
      {is.na(.) | . == ""}
  }
  pick$isCodeNULL <- {
    .currentSource$code %>%
      {is.null(.)}
  }
  return(pick)

}
add_parenthesis2conductor <- function(.currentSource, conductorTargets=character(0)){
  newCode <- .currentSource$code
  if(length(conductorTargets)!=0) {
    .currentSource$code %>%
      addConductorParenthesis(.currentSource$rmd$tibble$label, conductorTargets) -> newCode
  }
  return(newCode)
}


moveLabelOut_addReactiveParenthesis <- function(codeX,labelX) {
  ## remove label <-
  pattern <- glue::glue("{labelX}\\s*<-\\s*")
  whichFitsPattern <- stringr::str_which(
    codeX, pattern
  )
  newCodeX <- codeX

  if (length(whichFitsPattern) != 0) {
    patternX <- stringr::str_extract(
      newCodeX[[whichFitsPattern[[1]]]],
      pattern
    )
    stringr::str_remove(
      newCodeX[[whichFitsPattern[[1]]]],
      pattern
    ) -> newCodeX[[whichFitsPattern[[1]]]]
    newCodeX <- c(
      patternX,
      "reactive({", newCodeX, "})"
    )
  }
  return(newCodeX)
}
add_reactive <- function(.currentSource, pick)
{
  whichNeeds2MoveLabelOut <-
    which(!pick$drakeF & !pick$reactiveT & !pick$render & !pick$makecondition & !pick$nolabel & !pick$output & !pick$input)
  codeNew <- .currentSource$code

  if(length(whichNeeds2MoveLabelOut) !=0 ){
    purrr::map(
      whichNeeds2MoveLabelOut,
      ~moveLabelOut_addReactiveParenthesis(
        codeX = .currentSource$code[[.x]],
        labelX = .currentSource$rmd$tibble$label[[.x]]
      )
    ) -> codeNew[whichNeeds2MoveLabelOut]
    .currentSource$code <- codeNew
  }
  return(codeNew)
}

replaceInputDashWithDollar <- function(.currentSource, pick)
{
  whichNeeds2ChangeInput <-
    which(!pick$drakeF & !pick$render & !pick$makecondition & !pick$isCodeNULL)

  codeNew <- .currentSource$code
  if(length(whichNeeds2ChangeInput) !=0 ){
    purrr::map(
      whichNeeds2ChangeInput,
      ~{
        # .x=10
        codeX = .currentSource$code[[.x]]
        inputLabels <-
          unlist(stringr::str_extract_all(
            codeX,
            "\\binput_([:alnum:]*|[_\\.]*)"
          ))
        replacements <-
          stringr::str_replace(
            inputLabels,
            "^input_",
            "input$"
          )
        if(length(inputLabels) !=0){
          for(.i in seq_along(inputLabels)){
            label.i = inputLabels[[.i]]
            replacement.i = replacements[[.i]]
            stringr::str_replace_all(
              codeX,
              glue::glue("\\b{label.i}\\b"),
              replacement.i
            ) -> codeX
          }
        }

        codeX
      }
    ) -> codeNew[whichNeeds2ChangeInput]
  }
  return(codeNew)
}

validate_render_output_labels <- function(.currentSource, pick)
{
  .currentSource$rmd$tibble$label -> labels
  {
    whichIsRenderOrOutput <-
      which(!pick$drakeF & !pick$makecondition &
          (pick$render | pick$output))
  }
  labels[whichIsRenderOrOutput] %>%
    stringr::str_extract(
      "^output_[^_]*"
    ) %>%
    na.omit()-> outputLabels

  labels[whichIsRenderOrOutput] %>%
    stringr::str_extract(
      "^render_[^_]*"
    ) %>%
    na.omit()-> renderLabels

  outputLabels %>%
    stringr::str_replace(
      "^output_",
      "render_"
    ) %>%
    {!(. %in% renderLabels)} %>%
    which() -> whichOutputHasNoRender

  assertthat::assert_that(
    length(whichOutputHasNoRender)==0,
    msg={
      paste0(outputLabels[whichOutputHasNoRender], collapse = ", ") -> problemLabels
      glue::glue("{problemLabels} has no render chunk specified.")
    })

  render_output_labels = list(
    render=renderLabels,
    output=outputLabels
  )

  return(render_output_labels)
}

processOutputChunkX <- function(outputLabelX, codeX, .currentSource, pick) {

  # move output_ label out
  ## remove label <-
  pattern <- glue::glue("{outputLabelX}\\s*<-\\s*")
  whichFitsPattern <- stringr::str_which(
    codeX, pattern
  )
  newCodeX <- codeX

  if (length(whichFitsPattern) != 0) {
    patternX <- stringr::str_extract(
      newCodeX[[whichFitsPattern[[1]]]],
      pattern
    )
    stringr::str_remove(
      newCodeX[[whichFitsPattern[[1]]]],
      pattern
    ) -> newCodeX[[whichFitsPattern[[1]]]]
  }

  newoutputLabelX <-
    stringr::str_replace(
      outputLabelX, "^output_", "output$"
    )
  newoutputAssignX <- paste0(newoutputLabelX, " <- ")

  renderLabelX <-
    stringr::str_replace(
      outputLabelX, "^output_", "render_"
    )

  whichCodeIsRenderX <- which({
    (.currentSource$rmd$tibble$label == renderLabelX & !is.na(.currentSource$rmd$tibble$label == renderLabelX)) &
      !pick$drakeF &
      !pick$makecondition
  })

  if (length(whichCodeIsRenderX) != 1) {
    stop(glue::glue("There are more than one render functions specified for {outputLabelX}"))
  }

  .currentSource$code[[whichCodeIsRenderX]] %>%
    stringr::str_extract(
      "(?<=(<-)).*"
    ) -> reactFunname

  c(
    newoutputAssignX,
    paste0(reactFunname, "({"),
    newCodeX,
    "})"
  ) -> newCodeX
  return(newCodeX)
}
processOutputChunk <- function(.currentSource, pick) {
  newCode <- .currentSource$code
  for (.x in seq_along(.currentSource$output_render_labels$output)) {
    outputLabelX <- .currentSource$output_render_labels$output[[.x]]
    whichCodeIsOutputX <- which({
      (.currentSource$rmd$tibble$label == outputLabelX & !is.na(.currentSource$rmd$tibble$label == outputLabelX)) &
        !pick$drakeF &
        !pick$makecondition
    })

    codeX <- .currentSource$code[[whichCodeIsOutputX]]

    newCode[[whichCodeIsOutputX]] <- processOutputChunkX(outputLabelX, codeX, .currentSource, pick)
  }
  return(newCode)
}

assemble_serverFunctionScript <- function(.currentSource, pick)
{
  whichAreServerCode <-
    which(
      !pick$drakeF
      & !pick$makecondition
      & !pick$render
      & !pick$isCodeNULL
    )

  .currentSource$code[whichAreServerCode] %>%
    unlist() -> serverBody

  c(
    "server <- function(input, output, session){",
    serverBody,
    "}"
  ) -> serverFunScript

  styler::style_text(
    serverFunScript
  ) -> serverFunScript

  return(serverFunScript)

}
