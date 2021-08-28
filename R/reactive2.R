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
  .currentSource$code <- add_parenthesis2conductor(.currentSource = .currentSource, conductorTargets, pick)

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
    tryCatch(
      {assemble_serverFunctionScript(.currentSource, pick)},
      error=function(e){
        assemble_serverFunctionScript(.currentSource, pick, styler=F)
      }
    )


  return(serverFunScript)
}



# helpers -----------------------------------------------------------------

get_currentSource <- function(){
  .currentSource <<- rstudioapi::getSourceEditorContext()
  rstudioapi::documentSave(id=.currentSource$id)
}

addConductorParenthesis <- function(.code,label, conductorTargets, pick){
  # newCode <- .code
  for(.i in seq_along(conductorTargets)){
    conductorX = conductorTargets[[.i]]
    # if(conductorX=="codeContent") browser()
    # pattern: conductorX but not followed by "(" or "="
    pattern1=glue::glue("(?<![\"\'])\\b{conductorX}\\b(?!(\\(|\\s*=))")
    replacement1=glue::glue("{conductorX}()")
    pattern2=glue::glue(
      "[\"\']{conductorX}[\"\']"
    )
    replacement2= glue::glue("{conductorX}")

    pattern_replacement = c(replacement1, replacement2)
    names(pattern_replacement)=c(pattern1, pattern2)

    for(.x in seq_along(.code)){
      # print(
      #   glue::glue(
      #     '.i = {.i}; .x = {.x}'
      #   )
      # )
      # if(.i == 3L && .x == 47L) browser()

      if(is.null(.code[[.x]])
        || label[[.x]]==conductorX
        || pick$makecondition[[.x]]){
        next
      }
      .code[[.x]] <-
        codelineReplace_withExceptionWhenCertainPhraseShownInLine(
          .code[[.x]],
          pattern_replacement,
          certainPhrases = c("req")
        )
        # stringr::str_replace_all(
        #   .code[[.x]],
        #   pattern, replacement
        # )
    }
  }
  return(.code)
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
add_parenthesis2conductor <- function(.currentSource, conductorTargets=character(0), pick){
  newCode <- .currentSource$code
  if(length(conductorTargets)!=0) {
    newCode <-
      addConductorParenthesis(
        .currentSource$code,
        .currentSource$rmd$tibble$label,
        conductorTargets, pick)
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
      which(!pick$drakeF
        & !pick$makecondition
        # & !pick$reactiveT
        & (pick$render | pick$output))
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

  # take care output chunk with reactive=T
  {
    whichIsOutputButReactiveT <-
      which(!pick$drakeF
        & !pick$makecondition
        & pick$reactiveT
        & pick$output)
    labels[whichIsOutputButReactiveT] %>%
      stringr::str_extract(
        "^output_[^_]*"
      ) %>%
      na.omit()-> outputLabelsReactiveT
    outputLabelsReactiveT %>%
      stringr::str_replace(
        "^output_",
        "render_"
      ) -> correspondingRenders
  }

  outputLabels %>%
    stringr::str_replace(
      "^output_",
      "render_"
    ) %>%
    {!(. %in%
        c(renderLabels, correspondingRenders))} %>%
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

  if (length(whichCodeIsRenderX) > 1) {
    stop(glue::glue("There are more than one render functions specified for {outputLabelX}"))
  } else
    if (length(whichCodeIsRenderX) == 0) {
      c(
        newoutputAssignX,
        "{",
        newCodeX,
        "}"
      ) -> newCodeX

    } else {
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
    }
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

assemble_serverFunctionScript <- function(.currentSource, pick, styler=T)
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

  .currentSource %>%
    extract_makecondition(pick) -> .currentSource$makecondition

  c(
    "server <- function(input, output, session){",
    .currentSource$makecondition,
    serverBody,
    "}"
  ) -> serverFunScript

  if(styler){
    styler::style_text(
      serverFunScript
    ) -> serverFunScript
  }


  return(serverFunScript)

}
extract_makecondition <- function(.currentSource, pick){
  whichAreMakeCondition <-
    which(pick$makecondition
      & !pick$drakeF)

  .currentSource$code[whichAreMakeCondition] %>%
    unlist() -> .xx
  # remove input lines
  .xx %>%
    paste0(collapse = "\n") %>%
    rlang::parse_exprs() -> .xx_exprs

  .xx_exprs %>%
    purrr::map_lgl(
      ~{
        # .x=.xx_exprs[[1]]
        string_exprX <- rlang::expr_deparse(.x)
        any(stringr::str_detect(
          string_exprX, "(input_|reactiveConsole)"
        ))
      }
    ) -> whichHasInput

  .xx_exprs[!whichHasInput] -> .yy

  return(.yy)
}
codelineReplace_withExceptionWhenCertainPhraseShownInLine <- function(
  .codeX, pattern_replacement,
  certainPhrases=c("req")
){
  .exprsX <- rlang::parse_exprs(
    paste0(.codeX, collapse = "\n")
  )
  purrr::map(
    .exprsX,
    rlang::expr_deparse,
    width=200
  ) -> list_codelines
  regexCertainPhrases = paste0(
    "\\b(",
    paste0(certainPhrases, collapse="|"),
    ")\\b"
  )
  whichHasNoCertainPhrase <-
    which(purrr::map_lgl(
      list_codelines[[1]],
      ~{!stringr::str_detect(
        .x,
        regexCertainPhrases
      )}
    ))
  .code <- list_codelines[[1]]
  if(length(whichHasNoCertainPhrase)!=0){
    stringr::str_replace_all(
      .code[whichHasNoCertainPhrase],
      pattern_replacement
    ) -> .code[whichHasNoCertainPhrase]
  }
  return(.code)
}
