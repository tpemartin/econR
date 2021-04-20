web_translateJsChunk2RChunk <- function(web){
  function(){
    .GlobalEnv$drake$update()
    require(dplyr)
    list_newCommand <- get_listJsTargetCommand()
    jsAttachmentComplete <- translate_jsChunk2RChunk(list_newCommand)

    xfun::read_utf8(web$rmdfilename) -> rmdlines

    jsBreaks <- c(
      stringr::str_which(rmdlines, "^## JS")-1,
      stringr::str_which(rmdlines, "^<!--JS end-->")
    )

    if(length(jsBreaks) == 0){
      c(
        rmdlines,
        "\n",
        jsAttachmentComplete,
        ""
      ) -> newRmdlines
      paste0(newRmdlines, collapse = "\n") -> newRmdlines2write
      xfun::write_utf8(
        newRmdlines2write, con=web$rmdfilename
      )
    } else if (length(jsBreaks) == 2) {
      summary_rmdlines <- update_jsSection(rmdlines, jsBreaks, jsAttachmentComplete)
      xfun::write_utf8(
        summary_rmdlines$line, con=web$rmdfilename
      )
    } else {
      stop("length(jsBreaks) is neither 0 nor 2.")
    }
  }
}


## helpers

get_listJsTargetCommand <- function(){
  pickJs <- purrr::map_lgl(
    .GlobalEnv$drake[["activeRmd"]][["autopsy"]][["head_info"]],
    ~{.x[[1]]=="js"}
  )
  pickWeb = purrr::map_lgl(
    .GlobalEnv$drake[["activeRmd"]][["autopsy"]][["head_info"]],
    ~{
      !any(stringr::str_detect(stringr::str_remove_all(., "\\s"), "web=F"))
    }
  )
  pickJsWeb <- pickJs & pickWeb

  whichIsJsWeb <- which(pickJsWeb)

  jsTargetNames <-
    {
      purrr::map(whichIsJsWeb,
                 ~{
                   # .x =8
                   otherHeadInfoX <- .GlobalEnv$drake[["activeRmd"]][["autopsy"]][["head_info"]][[.x]][-1]
                   otherHeadInfoX_notOption <- stringr::str_subset(otherHeadInfoX, "=", negate=T)
                   if(length(otherHeadInfoX_notOption) == 0 ) next
                   otherHeadInfoX_notOption[[1]]
                 })
    }

  purrr::map(
    seq_along(whichIsJsWeb),
    ~{
      stringr::str_replace_all(
        .GlobalEnv$drake$activeRmd$autopsy$content[whichIsJsWeb[[.x]]][[1]],
        "^```.*",
        "\"") -> newContent
      newCommand <- paste0(
        jsTargetNames[[.x]], " <- ",
        paste0(newContent, collapse = "\n"), collapse = "\n"
      )
      newCommand
    }
  ) -> list_newCommand
  setNames(list_newCommand, jsTargetNames) -> list_newCommand
  return(list_newCommand)
}
translate_jsChunk2RChunk <- function(list_newCommand){
  names_list_newCommand <- names(list_newCommand)
  list_jsAttachment <- purrr::map(
    names_list_newCommand,
    ~{
      paste0("```{r ", .x,"}\n") -> chunkStart
      paste0(
        chunkStart,
        list_newCommand[[.x]],
        "```\n",
        collapse="\n"
      ) -> completeChunk
      completeChunk
    }
  )

  list_jsAttachment %>%
    unlist() %>%
    paste0(collapse = "\n") -> jsAttachmentContent

  c("## JS\n",
    jsAttachmentContent,
    "<!--JS end-->") %>%
    paste0(collapse = "\n") -> jsAttachmentComplete

  return(jsAttachmentComplete)
}
update_jsSection <- function(rmdlines, jsBreaks, jsAttachmentComplete){
  cut(seq_along(rmdlines),
      breaks = c(0, jsBreaks, Inf))
  jsLevelName <- paste0("(",paste0(jsBreaks, collapse = ","),"]")
  # jsLevelName
  df_rmdlines <-
    data.frame(
      lines = rmdlines,
      .cut = cut(seq_along(rmdlines),
                 breaks = c(0, jsBreaks, Inf))
    )
  df_rmdlines %>%
    group_by(
      .cut
    ) %>%
    summarise(
      line = paste0(lines, collapse = "\n")
    ) %>% ungroup() -> summary_rmdlines

  summary_rmdlines$line[[
    which(summary_rmdlines$.cut == jsLevelName)
  ]] <- jsAttachmentComplete
  return(summary_rmdlines)
}