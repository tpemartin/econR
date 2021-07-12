web_merge <- function(){
  rmdfilepath <- .GlobalEnv$web$rmdfilename
  rmdfolderpath <- dirname(rmdfilepath)
  webyml <- list.files(rmdfolderpath, pattern="_web.yml", full.names = T)
  if(file.exists(webyml)){
    yaml::read_yaml(webyml) -> webyml
    webyml$rmdfiles |>
      purrr::map_chr(
        ~{
          rmdfolderpath %//% .x
        }
      ) -> rmd2merge
  }

  rmd2merge |>
    get_mergedRmdlines(webyml) -> mergedRmdlines

  mergedRmdFilepath <-
    rmdfolderpath %//% webyml$mergedRmd

  xfun::write_utf8(
    mergedRmdlines,
    con=mergedRmdFilepath
  )
}


# helpers -----------------------------------------------------------------

get_mergedRmdlines <- function(rmd2merge, webyml){
  rmd2merge |>
    purrr::map(
      extract_merge_target_chunks
    ) ->
    list_extracted_merge_target

  list_extracted_merge_target |>
    get_uniqueShareTChunks() -> list_uniqueShareT

  list_extracted_merge_target |>
    purrr::map(
      ~.x$targets
    ) -> list_allTargets

  list_uniqueShareT |>
    purrr::map_chr(
      ~paste(c(.x, ""), collapse = "\n")
    ) -> content_mergerFchunks

  contentTargets <- vector("character", length(list_allTargets))
  for(.x in seq_along(list_allTargets)){
    targetsX <- list_allTargets[[.x]]
    targetsX |>
      purrr::map_chr(
        ~paste(.x, collapse = "\n")
      ) -> contentTargetX

    paste(
      c(
        glue::glue("\n## {basename(rmd2merge[[.x]])}"),
        contentTargetX), collapse = "\n\n") ->
      contentTargets[[.x]]
  }

  webyml$mergedRmd |>
    stringr::str_extract("[^\\.]+") -> cacheName
  mergedRmdlines <-
    c("---",
      glue::glue("drake_cache: \".{cacheName}\""),
      "dependencies: 'dependencies'",
      "output:",
      "  html_tag:",
      glue::glue("    filename: \"{cacheName}.html\""),
      "    dirpath: \"`r library(econR); . %//% 'docs'`\"",
      "    object: \"tempTag\"",
      "---",
      "",
      content_mergerFchunks,
      "",
      "```{r tempTag}",
      "tempTag <- tags$div()",
      "```",
      "",
      contentTargets)
  return(mergedRmdlines)
}


get_uniqueShareTChunks <- function(list_extracted_merge_target){
  list_extracted_merge_target |>
    purrr::detect(function(.x){ length(.x$shareT) !=0} ) ->
    firstHasShareT

  list_uniqueShareT <- firstHasShareT$shareT
  return(list_uniqueShareT)
}
extract_merge_target_chunks <- function(rmd2mergeX){
  # .x =2
  # rmd2merge[[.x]]
  rmd2mergeX |>
    xfun::read_utf8() -> rmdlines
    rmd2drake:::drake_get_rmdlinesTable(rmdlines) ->
    rmdlinesTableX

    pick_drakeT <- pick_whichNotDrakeF(rmdlinesTableX)
    pick_targetChunks <- {
      pick_whichHasTargets(rmdlinesTableX) &
        pick_drakeT
    }
    pick_shareT <- {
      pick_whichHasShareTrue(rmdlinesTableX) &
        pick_drakeT
    }

    get_listShareTrueCodeChunks(pick_shareT, rmdlines, rmdlinesTableX) -> list_shareTchunks
    get_listTargetChunks(pick_targetChunks, pick_shareT, rmdlines, rmdlinesTableX) -> list_targetChunks
  list(
    shareT = list_shareTchunks,
    targets = list_targetChunks
  )
}
pick_whichNotDrakeF <- function(rmdlinesTableX){
  purrr::map_lgl(
    rmdlinesTableX$engine_label_option,
    ~{
      # .x = rmdlinesTableX$engine_label_option[[7]]
      .x <- stringr::str_remove_all(.x, "\\s")
      all(stringr::str_detect(
        .x,
        "drake\\=F", negate = T))
    }
  )
}
pick_whichHasTargets <- function(rmdlinesTableX){
  rmdlinesTableX$engine_label_option |>
    purrr::map_lgl(
      ~{
        any(
          .x != "\\br\\b" & stringr::str_detect(.x, "=", negate = T)
        )
      }
    )
}
pick_whichHasShareTrue <- function(rmdlinesTableX){
  rmdlinesTableX$engine_label_option |>
    purrr::map_lgl(
      ~{
        any(
          .x != "r" & stringr::str_detect(.x, "share\\s*=\\s*T")
        )
      }
    )
}
get_listShareTrueCodeChunks <- function(pick_shareT, rmdlines, rmdlinesTableX){
  output <- list()
  which_shareT <- which(pick_shareT)
  if(length(which_shareT)!=0){
    purrr::map(
      which_shareT,
      ~{
        with(rmdlinesTableX, start[[.x]]:end[[.x]]) -> lineRange
        rmdlines[lineRange]
      }
    ) -> output
  }
  return(output)
}

get_listTargetChunks <- function(pick_targetChunks, pick_shareT, rmdlines, rmdlinesTableX){
  output <- list()
  which_target <- which(pick_targetChunks & !pick_shareT)
  if(length(which_target)!=0){
    purrr::map(
      which_target,
      ~{
        with(rmdlinesTableX, start[[.x]]:end[[.x]]) -> lineRange
        rmdlines[lineRange]
      }
    ) -> output
  }
  return(output)
}
