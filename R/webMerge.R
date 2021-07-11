web_merge <- function(){
  rmdfilepath <- .GlobalEnv$web$rmdfilename
  rmdfolderpath <- dirname(rmdfilepath)
  webyml <- list.files(rmdfolderpath, pattern="\\.yml$", full.names = T)
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
    get_uniqueMergeFChunks() -> list_uniqueMergeF

  list_extracted_merge_target |>
    purrr::map(
      ~.x$targets
    ) -> list_allTargets

  list_uniqueMergeF |>
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


get_uniqueMergeFChunks <- function(list_extracted_merge_target){
  list_extracted_merge_target |>
    purrr::detect(function(.x){ length(.x$mergeF) !=0} ) ->
    firstHasMergeF

  list_uniqueMergeF <- firstHasMergeF$mergeF
  return(list_uniqueMergeF)
}
extract_merge_target_chunks <- function(rmd2mergeX){
  # .x =2
  # rmd2merge[[.x]]
  rmd2mergeX |>
    xfun::read_utf8() -> rmdlines
    rmd2drake:::drake_get_rmdlinesTable(rmdlines) ->
    rmdlinesTableX

    pick_targetChunks <- pick_whichHasTargets(rmdlinesTableX)
    pick_mergeF <- pick_whichHasMergeFalse(rmdlinesTableX)

    get_listMergeFalseCodeChunks(pick_mergeF, rmdlines, rmdlinesTableX) -> list_mergeFchunks
    get_listTargetChunks(pick_targetChunks, pick_mergeF, rmdlines, rmdlinesTableX) -> list_targetChunks
  list(
    mergeF = list_mergeFchunks,
    targets = list_targetChunks
  )
}
pick_whichHasTargets <- function(rmdlinesTableX){
  rmdlinesTableX$engine_label_option |>
    purrr::map_lgl(
      ~{
        any(
          .x != "r" & stringr::str_detect(.x, "=", negate = T) & stringr::str_detect(.x, "drake\\s*=\\s*F", negate=T)
        )
      }
    )
}
pick_whichHasMergeFalse <- function(rmdlinesTableX){
  rmdlinesTableX$engine_label_option |>
    purrr::map_lgl(
      ~{
        any(
          .x != "r" & stringr::str_detect(.x, "merge\\s*=\\s*F") & stringr::str_detect(.x, "drake\\s*=\\s*F", negate=T)
        )
      }
    )
}
get_listMergeFalseCodeChunks <- function(pick_mergeF, rmdlines, rmdlinesTableX){
  output <- list()
  which_mergeF <- which(pick_mergeF)
  if(length(which_mergeF)!=0){
    purrr::map(
      which_mergeF,
      ~{
        with(rmdlinesTableX, start[[.x]]:end[[.x]]) -> lineRange
        rmdlines[lineRange]
      }
    ) -> output
  }
  return(output)
}

get_listTargetChunks <- function(pick_targetChunks, pick_mergeF, rmdlines, rmdlinesTableX){
  output <- list()
  which_target <- which(pick_targetChunks & !pick_mergeF)
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
