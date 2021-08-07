app_merge <- function(appymlfilepath){
  assertthat::assert_that(
    length(appymlfilepath) ==1 && file.exists(appymlfilepath),
    msg="Proper _shiny.yml file does not exist."
  )
  function(){

    yaml::read_yaml(appymlfilepath) -> .GlobalEnv$app$yaml
    .GlobalEnv$app$rmdfolder <- dirname(appymlfilepath)

    .GlobalEnv$app$yaml$merge_files$ui %>%
      purrr::map_chr(
        ~{
          .GlobalEnv$app$rmdfolder %//% .x
        }
      ) -> ui_rmd2merge
    .GlobalEnv$app$yaml$merge_files$server %>%
      purrr::map_chr(
        ~{
          .GlobalEnv$app$rmdfolder %//% .x
        }
      ) -> server_rmd2merge


    ui_rmd2merge %>%
      get_ui_mergedRmdlines() -> ui_mergedRmdlines

    server_rmd2merge %>%
      get_server_mergedRmdlines() ->
      server_mergedRmdlines

    mergedRmdFilepath_ui <-
      .GlobalEnv$app$rmdfolder %//% "ui_merged.Rmd"
    mergedRmdFilepath_server <-
      .GlobalEnv$app$rmdfolder %//% "server_merged.Rmd"

    xfun::write_utf8(
      ui_mergedRmdlines,
      con=mergedRmdFilepath_ui
    )
    xfun::write_utf8(
      server_mergedRmdlines,
      con=mergedRmdFilepath_server
    )

    .GlobalEnv$app$merged_uiOpen <- function(){
      file.edit(mergedRmdFilepath_ui)
    }
    .GlobalEnv$app$merged_serverOpen <- function(){
      file.edit(mergedRmdFilepath_server)
    }
  }

}
get_ui_mergedRmdlines <- function(rmd2merge){
  rmd2merge |>
    extractContentTargets() -> list_contentTargets
  contentTargets <- list_contentTargets$contentTargets
  content_mergerFchunks <- list_contentTargets$content_mergerFchunks

  cacheName <- "mergedUI"

  mergedRmdlines <-
    c("---",
      glue::glue("drake_cache: \".{cacheName}\""),
      "dependencies: 'dependencies'",
      glue::glue("dependencyRscriptFilePath: \"{.GlobalEnv$app$yaml$dependencyRscriptFilePath}\""),
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

get_server_mergedRmdlines <- function(rmd2merge){
  rmd2merge |>
    extractContentTargets() -> list_contentTargets
  contentTargets <- list_contentTargets$contentTargets
  content_mergerFchunks <- list_contentTargets$content_mergerFchunks

  cacheName <- "mergedServer"

  mergedRmdlines <-
    c("---",
      glue::glue("drake_cache: \".{cacheName}\""),
      "---",
      "",
      contentTargets)
  return(mergedRmdlines)
}
