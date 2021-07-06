#' Extract py scripts from Rmd
#'
#' @return
#' @export
#'
#' @examples None
extractPy <- function(){
  assertthat::assert_that(
    require(rmd2drake),
    msg="Please remotes::install_github('tpemartin/rmd2drake')")

  .GlobalEnv$drake <- rmd2drake:::Drake()
  require(dplyr)
  drake$activeRmd$codeChunkTable$engine_label_option %>%
    stringr::str_which("^py") %>%
    drake$activeRmd$codeChunkTable[., ] -> pyTable

  # .x=1
  purrr::map(
    seq_along(pyTable$start),
    ~{
      startPosition <- pyTable$start[[.x]]+1
      endPosition <- pyTable$end[[.x]]-1
      startPosition:endPosition
    }
  ) %>%
    unlist() -> whichArePyCodes
  stringr::str_replace(
    drake$activeRmd$filenames,
    "\\.[Rr][Mm][Dd]$",
    ".py") -> pyfilepath
  c(
    drake$activeRmd$lines[whichArePyCodes], "") %>%
    xfun::write_utf8(
      con = pyfilepath
    )
}
