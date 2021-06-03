#' Extract py scripts from Rmd
#'
#' @return
#' @export
#'
#' @examples None
extractPy <- function(){
  rmd <- new.env()
  rmd$activeRmd <- list()
  rstudioapi::getSourceEditorContext() -> content

  # rmd$activeRmd$filenames <- .activeFile
  rmd$activeRmd$filenames <- content$path
  rmd$activeRmd$lines <-
    xfun::read_utf8(rmd$activeRmd$filenames)

  stringr::str_which(rmd$activeRmd$lines, "^```") -> whichStartWith3ticks
  stringr::str_detect(rmd$activeRmd$lines[whichStartWith3ticks], "^```\\{python") -> pick_pythonStart
  stringr::str_detect(rmd$activeRmd$lines[whichStartWith3ticks], "purl\\s*=\\s*F", negate=T) -> pick_noPurlF
  index_pythonStart <- which(pick_pythonStart & pick_noPurlF)
  index_pythonEnd <- index_pythonStart+1
  data.frame(
    start=whichStartWith3ticks[index_pythonStart],
    end=whichStartWith3ticks[index_pythonEnd]
  ) -> codeTable

  purrr::map(
    1:nrow(codeTable),
    ~{
      start = codeTable$start[[.x]]+1
      end = codeTable$end[[.x]]-1
      rmd$activeRmd$lines[start:end]
    }
  ) -> list_pyLines

  .root <- rprojroot::is_rstudio_project$make_fix_file()
  purlfolder <-
    file.path(
      .root(),"purl"
    )

  # create purl folder
  if(!dir.exists(purlfolder)) dir.create(purlfolder)

  scriptfilepath <-
    file.path(
      purlfolder,
      basename(stringr::str_replace(
        content$path,
        "\\.[Rr][Mm][Dd]$",
        ".py"
      ))
    )

  xfun::write_utf8(
    unlist(list_pyLines),
    con=scriptfilepath
  )
  message(
    scriptfilepath
  )

}
