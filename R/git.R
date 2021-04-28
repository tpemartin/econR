Git <- function(){
  r_gitignore <- new.env()

  # .root <- rprojroot::is_rstudio_project$make_fix_file()
  gitignore::gi_available_templates()
  gitignore::gi_fetch_templates("R") -> gi_R_template
  r_gitignore$content <- gi_R_template

  r_gitignore$ignore <- list()

  r_gitignore$ignore$drakeCache <- {function(r_gitignore){
    function(){
      no_DrakeFolder <- "
\n# drake cache starting with .
**/\\.*"
      paste0(
        r_gitignore$content,
        no_DrakeFolder
      ) -> r_gitignore$content
    }
  }}(r_gitignore)

  r_gitignore$ignore$Rdata <- {function(r_gitignore){
    function(){
      # Rdata
      no_Rdata <- "
\n# .Rdata
**/*\\.Rdata
**/*\\.Rda
**/*\\.rda
**/*\\.rdata
"
      paste0(
        r_gitignore$content,
        no_Rdata
      ) -> r_gitignore$content
    }
  }}(r_gitignore)

  r_gitignore$create <- {function(r_gitignore){
    function(){
      .root <- rprojroot::is_rstudio_project$make_fix_file()
      gitfilename <- file.path(
        .root(), ".gitignore"
      )
      xfun::write_utf8(
        r_gitignore$content, con = gitfilename
      )
      # gitignore::gi_write_gitignore(r_gitignore$content)
    }
  }}(r_gitignore)
  return(r_gitignore)
}

