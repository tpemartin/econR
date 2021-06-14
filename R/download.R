internalData <- function(){
  Download = list(
    googleShareLink=googleLink_download
    # dropboxLink=NA
  )
}


# helpers -----------------------------------------------------------------

#' Download Google drive shared link to a destination folder
#'
#' @param googleSharedLink A shared link
#' @param destfolder A destination folder path
#'
#' @return
#' @export
#'
#' @examples none
googleLink_download <- function(googleSharedLink,
  destfolder=NULL){
  googledrive::as_dribble(googleSharedLink) -> drb
  if(
    drb$drive_resource[[1]]$kind=="drive#file"
  ){
    allFiles <- drb
  } else {
    googledrive::drive_ls(drb) -> allFiles
  }

  if(is.null(destfolder)){
    .root <- rprojroot::is_rstudio_project$make_fix_file()
    destfolder = .root()
  }
  if(!dir.exists(destfolder)) dir.create(destfolder)

  purrr::walk(
    1:nrow(allFiles),
    ~{
      fileX = allFiles[.x, ]
      googledrive::drive_download(
        file = allFiles[.x, ],
        path = file.path(destfolder, fileX$name),
        overwrite = T
      )
    }
  )

}

