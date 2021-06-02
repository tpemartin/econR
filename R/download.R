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
  destfolder=destfolder){
  googledrive::as_dribble(googleSharedLink) -> drb
  googledrive::drive_ls(drb) -> allFiles
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

