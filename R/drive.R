Drive <- function(link){
  .GlobalEnv$drive <- new.env()

  .GlobalEnv$drive$link = link

  require(dplyr)
  require(promises)

  .GlobalEnv$drive$link %>%
    get_dribble_async() %...>%
    complete_drive()
}



# helpers -----------------------------------------------------------------
complete_drive <- function(drb){

  .GlobalEnv$drive$dribble <- drb

  .GlobalEnv$drive$mimeType <- .GlobalEnv$drive$dribble$drive_resource[[1]][["mimeType"]]

  flag_folder <- stringr::str_detect(
    .GlobalEnv$drive$mimeType, "folder$"
  )

  .GlobalEnv$drive$.get_listOfFiles <- function(drbb){
    googledrive::drive_ls(drbb)
  }

  .GlobalEnv$drive$.get_listOfFiles(.GlobalEnv$drive$dribble) ->
    .GlobalEnv$drive$list_files


  .GlobalEnv$drive$.download_fileX <- function(.x){
    # .x =1
    .GlobalEnv$drive$list_files$drive_resource -> driveResources

    fileDetails <- list()
    fileDetails$filename <- driveResources[[.x]]$name
    fileDetails$creator <- driveResources[[.x]][["lastModifyingUser"]][c("displayName","emailAddress")]
    stringr::str_replace_all(
      fileDetails$creator$displayName,
      "\\s","_") -> displayName
    stringr::str_replace(
      fileDetails$filename,
      "\\.", glue::glue("_{displayName}.")) ->
      .filepath
    fileDetails$filepath <- . %//% .filepath
    googledrive::as_dribble(.GlobalEnv$drive$list_files[.x, ]) -> drbX
    googledrive::drive_download(
      drbX, path=fileDetails$filepath,
      overwrite = T
    )
    fileDetails
  }

  .GlobalEnv$drive$download <- function(){
    require(econR)
    purrr::map(
      1:nrow(.GlobalEnv$drive$list_files),
      .GlobalEnv$drive$.download_fileX
    ) -> .GlobalEnv$drive$fileDetails
  }

  .GlobalEnv$drive$download()
}

get_dribble_async <- function(link){
  promises::future_promise(
    {
      googledrive::as_dribble(link)
    }
  )
}
