
# ##
# imgpath = "img"
# web$img$set_image_path("img")
# web$img$set_target_image("voice_on.png")
# web$img$show()
# web$img$scale(200)

## helpers
set_image_path <- function(web){
  function(imgpath){
    .root <- rprojroot::is_rstudio_project$make_fix_file()
    imgpath <- file.path(.root(), imgpath)
    web$img$path <- imgpath
    imgfiles <- list.files(imgpath)
    imgfiles_full <- list.files(imgpath, full.names = T)
    names(imgfiles_full) <- imgfiles
    web$img$list <- imgfiles_full
    web$img$set_target_image <- function(imgfilename){
      web$img$target <- imgfilename
      web$img$image <- magick::image_read(web$img$list[imgfilename])
      web$img$show <- function(){
        web$img$image
      }
      web$img$info <- function(){
        imageX_info <- magick::image_info(web$img$image)
        imageX_info
      }
      web$img$scale <- function(width){
        geometry = magick::geometry_size_pixels(width=width)

        imageNew <- magick::image_scale(web$img$image, width)
        newnameX <- stringr::str_extract(web$img$target, ".+(?=\\.)")
        extensionX <- stringr::str_extract(web$img$target, "(?<=\\.).+$")
        imageX_scaled <- magick::image_scale(web$img$image, geometry)
        imageX_scaled_info <- magick::image_info(imageX_scaled)

        imageX_newName <-
          paste0(
            newnameX,
            paste0(imageX_scaled_info$width, "x", imageX_scaled_info$height),
            ".",
            extensionX
          )
        magick::image_write(imageX_scaled, path=
                              file.path(web$img$path,imageX_newName))

      }
    }

  }
}





