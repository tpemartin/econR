#' Convert imported gpx file's xml content to a list of longitude and latitude
#'
#' @return A list of longitude-latitude pairs
#' @export
#'
#' @examples None
gpx_as_list <- function(xml_gpx){
  xml2::as_list(xml_gpx) -> list_gpx
  list_gpx[[1]][[1]] -> branches
  purrr::map(
    branches,
    get_list_lon_lat
  ) -> list_lon_lat
}


# helpers -----------------------------------------------------------------

get_list_lon_lat <- function(sub_branches){
  purrr::map(
    sub_branches,
    ~{
      attributes(.x) -> sub_branches_i_attrs
      as.numeric(sub_branches_i_attrs[c("lon","lat")] )
    }
  ) -> list_lon_lat
  setNames(list_lon_lat, seq_along(list_lon_lat)) -> list_lon_lat
  return(list_lon_lat)
}

