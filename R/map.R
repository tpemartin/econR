#' Convert sf dataframe all polygons into multipolygons
#'
#' @description sf with geometry of the mixture of polygons and multipolygons can not be used in plotly add_sf directly. This function makes sure all geometries are multipolygons that plotly can deal wtih.
#' @param sf_data A sf dataframe
#'
#' @return
#' @export
#'
#' @examples None
convert_poly_multipoly_into_multipolyOnly <- function(sf_data){
  for(i in seq_along(sf_data$geometry)){
    if(!is(sf_data$geometry[[i]],"MULTIPOLYGON")){
      sf::st_coordinates(sf_data$geometry[[i]]) -> polyCoord
      L1_unique <- unique(polyCoord[,"L1"])
      list_polygon <- list()
      for(x in L1_unique){
        pickL1X <- which(polyCoord[,"L1"]==x)
        polyCoord[pickL1X, c("X","Y")] -> XYCoords
        list_polygon <- append(list_polygon, list(XYCoords))
      }
      sf::st_multipolygon(
        list(
          list_polygon
        )
      ) -> multipolygonX
      sf_data$geometry[[i]] <- multipolygonX
    }
  }
  sf_data$geometry <- sf::st_sfc(sf_data$geometry)
  return(sf_data)
}
#' A fixture to OSM simple feature date name error problem
#' @description A quick fix when 'Error in do.call(rbind, x) : variable names are limited to 10000 bytes' happens.
#' @param sf_object An sf object
#'
#' @return
#' @export
#'
#' @examples none
osm_geom_rename <- function(sf_object){
  require(sf)
  require(dplyr)
  sf_object %>%
    st_geometry() -> sfc_sf_object
  for(i in seq_along(sfc_sf_object)){
    names(sfc_sf_object[[i]][[1]]) <-
      1:length(names(sfc_sf_object[[i]][[1]]))
  }

  sf_object %>%
    st_set_geometry(
      sfc_sf_object
    ) -> sf_object2
  return(sf_object2)
}
