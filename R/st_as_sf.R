#' @importFrom sf st_as_sf
NULL

#' @export
st_as_sf.LAS = function(x, ...) { return(sf::st_as_sf(x@data, coords = c("X", "Y", "Z"), crs = st_crs(x))) }

#' @export
st_as_sf.LAScatalog = function(x, ...) { return(x@data) }
