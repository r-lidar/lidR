is.overlapping = function(catalog)
{
  contour       <- rgeos::gUnaryUnion(catalog)
  actual_area   <- round(contour@polygons[[1]]@area, 4)
  measured_area <- round(area(catalog), 4)
  return(actual_area < measured_area)
}


is.indexed = function(catalog)
{
  laxfiles <- paste0(tools::file_path_sans_ext(catalog@data$filename), ".lax")
  return(!any(!file.exists(laxfiles)))
}
