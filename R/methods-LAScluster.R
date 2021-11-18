LAScluster <- function(center, width, height, buffer, shape, files, name, crs = sf::NA_crs_, index = LIDRDEFAULTINDEX)
{
  return(new("LAScluster", center, width, height, buffer, shape, files, name, "", crs, index))
}
