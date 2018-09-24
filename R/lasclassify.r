# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016 Jean-Romain Roussel
#
# This file is part of lidR R package.
#
# lidR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>
#
# ===============================================================================

#' Classify points from a source of spatial data
#'
#' Classify points based on spatial data from external sources. It adds an attribute
#' along each point based on a value found in the spatial data. External sources can be a
#' \code{SpatialPolygonsDataFrame}) or a \code{RasterLayer}.\cr
#' \itemize{
#' \item{\code{SpatialPolygonsDataFrame}: it checks if the points belong within each polygons. If
#' the parameter \code{attribute} is the name of a an attribute in the table of attributes of the shapefile
#' it assigns to the points the values of that attribute. Otherwise it classifies the points as boolean.
#' TRUE if the points are in a polygon, FALSE otherwise.}
#' \item{\code{RasterLayer}: it attributes to each point the value found in each pixel of the \code{RasterLayer}}.
#' }
#'
#' @param las An object of the class \code{LAS}
#'
#' @param source An object of class \code{SpatialPolygonsDataFrame} or \code{RasterLayer}
#'
#' @param attribute characters. The name of a attribute in the table of attributes of the shapefile or
#' the name of a new column in the LAS object.
#'
#' @return An object of the class \code{LAS}
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' shapefile_dir <- system.file("extdata", package = "lidR")
#'
#' las   <- readLAS(LASfile)
#' lakes <- rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")
#'
#' # The attribute "inlake" does not exist in the shapefile.
#' # Points are classified as TRUE if in a polygon
#' las    <- lasclassify(las, lakes, "inlakes")     # New attribut 'inlakes' is added.
#' forest <- lasfilter(las, inlakes == FALSE)
#' plot(las)
#' plot(forest)
#'
#' # The attribute "LAKENAME_1" exists in the shapefile.
#' # Points are classified with the values of the polygons
#' las <- lasclassify(las, lakes, "LAKENAME_1")     # New column 'LAKENAME_1' is added.
lasclassify = function(las, source, attribute = NULL)
{
  stopifnotlas(las)

  if (is(source, "SpatialPolygonsDataFrame"))
    values = classify_from_shapefile(las, source, attribute)
  else if (is(source, "RasterLayer") | is(source, "RasterStack"))
    values = classify_from_rasterlayer(las, source)
  else
    stop("No method for this source format.", call. = F)

  if (is.null(attribute))
    attribute = "id"

  las@data[[attribute]] <- values
  return(las)
}

classify_from_shapefile = function(las, shapefile, attribute = NULL)
{
  info    <- NULL
  npoints <- nrow(las@data)

  # No attribute is provided: assign the number of the polygon
  if (is.null(attribute))
  {
    method <- 0
  }
  # The attribute is the name of a attribute in the attribute table: assign the value of the attribute
  else if (attribute %in% names(shapefile@data))
  {
    method <- 1
    data   <- shapefile@data[[attribute]]

    if (class(data) == "factor")
      values = factor(rep(NA_integer_, npoints), levels = levels(shapefile@data[,attribute]))
    else if (class(data) == "integer")
      values = rep(NA_integer_, npoints)
    else if (class(data) == "logical")
      values = rep(NA, npoints)
    else if (class(data) == "numeric")
      values = rep(NA_real_, npoints)
    else if (class(data) == "character")
      values = rep(NA_character_, npoints)
    else
      stop(glue::glue("The attribute {attribute} the in the table of attribute is not of a supported type."), call. = FALSE)
  }
  # The attribute is not the name of a attribute in the attribute table: assign a boolean if the point is in a polygon or not.
  else
  {
    method <- 2
    values <- logical(npoints)
  }

  # Crop the shapefile to minimize the computations removing out of bounds polygons
  if (raster::extent(shapefile) >  2*raster::extent(las))
  {
    verbose("Croping the shapefile...")
    polys <- raster::crop(shapefile, raster::extent(las)*1.01)
  }
  else
    polys <- shapefile

  # No polygon? Return NA or false depending on the method used
  if (is.null(polys))
  {
    verbose("No polygon found within the data", call. = F)
    return(values)
  }

  verbose("Analysing the polygons...")

  sfgeom <- sf::st_as_sf(polys)

  verbose("Testing whether points fall in a given polygon...")

  ids <- rep(0L, npoints)
  for (i in 1:length(sfgeom$geometry))
  {
    wkt          <- sf::st_as_text(sfgeom$geometry[i])
    in_poly      <- C_points_in_polygon_wkt(las@data$X, las@data$Y, wkt)
    ids[in_poly] <- i
  }

  if (method == 1)
  {
    values[ids] <- polys@data[, attribute][ids]
    verbose(glue::glue("Assigned the value of attribute {attribute} from the table of attibutes to the points"))
  }
  else if (method == 2)
  {
    values <- ids > 0
    verbose("Assigned a boolean value to the points")
  }
  else
  {
    values <- ifelse(ids == 0L, NA_integer_, ids)
    verbose("Assigned a number to each individual polygon")
  }

  return(values)
}

classify_from_rasterlayer = function(las, raster)
{
  . <- X <- Y <- info <- NULL

  #xres = raster::res(raster)[1]
  #xmin = raster@extent@xmin
  #ymin = raster@extent@ymin
  #m  = raster::as.matrix(raster)
  #v = fast_extract(m, las@data$X, las@data$Y, xmin, ymin, xres) # 15 times faster than raster::extract + much memory effcient
  cells = raster::cellFromXY(raster, las@data[,.(X,Y)])
  return(raster@data@values[cells])
}

