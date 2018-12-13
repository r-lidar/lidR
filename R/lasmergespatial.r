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

#' Merge a point cloud with a source of spatial data
#'
#' Merge a point cloud with a source of spatial data. It adds an attribute along each point based on
#' a value found in the spatial data. Sources of spatial data can be a \code{SpatialPolygonsDataFrame})
#' or a \code{RasterLayer}.\cr
#' \itemize{
#' \item{\code{SpatialPolygonsDataFrame}: it checks if the points belongs within each polygon. If
#' the parameter \code{attribute} is the name of an attribute in the table of attributes of the shapefile,
#' it assigns to the points the values of that attribute. Otherwise it classifies the points as boolean.
#' TRUE if the points are in a polygon, FALSE otherwise.}
#' \item{\code{RasterLayer}: it attributes to each point the value found in each pixel of the \code{RasterLayer}}.
#' \item{\code{RasterStack} or \code{RasterBrick} must have 3 channels for RGB colors. It colorizes the
#' point cloud with RGB values.}
#' }
#'
#' @param las An object of class \code{LAS}
#' @param source An object of class \code{SpatialPolygonsDataFrame} or \code{RasterLayer} or a
#' \code{RasterStack} or \code{RasterBrick} with RGB colors.
#' @param attribute character. The name of an attribute in the table of attributes of the shapefile or
#' the name of a new column in the LAS object. Not relevant for RGB colorization.
#'
#' @return An object of the class \code{LAS}.
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' shp     <- system.file("extdata", "lake_polygons_UTM17.shp", package = "lidR")
#'
#' las   <- readLAS(LASfile)
#' lakes <- shapefile(shp)
#'
#' # The attribute "inlake" does not exist in the shapefile.
#' # Points are classified as TRUE if in a polygon
#' las    <- lasmergespatial(las, lakes, "inlakes")     # New attribute 'inlakes' is added.
#' forest <- lasfilter(las, inlakes == FALSE)
#' plot(las)
#' plot(forest)
#'
#' # The attribute "LAKENAME_1" exists in the shapefile.
#' # Points are classified with the values of the polygons
#' las <- lasmergespatial(las, lakes, "LAKENAME_1")     # New column 'LAKENAME_1' is added.
lasmergespatial = function(las, source, attribute = NULL)
{
  UseMethod("lasmergespatial", las)
}

#' @export
lasmergespatial.LAS = function(las, source, attribute = NULL)
{
  if (is(source, "SpatialPolygonsDataFrame"))
    values = lasmergeSpatialPolygonDataFrame(las, source, attribute)
  else if (is(source, "RasterLayer"))
    values = lasmergeRasterLayer(las, source)
  else if (is(source, "RasterStack") | is(source, "RasterBrick"))
    return(lasmergergb(las, source))
  else
    stop("No method for this source format.")

  if (is.null(attribute))
    attribute = "id"

  las = lasadddata(las, values, attribute)
  return(las)
}

lasmergergb = function(las, source)
{
  . <- X <- Y <- NULL

  R <- source[[1]]@data@values
  G <- source[[2]]@data@values
  B <- source[[3]]@data@values

  maxr <- max(R, na.rm = TRUE)
  maxg <- max(G, na.rm = TRUE)
  maxb <- max(B, na.rm = TRUE)

  scale <- 1
  if (maxr <= 255 & maxg <= 255 & maxb <= 255)
    scale <- 257

  cells <- raster::cellFromXY(source[[1]], coordinates(las))

  las@data$R <- as.integer(R[cells]*scale)
  las@data$G <- as.integer(G[cells]*scale)
  las@data$B <- as.integer(B[cells]*scale)

  format <- las@header@PHB$`Point Data Format ID`

  if (format %in% c(2,3,8))
  {
    # nothing to do
  }
  else if ("NIR" %in% names(las@data))
  {
    format <- 8L
  }
  else if ("gpstime" %in% names(las@data))
  {
    format <- 3L
  }
  else
  {
    format <- 2L
  }

  las@header@PHB$`Point Data Format ID` <- format

  return(las)
}

lasmergeSpatialPolygonDataFrame = function(las, shapefile, attribute = NULL)
{
  npoints <- nrow(las@data)

  # No attribute is provided: assign the number of the polygon
  if (is.null(attribute))
  {
    method <- 0
  }
  # The attribute is the name of an attribute in the attribute table: assign the value of the attribute
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
      stop(glue::glue("The attribute {attribute} the in the table of attribute is not of a supported type."))
  }
  # The attribute is not the name of an attribute in the attribute table: assign a boolean value if the point is in a polygon or
  # not.
  else
  {
    method <- 2
    values <- logical(npoints)
  }

  # Crop the shapefile to minimize the computations removing out-of-bounds polygons
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
    verbose("No polygon found within the data")
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

lasmergeRasterLayer = function(las, raster)
{
  cells <- raster::cellFromXY(raster, coordinates(las))
  return(raster@data@values[cells])
}

