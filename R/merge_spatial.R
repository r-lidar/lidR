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
#' a value found in the spatial data. Sources of spatial data can be a \code{SpatialPolygons*})
#' , a \code{sf data.frame} or a \code{Raster*}.\cr
#' \itemize{
#' \item{\code{SpatialPolygons*, sf}: it checks if the points belongs within each polygon. If
#' the parameter \code{attribute} is the name of an attribute in the table of attributes it assigns
#' to the points the values of that attribute. Otherwise it classifies the points as boolean.
#' TRUE if the points are in a polygon, FALSE otherwise.}
#' \item{\code{RasterLayer}: it attributes to each point the value found in each pixel of the
#' \code{RasterLayer}}.
#' \item{\code{RasterStack} or \code{RasterBrick} must have 3 channels for RGB colors. It colorizes the
#' point cloud with RGB values.}
#' }
#'
#' @param las An object of class \code{LAS}
#' @param source An object of class \code{SpatialPolygons*} or \code{sf} or \code{RasterLayer} or a
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
#' las    <- merge_spatial(las, lakes, "inlakes")     # New attribute 'inlakes' is added.
#' forest <- filter_poi(las, inlakes == FALSE)
#' plot(las)
#' plot(forest)
#'
#' # The attribute "LAKENAME_1" exists in the shapefile.
#' # Points are classified with the values of the polygons
#' las <- merge_spatial(las, lakes, "LAKENAME_1")     # New column 'LAKENAME_1' is added.
merge_spatial = function(las, source, attribute = NULL)
{
  UseMethod("merge_spatial", las)
}

#' @export
merge_spatial.LAS = function(las, source, attribute = NULL)
{
  if (is(source, "sf"))
  {
    if (sf::st_geometry_type(source) != "POLYGON")
      stop("Only POLYGON geometry types are supported for sf objects", call. = FALSE)

    box <- sf::st_bbox(las)
    width <- (box[3] - box[1])*0.01 + las@header@PHB[["X scale factor"]]
    height <- (box[4] - box[2])*0.01 + las@header@PHB[["Y scale factor"]]
    box <- box + c(-width, -height, width, height)
    attr(box, "crs") <- sf::st_crs(source) # fix for code before rgdal 1.5-8 (probably)
    sf::st_agr(source) = "constant"
    source <- sf::st_crop(source, box)
  }

  if (is(source, "SpatialPolygons") && !is(source, "SpatialPolygonsDataFrame"))
    attribute <- NULL

  if (is(source, "Polygon"))
    source <- sp::Polygons(list(source), ID = 1)

  if (is(source, "Polygons"))
    source <- sp::SpatialPolygons(list(source), proj4string = las@proj4string)

  if (is(source, "SpatialPolygons") | is(source, "SpatialPolygonsDataFrame"))
  {
    bbox <- extent(las)
    source2 <- raster::crop(source, bbox*1.01 + las@header@PHB[["X scale factor"]])

    if (!is.null(source2))
      source <- sf::st_as_sf(source2)
    else
    {
      source <- sf::st_as_sf(source)
      source <- source[0,]
    }
  }

  if (is(source, "sf"))
    values <- merge_sf(las, source, attribute)
  else if (is(source, "RasterLayer"))
    values <- merge_raster(las, source)
  else if (is(source, "RasterStack") | is(source, "RasterBrick"))
    return(merge_rgb(las, source))
  else
    stop("No method for this source format.")

  if (is.null(attribute))
    attribute <- "id"

  las <- add_attribute(las, values, attribute)
  return(las)
}

merge_rgb = function(las, source)
{
  cells <- raster::cellFromXY(source[[1]], coordinates(las))

  R <- source[[1]][]
  G <- source[[2]][]
  B <- source[[3]][]
  R <- as.integer(R[cells])
  G <- as.integer(G[cells])
  B <- as.integer(B[cells])

  if (anyNA(R) | anyNA(G) | anyNA(B))
    stop("Some points were associated with an RGB color of NA. RGB cannot be NA in a LAS object. Colorization aborted.", call. = FALSE)

  return(add_lasrgb(las, R, G, B))
}

merge_sf = function(las, source, attribute = NULL)
{
  npoints <- nrow(las@data)

  # No attribute is provided: assign the number of the polygon
  if (is.null(attribute))
  {
    method <- 0
    values <- rep(NA_integer_, npoints)
  }
  # The attribute is the name of an attribute in the attribute table: assign the value of the attribute
  else if (attribute %in% names(source))
  {
    method <- 1
    data   <- source[[attribute]]

    if (class(data) == "factor")
      values = factor(rep(NA_integer_, npoints), levels = levels(data))
    else if (class(data) == "integer")
      values = rep(NA_integer_, npoints)
    else if (class(data) == "logical")
      values = rep(NA, npoints)
    else if (class(data) == "numeric")
      values = rep(NA_real_, npoints)
    else if (class(data) == "character")
      values = rep(NA_character_, npoints)
    else
      stop(glue::glue("The attribute {attribute} in the table of attribute is not of a supported type."))
  }
  # The attribute is not the name of an attribute in the attribute table: assign a boolean value if
  # the point is in a polygon or not.
  else
  {
    method <- 2
    values <- logical(npoints)
  }

  if (nrow(source) == 0)
    return(values)

  verbose("Testing whether points fall in a given polygon...")

  ids <- rep(0L, npoints)

  for (i in seq_along(sf::st_geometry(source)))
  {
    wkt          <- sf::st_as_text(sf::st_geometry(source)[i], digits = 10)
    in_poly      <- C_in_polygon(las, wkt, getThread())
    ids[in_poly] <- i
  }

  if (method == 1)
    values[ids > 0L] <- source[[attribute]][ids[ids > 0]]
  else if (method == 2)
    values <- ids > 0
  else
    values <- ifelse(ids == 0L, NA_integer_, ids)

  return(values)
}

merge_raster = function(las, raster)
{
  cells <- raster::cellFromXY(raster, coordinates(las))

  # This will respect data type when in memory (data type lost if written in file)
  if (raster::inMemory(raster))
    return(raster@data@values[cells])
  else
    return(raster[cells])
}

