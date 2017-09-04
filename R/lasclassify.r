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



#' Classify LiDAR points from source data
#'
#' Classify LAS points based on geographic data from external sources. It adds an attribute
#' to each point based on a value found in the external data. External sources can be an ESRI
#' Shapefile (SpatialPolygonsDataFrame) or a Raster (RasterLayer).
#'
#' The function recognizes several type of sources:
#' \itemize{
#' \item{\code{SpatialPolygonsDataFrame}: Polygons can be simple, one-part shapes,
#' multi-part polygons, or polygons with holes. It checks if the LiDAR points are in polygons
#' given in the shapefile. If the parameter \code{field} is the name of a field in the table of attributes
#' of the shapefile it assigns to the points the values of that field. Otherwise it classifies
#' the points as boolean. TRUE if the points are in a polygon, FALSE otherwise. This function
#' allows filtering of lakes, for example.
#' }
#' \item{\code{RasterLayer}: It attributes to each point the value found in each pixel of the RasterLayer.
#' Use the parameter \code{field} to force the name of the new column added in the LAS object. This function
#' is used internally to normalize the lidar dataset and is exported because some users may find it useful
#' (for example to colorize the point cloud using a georeferenced RGB image.}
#' }
#' More examples available on \href{https://github.com/Jean-Romain/lidR/wiki/lasclassify}{lidR wiki}.
#'
#' @param .las An object of the class \code{LAS}
#' @param source An object of class \code{SpatialPolygonsDataFrame} or \code{RasterLayer}
#' @param field characters. The name of a field in the table of attributes of the shapefile or
#' the name of the new column in the LAS object (see details)
#' @return Nothing. The new field is added by reference to the original data.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' shapefile_dir <- system.file("extdata", package = "lidR")
#'
#' lidar = readLAS(LASfile)
#' lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")
#'
#' # The field "inlake" does not exist in the shapefile. Points are classified as TRUE if in a polygon
#' lasclassify(lidar, lakes, "inlakes") # New column 'inlakes' is added.
#' forest = lasfilter(lidar, inlakes == FALSE)
#' plot(lidar)
#' plot(forest)
#'
#' # The field "LAKENAME_1" exists in the shapefile.
#' # Points are classified with the values of the polygons
#' lasclassify(lidar, lakes, "LAKENAME_1") # New column 'LAKENAME_1' is added.
#' @seealso
#' \code{\link[rgdal:readOGR]{readOGR} }
#' \code{\link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame} }
#' @export
lasclassify = function(.las, source, field = NULL)
{
  stopifnotlas(.las)

  if (is(source, "SpatialPolygonsDataFrame"))
    values = classify_from_shapefile(.las, source, field)
  else if (is(source, "RasterLayer") | is(source, "RasterStack"))
    values = classify_from_rasterlayer(.las, source, field)
  else
    stop("No method for this source format.", call. = F)

  .las@data[,(field) := values][]

  return(invisible())
}

classify_from_shapefile = function(.las, shapefile, field = NULL)
{
  info <- NULL

  npoints = dim(.las@data)[1]

  # No field is provided:
  # Assign the number of the polygon
  if (is.null(field))
  {
    field = "id"
    method = 0
  }
  # The field is the name of a field in the attribute table:
  # Assign the value of the field
  else if (field %in% names(shapefile@data))
  {
    method = 1

    if (class(shapefile@data[,field]) == "factor")
      values = factor(rep(NA, npoints), levels = levels(shapefile@data[,field]))
    else
      values = rep(NA_real_, npoints)
  }
  # The field is not the name of a field in the attribute table:
  # Assign a boolean if the point is in a polygon or not.
  else
  {
    method = 2
    values = logical(npoints)
  }

  # Crop the shapefile to minimize the computations removing out of bounds polygons
  verbose("Croping the shapefile...")
  polys = raster::crop(shapefile, extent(.las) + 20)


  # No polygon? Return NA or false depending on the method used
  if (!is.null(polys))
  {
    verbose("Analysing the polygons...")

    # Extract the coordinates of each polygon as a list.
    # The list has 2 levels of depth because of multi part polygons
    xcoords = lapply(polys@polygons,
                     function(x)
                     {
                       lapply(x@Polygons, function(x){x@coords[,1]})
                     })

    ycoords = lapply(polys@polygons,
                     function(x)
                     {
                       lapply(x@Polygons, function(x){x@coords[,2]})
                     })

    is_hole = lapply(polys@polygons,
                     function(x)
                     {
                       lapply(x@Polygons, function(x){x@hole})
                     })

    # The reduction to 1 level of depth list will cause loss of information
    # for multi-part polygon. Here we need to retrieve the real IDs of each polygon
    # before reducing to 1 level of depth
    i = 0
    lengths = lapply(xcoords, length)  %>%  unlist
    idpolys = lapply(lengths, function(x){i <<- i + 1 ; rep.int(i,x)}) %>% unlist

    # Make the lists 1 level depth
    xcoords %<>% unlist(recursive = FALSE)
    ycoords %<>% unlist(recursive = FALSE)

    is_hole %<>% unlist()
    is_hole = c(FALSE, is_hole)

    # Return the id of each polygon
    verbose("Testing whether points fall in a given polygon...")

    ids = points_in_polygons(xcoords, ycoords, .las@data$X, .las@data$Y, LIDROPTIONS("progress"))

    if (method == 1)
    {
      verbose("Retrieving correspondances in the table of attributes...")

      inpoly = ids > 0
      inhole = is_hole[ids + 1]
      inpoly.nothole = inpoly & !inhole

      id = idpolys[ids[inpoly.nothole]]
      values[inpoly.nothole] = polys@data[, field][id]

      verbose(paste0("Assigned the value of field ", field , " from the table of attibutes to the points"))
    }
    else if (method == 2)
    {
      values = ids > 0 & !is_hole[ids + 1]
      verbose("Assigned a boolean value to the points")
    }
    else
    {
      values = ifelse(ids == 0, NA_real_, ids)
      verbose("Assigned a number to each individual polygon")
    }
  }
  else
  {
    verbose("No polygon found within the data", call. = F)
  }

  return(values)
}

classify_from_rasterlayer = function(.las, raster, field = NULL)
{
  . <- X <- Y <- info <- NULL

  if (is.null(field))
    field = lazyeval::expr_label(raster)

  xres = raster::res(raster)[1]
  xmin = raster@extent@xmin
  ymin = raster@extent@ymin
  m  = raster::as.matrix(raster)
  values = fast_extract(m, .las@data$X, .las@data$Y, xmin, ymin, xres) # 15 times faster than raster::extract + much memory effcient
  return(values)
}

