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



#' Classify LiDAR points from the polygons in a shapefile
#'
#' Classify LiDAR points from the polygons in an ESRI shapefile
#'
#' Classify LAS points based on geographic data found in a shapefile. It checks
#' if the LiDAR points are in polygons given in the shapefile. If the parameter
#' \code{field} is the name of a field in the shapefile it classifies the points
#' based on the data in the shapefile. Else it classifies the points as boolean. TRUE
#' if the points are in a polygon, FALSE otherwise. This function allows for filtering
#' lakes, for example.
#' @param obj An object of the class \code{LAS}
#' @param shapefile An object of class SpatialPolygonsDataFrame
#' @param field characters. The name of a field of the shapefile or the name of the new field in the LAS object.
#' @return An object of the class \code{LAS} with a new field
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' shapefile_dir <- system.file("extdata", package = "lidR")
#'
#' lidar = readLAS(LASfile)
#' lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")
#'
#' # The field "inlake" does not exist in the shapefile. Points are classified as TRUE if in a polygon
#' classify_from_shapefile(lidar, lakes, "inlakes")
#' forest = lasfilter(lidar, inlakes == FALSE)
#' plot(lidar)
#' plot(forest)
#'
#' # The field "LAKENAME_1" exists in the shapefile.
#' # Points are classified with the value of the polygon
#' classify_from_shapefile(lidar, lakes, "LAKENAME_1")
#' @seealso
#' \code{\link[rgdal:readOGR]{readOGR} }
#' \code{\link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame} }
#' @export classify_from_shapefile
#' @importFrom raster crop
#' @importFrom rgdal readOGR
#' @importFrom data.table setnames :=
setGeneric("classify_from_shapefile", function(obj, shapefile, field){standardGeneric("classify_from_shapefile")})

#' @rdname classify_from_shapefile
#' @useDynLib lidR
#' @importFrom Rcpp sourceCpp
setMethod("classify_from_shapefile", "LAS",
  function(obj, shapefile, field)
  {
    info <- NULL

    npoints = dim(obj@data)[1]

    if(field %in% names(shapefile@data))
    {
      method = 1

      if(class(shapefile@data[,field]) == "factor")
        values = factor(rep(NA, npoints), levels = levels(shapefile@data[,field]))
      else
        values = rep(NA_real_, npoints)
    }
    else
    {
      method = 2
      values = logical(npoints)
    }

    polys = raster::crop(shapefile, extent(obj))

    if(is.null(polys))
      return(values)

    xcoords = lapply(polys@polygons, function(x){x@Polygons[[1]]@coords[,1]})
    ycoords = lapply(polys@polygons, function(x){x@Polygons[[1]]@coords[,2]})

    ids = pointsInPolygons(xcoords, ycoords, obj@data$X, obj@data$Y)

    if(method == 1)
    {
      ids = ids[ids > 0]
      values[ids] = polys@data[, field][ids]
    }

    else if(method == 2)
      values = ids > 0

    obj@data[,info:=values]

    colnames = names(obj@data)
    colnames[length(colnames)] = field
    data.table::setnames(obj@data, colnames)

    return(invisible(NULL))
  }
)
