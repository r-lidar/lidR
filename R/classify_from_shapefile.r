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
#' @return Nothing. The new field is added by reference in the original data.
#' @examples
#' \dontrun{
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
#' }
#' @seealso
#' \code{\link[rgdal:readOGR]{readOGR} }
#' \code{\link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame} }
#' @export classify_from_shapefile
#' @importFrom raster crop
#' @importFrom data.table setnames :=
setGeneric("classify_from_shapefile", function(obj, shapefile, field = NULL){standardGeneric("classify_from_shapefile")})

#' @rdname classify_from_shapefile
#' @useDynLib lidR
#' @importFrom Rcpp sourceCpp
setMethod("classify_from_shapefile", c("LAS", "SpatialPolygonsDataFrame"),
  function(obj, shapefile, field = NULL)
  {
    info <- NULL

    npoints = dim(obj@data)[1]

    # No field is provide:
    # Associate the number of the polygon
    if(is.null(field))
    {
      field = "id"
      method = 0
    }
    # The field is the name of a field in the attribute table:
    # Associate the value of the field
    else if(field %in% names(shapefile@data))
    {
      method = 1

      if(class(shapefile@data[,field]) == "factor")
        values = factor(rep(NA, npoints), levels = levels(shapefile@data[,field]))
      else
        values = rep(NA_real_, npoints)
    }
    # The field is not the name of a field in the attribute table:
    # Associate a boolean if the point is in a polygon or not.
    else
    {
      method = 2
      values = logical(npoints)
    }

    # Crop the shapefile to minimize the computations removing out of bounds polygons
    polys = raster::crop(shapefile, extent(obj))

    # No polygon? Return NA or false depending on the method used
    if(is.null(polys))
      return(values)

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

    # The reduction to 1 level of depth will introduce a lost of information for multi part polygon
    # Retrieve the real ids of each polygon befor reducing to 1 level depth
    i = 0
    lengths = lapply(xcoords, length)  %>%  unlist
    idpolys = lapply(lengths, function(x){i<<-i+1;rep.int(i,x)}) %>% unlist

    # Make the lists 1 level depth
    xcoords %<>% unlist(recursive = FALSE)
    ycoords %<>% unlist(recursive = FALSE)

    # Return the id of each polygon
    ids = points_in_polygons(xcoords, ycoords, obj@data$X, obj@data$Y)

    if(method == 1)
    {
      inpoly = ids > 0
      values[inpoly] = polys@data[, field][idpolys[ids[inpoly]]]

      message(paste0("Assign the value of field ", field , " from the table of attibutes to the points"))
    }
    else if(method == 2)
    {
      values = ids > 0

      message("Assign a boolean value to the points")
    }
    else
    {
      values = ifelse(ids == 0, NA_real_, ids)

      message("Assign a number to each individual polygon")
    }

    obj@data[,info:=values][]

    colnames = names(obj@data)
    colnames[length(colnames)] = field
    data.table::setnames(obj@data, colnames)

    return(invisible(NULL))
  }
)
