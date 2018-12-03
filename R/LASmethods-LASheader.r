# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
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

#' Create a \code{LASheader} object
#'
#' @param data a list containing the data from the header of a las file.
#' @return An object of class \code{LASheader}
#' @export
LASheader <- function(data = list()) {return(new("LASheader", data))}

#' Plot LAS* objects
#'
#' Plot displays a \link[lidR:LASheader-class]{LASheader} object exactly like it displays a LAScatalog
#' object.
#'
#' @export
#' @examples
#'
#' \dontrun{
#'
#' # single file catalog using data provided in lidR
#' ctg = catalog(LASfile)
#' plot(ctg)
#' }
#' @describeIn plot plot LASheader
setMethod("plot", signature(x = "LASheader", y = "missing"), function(x, y, mapview = TRUE, ...)
{
  epsg <- epsg(x)
  crs  <- tryCatch({sp::CRS(glue::glue("+init=epsg:{epsg}"))}, error = function(e) return(sp::CRS()))
  xmin <- x@PHB$`Min X`
  xmax <- x@PHB$`Max X`
  ymin <- x@PHB$`Min Y`
  ymax <- x@PHB$`Max Y`
  mtx  <- matrix(c(xmin, xmax, ymin, ymax)[c(1, 1, 2, 2, 1, 3, 4, 4, 3, 3)], ncol = 2)
  Sr   <- sp::Polygons(list(sp::Polygon(mtx)), "1")
  Sr   <- sp::SpatialPolygons(list(Sr), proj4string = crs)

  res <- new("LAScatalog")
  res@bbox <- Sr@bbox
  res@proj4string <- Sr@proj4string
  res@plotOrder <- Sr@plotOrder
  res@data <- data.table::as.data.table(x@PHB)
  res@polygons <- Sr@polygons

  plot.LAScatalog(res, mapview = mapview, ...)
})

setMethod("show", "LASheader",  function(object)
{
  x = object@PHB

  cat("File signature:          ", x$`File Signature`, "\n")
  cat("File source ID:          ", x$`File Source ID`, "\n")
  cat("Global encoding:         ", x$`Global Encoding`, "\n")
  cat("Project ID - GUID:       ", x$`Project ID - GUID`, "\n")
  cat("Version:                  ", x$`Version Major`, ".", x$`Version Minor`, "\n", sep = "")
  cat("System identifier:       ", x$`System Identifier`, "\n")
  cat("Generating software:     ", x$`Generating Software`, "\n")
  cat("File creation d/y:        ", x$`File Creation Day of Year`, "/", x$`File Creation Year`, "\n", sep = "")
  cat("header size:             ", x$`Header Size`, "\n")
  cat("Offset to point data:    ", x$`Offset to point data`, "\n")
  cat("Num. var. length record: ", x$`Number of variable length records`, "\n")
  cat("Point data format:       ", x$`Point Data Format ID`, "\n")
  cat("Point data record length:", x$`Point Data Record Length`, "\n")
  cat("Num. of point records:   ", x$`Number of point records`, "\n")
  cat("Num. of points by return:", x$`Number of 1st return`, x$`Number of 2nd return`, x$`Number of 3rd return`, x$`Number of 4th return`, x$`Number of 5th return`, "\n")
  cat("Scale factor X Y Z:      ", x$`X scale factor`, x$`Y scale factor`, x$`Z scale factor`, "\n")
  cat("Offset X Y Z:            ", x$`X offset`, x$`Y offset`, x$`Z offset`, "\n")
  cat("min X Y Z:               ", x$`Min X`, x$`Min Y`, x$`Min Z`, "\n")
  cat("max X Y Z:               ", x$`Max X`, x$`Max Y`, x$`Max Z`, "\n")

  n = length(object@VLR)

  if(n == 0)
  {
    cat("Variable length records:  void\n")
    return(invisible())
  }

  cat("Variable length records: \n")

  for(i in 1:n)
  {
    vlr = object@VLR[[i]]

    cat("   Variable length record", i, "of", n, "\n")
    #cat("       Reserve:            ",  vlr$reserved, "\n")
    #cat("       User ID:             ", vlr$`user ID`, "\n")
    #cat("       record ID:           ", vlr$`record ID`, "\n")
    #cat("       Length after header: ", vlr$`length after header`, "\n")
    cat("       Description: ", vlr$description, "\n")

    if(vlr$`record ID` == 34735)
    {
      cat("       Tags:\n")
      lapply(vlr[[6]], function(xx)
      {
        cat("          Key", xx$key, "tiff_tag_location", xx$`tiff tag location`, "count", xx$count, "value offset", xx$`value offset`, "\n")
      })
    }
    else if(vlr$`record ID` == 34736)
    {
      cat("       data:                ", vlr[[6]], "\n")
    }
    else if(vlr$`record ID` == 34737)
    {
      cat("       data:                ", vlr[[6]], "\n")
    }
    if(vlr$`record ID` == 4)
    {
      cat("       Extra Bytes Description:\n")
      lapply(vlr$`Extra Bytes Description`, function(xx)
      {
        cat("          ", xx$name, ": ", xx$description, "\n", sep = "")
      })
    }
  }

  return(invisible())
})

#' Transform to a list
#'
#' Functions to construct, coerce and check for both kinds of R lists.
#'
#' @param x A LASheader object
#' @param ... unused
#' @method as.list LASheader
#' @export
as.list.LASheader <- function(x, ...)
{
  PHB = x@PHB
  VLR = list(`Variable Length Records` = x@VLR)
  return(c(PHB, VLR))
}
