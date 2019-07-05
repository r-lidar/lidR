# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2019 Jean-Romain Roussel
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

#' Get or set the projection of a LAS* object
#'
#' Get or set the projection of a LAS* object with the function \code{projection}. Functions \code{epsg}
#' and \code{wkt} are reserved for advanced users (see details).
#'
#' There are two ways to store the CRS of a point cloud in a LAS file:
#' \itemize{
#' \item Store an EPSG code (for LAS 1.0 to 1.4)
#' \item Store a WTK string (for LAS 1.4)
#' }
#' On the other hand, all spatial R packages use a \code{proj4string} to store the CRS. This is why
#' the CRS is duplicated in a LAS object. The information belongs within the header in a format that
#' can be written in a LAS file and in the slot \code{proj4string} in a format that can be understood
#' by R packages.
#' \itemize{
#' \item \code{projection<-}: updates the CRS from a \code{proj4string}. It updates the header either
#' with the EPSG code for LAS formats < 1.4 or with a WKT string for LAS format 1.4 and updates the
#' \code{proj4string} slot. This function should always be preferred.
#' \item \code{epsg<-}: updates the CRS from an EPSG code. It adds the EPSG code in the header and updates
#' the \code{proj4string} slot.
#' \item \code{wkt<-}: updates the CRS from a WKT string. It adds the WKT string in the header and updates
#' the \code{proj4string} slot.
#' \item \code{projection}: reads the \code{proj4string} from the \code{proj4string} slot.
#' \item \code{epsg}: reads the epsg code from the header.
#' \item \code{wkt}: reads the WKT string from the header.
#' }
#'
#' @param object,x An object of class LAS or eventually LASheader (regular users don't need to manipulate
#' LASheader objects).
#' @param ... Unused.
#' @param asText logical. If TRUE, the projection is returned as text. Otherwise a CRS object is returned.
#' @param value A \code{CRS} object or a \code{proj4string} string for function\code{projection}.
#' An EPSG code as integer for function \code{epsg}. A \code{WKT} string for function \code{wkt}.
#'
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile)
#' crs <- sp::CRS("+init=epsg:26918")
#'
#' projection(las)
#' projection(las) <- crs
#' @importFrom raster projection<-
#' @importFrom raster projection
#' @name projection
#' @rdname projection
NULL

#' @export
#' @rdname projection
setGeneric("epsg", function(object, ...)
  standardGeneric("epsg"))

#' @export
#' @rdname projection
setGeneric("epsg<-", function(object, value)
  standardGeneric("epsg<-"))

#' @export
#' @rdname projection
setGeneric("wkt", function(object, ...)
  standardGeneric("wkt"))

#' @export
#' @rdname projection
setGeneric("wkt<-", function(object, value)
  standardGeneric("wkt<-"))

#' @export
#' @rdname projection
setMethod("projection", "LASheader", function(x, asText = TRUE)
{
  if (epsg(x) != 0L)
  {
    proj4 <- tryCatch(sp::CRS(paste0("+init=epsg:", epsg(x))), error = function(e) sp::CRS())
    proj4@projargs <- gsub("\\+init=epsg:\\d+\\s", "", proj4@projargs)
  }
  else if (wkt(x) != "")
    proj4 <- tryCatch(sp::CRS(rgdal::showP4(wkt(x))), error = function(e) sp::CRS())
  else
    proj4 <- sp::CRS()

  if (asText)
    return(proj4@projargs)
  else
    return(proj4)
})

# ==== LASheader ====

#' @export
#' @rdname projection
setMethod("epsg", "LASheader", function(object, ...)
{
  return(rlas::header_get_epsg(as.list(object)))
})

#' @export
#' @rdname projection
setMethod("epsg<-", "LASheader", function(object, value)
{
  header <- as.list(object)
  header <- rlas::header_set_epsg(header, value)
  return(LASheader(header))
})

#' @export
#' @rdname projection
setMethod("wkt", "LASheader", function(object, ...)
{
  return(rlas::header_get_wktcs(as.list(object)))
})

#' @export
#' @rdname projection
setMethod("wkt<-", "LASheader", function(object, value)
{
  header <- as.list(object)
  header <- rlas::header_set_wktcs(header, value)
  return(LASheader(header))
})


# ==== LAS ====

#' @export
#' @rdname projection
setMethod("projection<-", "LAS", function(x, value)
{
  if (is(value, "CRS"))
    proj4 <- value@projargs
  else if (is.character(value))
    proj4 <- value
  else
    stop("'value' is not a CRS or a string.")

  epsg  <- sub("\\+init=epsg:(\\d+).*",  "\\1", proj4)
  epsg  <- suppressWarnings(as.integer(epsg))
  proj4 <- sub("\\+init=epsg:\\d+\\s", "", proj4)

  if (x@header@PHB[["Global Encoding"]][["WKT"]] == TRUE)
  {
    if (is.na(proj4))
      return(x)

    wkt <- rgdal::showWKT(proj4)
    wkt(x@header) <- wkt
    x@proj4string <- sp::CRS(proj4)
    return(x)
  }
  else
  {
    if (is.na(proj4))
      return(x)

    if (is.na(epsg))
      epsg <- rgdal::showEPSG(proj4)

    if (epsg == "OGRERR_UNSUPPORTED_SRS")
    {
      warning("EPSG code not found: header not updated. Try to use the function epsg() manually to ensure CRS will be written in file.", call. = FALSE)
      pos <- rlas:::where_is_epsg(as.list(x@header))
      if (pos != 0)  x@header@VLR[["GeoKeyDirectoryTag"]][["tags"]][[pos]] <- NULL
    }
    else
    {
      epsg(x@header) <- epsg
    }

    x@proj4string <- sp::CRS(proj4)
    return(x)
  }
})

#' @export
#' @rdname projection
setMethod("epsg", "LAS", function(object)
{
  return(epsg(object@header))
})

#' @export
#' @rdname projection
setMethod("epsg<-", "LAS", function(object, value)
{
  proj4 <- sp::CRS(glue::glue("+init=epsg:{value}"))
  proj4 <- gsub("\\+init=epsg:\\d+\\s", "", proj4)
  epsg(object@header) <- value
  raster::projection(object)  <- proj4
  return(object)
})

#' @export
#' @rdname projection
setMethod("wkt", "LAS", function(object)
{
  return(wkt(object@header))
})

#' @export
#' @rdname projection
setMethod("wkt<-", "LAS", function(object, value)
{
  proj4 <- rgdal::showP4(value)
  wkt(object@header) <- value
  raster::projection(object)  <- proj4
  return(object)
})
