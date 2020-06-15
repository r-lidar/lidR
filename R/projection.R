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
#' Get or set the projection of a LAS* object with the function `projection`. Functions `epsg`
#' and `wkt` are reserved for advanced users (see details).
#'
#' There are two ways to store the CRS of a point cloud in a LAS file:
#'
#'    - Store an EPSG code (for LAS 1.0 to 1.4)
#'    - Store a WTK string (for LAS 1.4)
#'
#' On the other hand, all spatial R packages use a `proj4string` to store the CRS. This is why
#' the CRS is duplicated in a LAS object. The information belongs within the header in a format that
#' can be written in a LAS file and in the slot `proj4string` in a format that can be understood
#' by R packages.
#'
#'    - `projection<-`: updates the CRS from a `proj4string`. It updates the header either
#' with the EPSG code for LAS formats < 1.4 or with a WKT string for LAS format 1.4 and updates the
#' `proj4string` slot. This function should always be preferred.
#'    - `epsg<-`: updates the CRS from an EPSG code. It adds the EPSG code in the header and updates
#' the `proj4string` slot.
#'    - `wkt<-`: updates the CRS from a WKT string. It adds the WKT string in the header and updates
#' the `proj4string` slot.
#'    - `projection`: reads the `proj4string` from the `proj4string` slot.
#'    - `epsg`: reads the epsg code from the header.
#'    - `wkt`: reads the WKT string from the header.
#'    - `crs` and `crs<-` are equivalent to `projection`
#'
#' @param object,x,obj An object of class LAS or eventually LASheader (regular users don't need to manipulate
#' LASheader objects).
#' @param ... Unused.
#' @param asText logical. If TRUE, the projection is returned as text. Otherwise a CRS object is returned.
#' @param value A `CRS` object or a `proj4string` string for function`projection`.
#' An EPSG code as integer for function `epsg`. A `WKT` string for function `wkt`.
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
#' @importFrom raster crs<-
#' @importFrom raster crs
#' @name projection
#' @rdname projection
#' @md
NULL

#' @export
#' @rdname projection
setGeneric("epsg", function(object, ...)
  standardGeneric("epsg"))

#' @export
#' @rdname projection
setGeneric("epsg<-", function(object, value)
  standardGeneric("epsg<-"))

# @export
# @rdname projection
#setGeneric("wkt", function(object, ...)
#  standardGeneric("wkt"))

#' @export
#' @rdname projection
setGeneric("wkt<-", function(object, value)
  standardGeneric("wkt<-"))

#' @export
#' @rdname projection
setMethod("projection", "LASheader", function(x, asText = TRUE)
{
  if (use_epsg(x) && epsg(x) != 0L)
    proj4 <- epsg2CRS(epsg(x))
  else if (use_wktcs(x) && wkt(x) != "")
    proj4 <- wkt2CRS(wkt(x))
  else
    proj4 <- sp::CRS()

  if (asText)
    return(proj4@projargs)
  else
    return(proj4)
})

#' @export
#' @rdname projection
setMethod("crs", "LASheader", function(x, asText = FALSE)
{
  return(projection(x, asText))
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
  if (use_wktcs(object)) stop("This object is not in a format that supports EPSG code. Use a WKT string.", call. = FALSE)
  header <- as.list(object)
  header <- rlas::header_set_epsg(header, value)
  return(LASheader(header))
})

#' @export
#' @importFrom sp wkt
#' @rdname projection
setMethod("wkt", "LASheader", function(obj)
{
  return(rlas::header_get_wktcs(as.list(obj)))
})

#' @export
#' @rdname projection
setMethod("wkt<-", "LASheader", function(object, value)
{
  if (!use_wktcs(object)) stop("This object is not in a format that supports WKT CRS. Use an EPSG code.", call. = FALSE)
  header <- as.list(object)
  header <- rlas::header_set_wktcs(header, value)
  return(LASheader(header))
})


# ==== LAS ====

#' @export
#' @rdname projection
setMethod("projection", "LAS", function(x, asText = TRUE)
{
  proj4 <- x@proj4string

  if (asText)
    return(proj4@projargs)
  else
    return(proj4)
})

#' @export
#' @rdname projection
setMethod("projection<-", "LAS", function(x, value)
{
  # The input is a proj4string or a CRS from sp
  if (is(value, "CRS"))
    proj4 <- value@projargs
  else if (is.character(value))
    proj4 <- value
  else
    stop("'value' is not a CRS or a string.")

  # Extract epsg code if any
  epsg  <- sub("\\+init=epsg:(\\d+).*",  "\\1", proj4)
  epsg  <- suppressWarnings(as.integer(epsg))

  # Remove +init=epsg:xxx if any
  proj4 <- sub("\\+init=epsg:\\d+\\s", "", proj4)

  if (is.na(proj4)) return(x)

  if (use_wktcs(x))
  {
    wkt <- rgdal::showWKT(proj4)
    wkt(x@header) <- wkt
    x@proj4string <- sp::CRS(proj4)
    return(x)
  }
  else
  {
    ## We were not able to extract the epsg code earlier, we can retrieve it with rgdal
    if (is.na(epsg))
      epsg <- rgdal::showEPSG(proj4)

    # We are still unable to find an epsg code
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
setMethod("crs", "LAS", function(x, asText = FALSE)
{
  return(projection(x, asText))
})

#' @export
#' @rdname projection
setMethod("crs<-", "LAS", function(x, ..., value)
{
  projection(x) <- value
  return(x)
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
  proj4 <- epsg2CRS(value, fail = TRUE)
  epsg(object@header) <- value
  object@proj4string <- proj4
  return(object)
})

#' @export
#' @importFrom sp wkt
#' @rdname projection
setMethod("wkt", "LAS", function(obj)
{
  return(wkt(obj@header))
})

#' @export
#' @rdname projection
setMethod("wkt<-", "LAS", function(object, value)
{
  proj4 <- wkt2CRS(value, fail = TRUE)
  wkt(object@header) <- value
  object@proj4string <- proj4
  return(object)
})

# ===== LAScatalog =======

#' @export
#' @rdname projection
setMethod("projection", "LAScatalog", function(x, asText = TRUE)
{
  proj4 <- x@proj4string

  if (asText)
    return(proj4@projargs)
  else
    return(proj4)
})

#' @export
#' @rdname projection
setMethod("crs", "LAScatalog", function(x, asText = FALSE)
{
  return(projection(x, asText))
})

# ===== INTERNAL TOOLS =======

#' Datum transformation for LAS objects
#'
#' A version of \link[rgdal:spTransform]{spTransform} for \link[=LAS-class]{LAS} objects.
#' Returns transformed coordinates of a \code{LAS} object from the projection of the object to the
#' the projection given by arguments.
#'
#' @param x An object of class \link[=LAS-class]{LAS}
#' @param CRSobj logical. Object of class \link[sp:CRS-class]{CRS} or of class character, in which
#' case it is converted to \link[sp:CRS-class]{CRS}.
#' @param ... ignored
#'
#' @return An object of class \link[=LAS-class]{LAS} with coordinates XY transformed to the new
#' coordinate reference system. The header has been update by add the ESPG code or a WKT OGC CS string
#' as a function of the defined Global Encoding WKT bit (see LAS specifications).
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzrn")
#' crs <- sp::CRS("+init=epsg:26918")
#'
#' las <- spTransform(las, crs)
#' @importMethodsFrom sp spTransform
#' @noRd
setMethod("spTransform", signature("LAS", "CRS"), function(x, CRSobj, ...)
{
  if (is.na(sp::proj4string(x)))
    stop("No transformation possible from NA reference system")

  # Tranform the point coordinates
  spts <- sp::SpatialPoints(coordinates(x))
  spts@proj4string <- crs(x)
  spts <- sp::spTransform(spts, CRSobj)

  # Update the LAS object
  x@data[["X"]] <- spts@coords[,1]
  x@data[["Y"]] <- spts@coords[,2]
  x <- lasupdateheader(x)

  # Update the offsets
  x@header@PHB[["X offset"]] <- min(x$X)
  x@header@PHB[["Y offset"]] <- min(x$Y)

  crs(x) <- CRSobj
  return(x)
})

use_wktcs <- function(x) {
  UseMethod("use_wktcs", x)
}

use_wktcs.LAS <- function(x) {
  return(x@header@PHB[["Global Encoding"]][["WKT"]])
}

use_wktcs.LASheader <- function(x) {
  return(x@PHB[["Global Encoding"]][["WKT"]])
}

use_epsg <- function(x) {
  UseMethod("use_epsg", x)
}

use_epsg.LAS <- function(x) {
  return(!x@header@PHB[["Global Encoding"]][["WKT"]])
}

use_epsg.LASheader <- function(x) {
  return(!x@PHB[["Global Encoding"]][["WKT"]])
}

epsg2proj <- function(epsg, fail = FALSE)
{
  tryCatch(
  {
    wkt <- rgdal::showWKT(glue::glue("+init=epsg:{epsg}"))
    rgdal::showP4(wkt)
  },
  error = function(e)
  {
    if (!fail)
      return(NA_character_)
    else
      stop("Invalid epsg code", call. = FALSE)
  })
}

epsg2CRS <- function(epsg, fail = FALSE)
{
  # patch starting from rgdal 1.5-8
  if (utils::packageVersion("rgdal") > "1.4.8" && rgdal::new_proj_and_gdal())
  {
    SRS_string = paste0("EPSG:", epsg)

    crs <- tryCatch(
    {
      sp::CRS(SRS_string = SRS_string)
    },
    error = function(e)
    {
      if (!fail)
        return(sp::CRS(NA_character_))
      else
        stop("Invalid epsg code", call. = FALSE)
    })

    return(crs)
  }
  else
  {
    proj <- epsg2proj(epsg, fail)
    crs <- sp::CRS(proj)
  }

  return(crs)
}

wkt2proj <- function(wkt, fail = FALSE)
{
  tryCatch(
  {
    rgdal::showP4(wkt)
  },
  error = function(e)
  {
    if (!fail)
        return(NA_character_)
    else
      stop("Invalid WKT", call. = FALSE)
  })
}

wkt2CRS <- function(wkt, fail = FALSE)
{
  # patch starting from rgdal 1.5-8
  if (utils::packageVersion("rgdal") > "1.4.8" && rgdal::new_proj_and_gdal())
  {
    crs <- tryCatch(
    {
        sp::CRS(SRS_string = wkt)
    },
    error = function(e)
    {
      if (!fail)
        return(sp::CRS(NA_character_))
      else
        stop("Invalid WKT string", call. = FALSE)
    })

    return(crs)
  }
  else
  {
    proj <- wkt2proj(wkt, fail)
    crs <- sp::CRS(proj, doCheckCRSArgs = FALSE) # doCheckCRSArgs = FALSE added in 2.2.4 after #323
  }

  return(crs)
}
