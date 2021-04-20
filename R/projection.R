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
#' Get or set the projection of a LAS* object.
#'
#' There are two ways to store the CRS of a point cloud in a LAS file:
#'
#'    - Store an EPSG code (for LAS 1.0 to 1.3)
#'    - Store a WTK string (for LAS 1.4)
#'
#' On the other hand, R sptial packages use a `proj4string` to store the CRS (but
#' the ecosystem is moving to WKT). This is why the CRS is duplicated in a LAS object.
#' The information belongs within the header in a format that can be written in a
#' LAS file and in the slot `proj4string` in a format that can be understood by R
#' packages.
#'
#'    - `projection<-`: assigns a CRS from a `CRS` (`sp`), a `crs` (`sf`), a WKT
#'    string, a proj4string or an epsg code. It updates the header of the LAS
#'    object either with the EPSG code for LAS formats < 1.4 or with a WKT string
#'    for LAS format 1.4 and updates the `proj4string` slot.
#'    - `projection`: returns the CRS in `sp` format
#'    - `crs` and `crs<-` are equivalent to `projection` and `projection<-`
#'    - `epsg<-`, `wkt<- `: legacy functions superseded by `projection<-`
#'    - `sf::st_crs` return the CRS in `sf` format.
#'    - `epsg`: reads the epsg code from the header.
#'    - `wkt`: reads the WKT string from the header.
#'
#' @param object,x,obj An object of class LAS or eventually LASheader (regular users don't need to manipulate
#' LASheader objects).
#' @param ... Unused.
#' @param asText logical. If TRUE, the projection is returned as text. Otherwise a CRS object is returned.
#' @param value A `CRS` object or a `proj4string` string or WKT string or an EPSG code.
#'
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' # Get the EPSG code stored in the header (returns 0 if not recorded)
#' epsg(las)
#'
#' # Get the WKT string stored in the header (LAS >= 1.4)
#' wkt(las)
#'
#' if (rgdal::new_proj_and_gdal())
#' {
#'    # Get the WKT of the CRS
#'    sp::wkt(crs(las))
#'
#'    # Recorded CRS is "NAD83 / UTM zone 17N"
#'    sf::st_crs(las)$input
#' }
#'
#' # Overwrite the CRS (but does not reproject)
#' crs <- sp::CRS("+init=epsg:26918")
#' projection(las) <- crs
#' sf::st_crs(las)$input
#'
#' # Uses the EPSG code
#' projection(las) <- 26919
#' sf::st_crs(las)$input
#'
#' # Uses a crs from sf
#' crs <- sf::st_crs(3035)
#' projection(las) <- crs
#'
#' @importFrom raster projection<-
#' @importFrom raster projection
#' @importFrom raster crs<-
#' @importFrom raster crs
#' @name projection
#' @rdname projection
#' @md
NULL

# ==== EPSG ====

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
setMethod("epsg", "LASheader", function(object, ...)
{
  if (use_wktcs(object))  warning("This LAS object stores the CRS as WKT. 0 returned; use 'wkt()' instead.", call. = FALSE)
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

# ==== WKT =====

# @export
# @rdname projection
#setGeneric("wkt", function(object, ...)
#  standardGeneric("wkt"))

#' @export
#' @rdname projection
setGeneric("wkt<-", function(object, value)
  standardGeneric("wkt<-"))

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


# ==== PROJECTION =====

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

# This is legacy code that should no longer be used because it returns a proj4 string

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
  # The input is either:
  # - a string > proj4 or WKT
  # - a CRS > from sp
  # - a crs > from sf
  # - a number > EPSG code
  # In all case we need to get a WKT or an EPSG code to update the
  # header depending on the LAS format (1.4 or above)
  # In addition we need this to work both with old and new proj and gdal

  proj6 <- rgdal::new_proj_and_gdal()

  if (is(value, "CRS"))
  {
    proj4 <- value@projargs
    CRS <- value
    wkt <- if (proj6) comment(value) else rgdal::showWKT(proj4)
    if (is.null(wkt)) wkt = ""
    epsg <- .find_epsg_code(CRS)
  }
  else if (is(value, "crs"))
  {
    if (proj6)
    {
      wkt <- value$wkt
      CRS <- sp::CRS(SRS_string = wkt)
      proj4 <- CRS@projargs
      epsg <- .find_epsg_code(value)
    }
    else # nocov start
    {
      proj4 <- value$proj4string
      CRS <- sp::CRS(proj4)
      wkt <- rgdal::showWKT
      epsg <- .find_epsg_code(value)
    } # nocov end
  }
  else if (is.character(value))
  {
    CRS <- sp::CRS(SRS_string = value)
    proj4 <- CRS@projargs
    wkt <- if (proj6) comment(CRS) else rgdal::showWKT(proj4)
    epsg <- .find_epsg_code(CRS)
  }
  else if (is.numeric(value))
  {
    epsg <- value
    CRS <- epsg2CRS(epsg)
    proj4 <- CRS@projargs
    wkt <- if (proj6) comment(CRS) else rgdal::showWKT(proj4)
  }
  else
    stop("'value' is not a CRS or a string or a number.")

  if (use_wktcs(x))
  {
    if (is.null(wkt) || wkt == "")
      x@header@VLR[["WKT OGC CS"]] <- NULL
    else
      wkt(x@header) <- wkt

    x@proj4string <- CRS
  }
  else
  {
    if (epsg == 0L)
    {
      if(!is.na(proj4)) warning(paste0("EPSG code not found: header not updated. Try to provide the ESPG code instead of a ", class(value)[1]), call. = FALSE)
      pos <- rlas:::where_is_epsg(as.list(x@header))
      if (pos != 0)  x@header@VLR[["GeoKeyDirectoryTag"]][["tags"]][[pos]] <- NULL
    }
    else
    {
      epsg(x@header) <- epsg
    }

    x@proj4string <- CRS
  }

  return(x)
})

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

# ==== CRS ====

#' @export
#' @rdname projection
setMethod("crs", "LASheader", function(x, asText = FALSE)
{
  return(projection(x, asText))
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
setMethod("crs", "LAScatalog", function(x, asText = FALSE)
{
  return(projection(x, asText))
})

# ===== SPTRANSFORM ====

#' @importMethodsFrom sp spTransform
#' @noRd
setMethod("spTransform", signature("LAS", "CRS"), function(x, CRSobj, ...)
{
  if (is.na(x@proj4string@projargs))
    stop("No transformation possible from NA reference system")

  p <- list(...)

  # Transform the point coordinates
  spts <- sp::SpatialPoints(coordinates(x))
  spts@proj4string <- crs(x)
  spts <- sp::spTransform(spts, CRSobj)
  X <- spts@coords[,1]
  Y <- spts@coords[,2]

  # Update the offsets in the header
  offsetx <- floor(min(X))
  offsety <- floor(min(Y))
  if (!is.null(p$xoffset)) offsetx <- p$xoffset
  if (!is.null(p$yoffset)) offsety <- p$yoffset
  x@header@PHB[["X offset"]] <- offsetx
  x@header@PHB[["Y offset"]] <- offsety

  # Update the scale factors
  scalex <- x@header@PHB[["X scale factor"]]
  scaley <- x@header@PHB[["Y scale factor"]]
  if (!is.null(p$scale)) scalex <- scaley <- p$scale
  x@header@PHB[["X scale factor"]] <- scalex
  x@header@PHB[["Y scale factor"]] <- scaley

  # Quantize the coordinates
  fast_quantization(X, scalex, offsetx)
  fast_quantization(Y, scaley, offsety)

  # Update the LAS object
  x@data[["X"]] <- X
  x@data[["Y"]] <- Y
  x <- lasupdateheader(x)
  crs(x) <- CRSobj

  return(x)
})

# ===== INTERNAL TOOLS =======

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

epsg2CRS <- function(epsg, fail = FALSE)
{
  if (rgdal::new_proj_and_gdal())
  {
    crs <- tryCatch(
    {
      sfcrs <- suppressWarnings(sf::st_crs(paste0("EPSG:", epsg)))
      return(suppressWarnings(sp::CRS(SRS_string = sfcrs$wkt)))
    },
    error = function(e)
    {
      if (!fail)
        return(sp::CRS(NA_character_))
      else
        stop(paste("Invalid epsg code", epsg), call. = FALSE)
    })
  }
  else # nocov start
  {
    proj <- epsg2proj(epsg, fail)
    crs <- sp::CRS(proj)
  } # nocov end

  return(crs)
}

wkt2CRS <- function(wkt, fail = FALSE)
{
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
  }
  else # nocov start
  {
    proj <- wkt2proj(wkt, fail)
    crs <- sp::CRS(proj, doCheckCRSArgs = FALSE) # doCheckCRSArgs = FALSE added in 2.2.4 after #323
  } # nocov end

  return(crs)
}

.find_epsg_code <- function(x)
{
  if (is(x, "CRS"))
  {
    if (is.na(x@projargs))
      return(0L)

    # Extract epsg code if any
    epsg  <- sub("\\+init=epsg:(\\d+).*",  "\\1", x@projargs)
    epsg  <- suppressWarnings(as.integer(epsg))

    if (!is.na(epsg)) return(epsg)

    ## We were not able to extract the epsg code earlier, we can retrieve it
    ## with rgdal and proj4 string
    epsg <- rgdal::showEPSG(x@projargs)

    # We are still unable to find an epsg code
    if (epsg == "OGRERR_UNSUPPORTED_SRS")
      return(0L)
    else
      return(epsg)
  }
  else if (is(x, "crs"))
  {
    if (!is.null(x$epsg))
      return(x$epsg)

    if (is.na(x$input))
      return(0L)

    epsg  <- sub("\\EPSG:(\\d+).*",  "\\1", x$input)
    epsg  <- suppressWarnings(as.integer(epsg))

    if (!is.na(epsg))
      return(epsg)
    else
      return(0L)
  }
  else
    stop("Internal error: x is not a CRS or a crs", call. = FALSE)
}

# nocov start
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
# nocov end
