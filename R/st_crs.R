#' Get or set the projection of a LAS* object
#'
#' Get or set the projection of a `LAS*` object.  `st_crs()` extends `sf:st_crs()`, `projection()` and
#' `crs()` extend `raster:projection()` and `raster:crs()`. `projection()` and `crs()` are provided
#' for backward compatibility. For `epsg()` and `wkt()`, see details.
#'
#' There are two ways to store the CRS of a point cloud in a LAS file:
#'
#'    - Store an EPSG code (for LAS 1.0 to 1.3)
#'    - Store a WTK string (for LAS 1.4)
#'
#' On the other hand, R spatial packages use a `crs` object to store the CRS. This is why the CRS is duplicated
#' in a LAS object. The information belongs within the header in a format that can be written in a
#' LAS file and in the slot `crs`, and also in a format that can be understood by other R packages.
#'
#'    - `st_crs` return the CRS in `sf` format.
#'    - `st_crs<-`: assigns a CRS from a `CRS` (`sp`), a `crs` (`sf`), a WKT
#'    string, a proj4string or an epsg code. It updates the header of the LAS
#'    object either with the EPSG code for LAS formats < 1.4 or with a WKT string
#'    - `epsg`: reads the epsg code from the header.
#'    - `wkt`: reads the WKT string from the header.
#'
#' @param x,object,obj An object of class LAS*
#' @param ... Unused.
#' @param asText logical. If TRUE, the projection is returned as text. Otherwise a CRS object is returned.
#' @param value A `CRS` or a `crs` or a `proj4string` string or WKT string or an EPSG code.
#'
#' @return A `st_crs()` returns a `sf::crs`. `projection()` and `crs()` return a `sp::CRS` and should
#' no longer be used.
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
#' # Overwrite the CRS (but does not reproject)
#' st_crs(las) <- 26918
#' las
#'
#' @importFrom sf st_crs
#' @importFrom sf st_crs<-
#' @name st_crs
#' @md
NULL

#' @export
#' @rdname st_crs
st_crs.LAS = function(x, ...) {
  # Workaround to minimize backward incompatibilities
  if (is_las_v3(x))
    return(sf::st_crs(x@proj4string)) #nocov

  return(x@crs)
}

#' @export
#' @rdname st_crs
st_crs.LAScatalog = function(x, ...)
{
  # Workaround to minimize backward incompatibilities
  if (is_lascatalog_v3(x))
    return(sf::st_crs(x@proj4string)) #nocov

  return(sf::st_crs(x@data))
}

#' @export
#' @rdname st_crs
st_crs.LASheader = function(x, ...)
{
  if (use_epsg(x) && epsg(x) != 0L)
    return(epsg2crs(epsg(x)))

  if (use_wktcs(x) && wkt(x) != "")
    return(wkt2crs(wkt(x)))

  return(sf::NA_crs_)
}

#' @export
#' @rdname st_crs
st_crs.LAScluster = function(x, ...) { return(x@crs) }

#' @export st_crs<-
#' @rdname st_crs
#' @name st_crs<-
NULL

#' @export
#' @rdname st_crs
`st_crs<-.LAS` = function(x, value)
{
  if (is(value, "crs") && value == sf::NA_crs_)
  {
    x@crs <- sf::NA_crs_
    return(x)
  }

  # The input is either:
  # - a string > proj4 or WKT
  # - a CRS > from sp
  # - a crs > from sf
  # - a number > EPSG code
  # In all case we need to get a WKT or an EPSG code to update the
  # header depending on the LAS format (1.4 or above)

  # Workaround to repair LAS v3 and minimize backward incompatibilities with v4
  x <- las_v3_repair(x)

  all   <- all_crs_formats(value)
  #CRS   <- all[["CRS"]]
  crs   <- all[["crs"]]
  wkt   <- all[["wkt"]]
  epsg  <- all[["epsg"]]
  #proj4 <- all[["proj4"]]

  if (is.na(crs)) stop("Invalid CRS as input", call. = FALSE)

  if (is.null(epsg) | is.na(epsg)) epsg = 0

  if (use_wktcs(x))
  {
    if (is.null(wkt) || wkt == "" || is.na(wkt))
      x@header@VLR[["WKT OGC CS"]] <- NULL
    else
      wkt(x@header) <- wkt

    x@crs <- crs
  }
  else
  {
    if (epsg == 0L)
    {
      warning(paste0("EPSG code not found: header not updated. Try to provide the ESPG code instead of a ", class(value)[1]), call. = FALSE)
      pos <- rlas::find_epsg_position(as.list(x@header))
      if (pos != 0)  x@header@VLR[["GeoKeyDirectoryTag"]][["tags"]][[pos]] <- NULL
    }
    else
    {
      epsg(x@header) <- epsg
    }

    x@crs <- crs
  }

  return(x)
}

#' @export
#' @rdname st_crs
`st_crs<-.LASheader` = function(x, value)
{
  crs <- all_crs_formats(value)

  if (use_epsg(x))
    epsg(x) <- crs$epsg
  else
    wkt(x) <- crs$wkt

  return(x)
}

#' @export
#' @rdname st_crs
`st_crs<-.LAScatalog` = function(x, value)
{
  # Workaround to repair LAScatalog v3 and minimize backward incompatibilities with v4
  x <- lascatalog_v3_repair(x)

  x@data <- sf::st_set_crs(x@data, sf::NA_crs_)
  x@data <- sf::st_set_crs(x@data, sf::st_crs(value))
  return(x)
}

# ==== PROJECTION ====

#' @export
#' @rdname st_crs
projection <- function(x, asText = TRUE)
{
  CRS <- as(sf::st_crs(x), "CRS")
  if (asText) return(CRS@projargs) else return(CRS)
}

#' @export
#' @rdname st_crs
"projection<-" <- function(x, value)
{
  if (inherits(x, "BasicRaster"))
    raster::projection(x) <- value
  else
    sf::st_crs(x) <- value

  return(x)
}

# ==== CRS ====

#' @importMethodsFrom terra crs
#' @importMethodsFrom terra crs<-
#' @rdname st_crs
setMethod("crs", "LASheader", function(x)
{
  return(projection(x, FALSE))
})

#' @export
#' @rdname st_crs
setMethod("crs", "LAS", function(x)
{
  return(projection(x, FALSE))
})

#' @export
#' @rdname st_crs
setMethod("crs<-", "LAS", function(x, ..., value)
{
  projection(x) <- value
  return(x)
})

#' @export
#' @rdname st_crs
setMethod("crs", "LAScatalog", function(x, asText = FALSE)
{
  return(projection(x, asText))
})

#' @export
#' @rdname st_crs
setMethod("crs", "LAScluster", function(x, asText = FALSE)
{
  return(projection(x, asText))
})

#' @export
#' @rdname st_crs
setMethod("crs<-", "LAScatalog", function(x, ..., value)
{
  projection(x) <- value
  return(x)
})

#' @export
#' @rdname st_crs
setMethod("crs<-", "LASheader", function(x, ..., value)
{
  projection(x) <- value
  return(x)
})

# ==== EPSG ====

#' @export
#' @rdname st_crs
setGeneric("epsg", function(object, ...) standardGeneric("epsg"))

#' @export
#' @rdname st_crs
setGeneric("epsg<-", function(object, value) standardGeneric("epsg<-"))

#' @export
#' @rdname st_crs
setMethod("epsg", "LASheader", function(object, ...)
{
  if (use_wktcs(object))  warning("This LAS object stores the CRS as WKT. CRS field might not be correctly populated, yielding uncertain results; use 'wkt()' instead.", call. = FALSE)
  return(rlas::header_get_epsg(as.list(object)))
})

#' @export
#' @rdname st_crs
setMethod("epsg<-", "LASheader", function(object, value)
{
  if (use_wktcs(object)) stop("This object is not in a format that supports EPSG code. Use a WKT string.", call. = FALSE)
  header <- as.list(object)
  header <- rlas::header_set_epsg(header, value)
  return(LASheader(header))
})

#' @export
#' @rdname st_crs
setMethod("epsg", "LAS", function(object)
{
  return(epsg(object@header))
})

#' @export
#' @rdname st_crs
setMethod("epsg<-", "LAS", function(object, value)
{
  assert_is_a_number(value)
  crs <- epsg2crs(value, fail = TRUE)
  epsg(object@header) <- value
  object@crs <- crs
  return(object)
})

# ==== WKT =====

#' @export
#' @rdname st_crs
setGeneric("wkt", function(obj) standardGeneric("wkt"))

#' @export
#' @rdname st_crs
setGeneric("wkt<-", function(obj, value) standardGeneric("wkt<-"))

#' @export
#' @rdname st_crs
setMethod("wkt", "LASheader", function(obj)
{
  return(rlas::header_get_wktcs(as.list(obj)))
})

#' @export
#' @rdname st_crs
setMethod("wkt<-", "LASheader", function(obj, value)
{
  if (!use_wktcs(obj)) stop("This object is not in a format that supports WKT CRS. Use an EPSG code.", call. = FALSE)
  header <- as.list(obj)
  header <- rlas::header_set_wktcs(header, value)
  return(LASheader(header))
})

#' @export
#' @rdname st_crs
setMethod("wkt", "LAS", function(obj)
{
  return(wkt(obj@header))
})

#' @export
#' @rdname st_crs
setMethod("wkt<-", "LAS", function(obj, value)
{
  assert_is_a_string(value)
  crs <- wkt2crs(value, fail = TRUE)
  wkt(obj@header) <- value
  obj@crs <- crs
  return(obj)
})

# ===== INTERNAL TOOLS =======

use_wktcs <- function(x) {
  UseMethod("use_wktcs", x)
}

use_wktcs.LAS <- function(x) {
  return(x[["Global Encoding"]][["WKT"]])
}

use_wktcs.LASheader <- function(x) {
  return(x@PHB[["Global Encoding"]][["WKT"]])
}

use_epsg <- function(x) {
  UseMethod("use_epsg", x)
}

use_epsg.LAS <- function(x) {
  return(!x[["Global Encoding"]][["WKT"]])
}

use_epsg.LASheader <- function(x) {
  return(!x@PHB[["Global Encoding"]][["WKT"]])
}

all_crs_formats = function(value)
{
  crs <- sf::st_crs(value)

  if (is(value, "CRS"))
  {
    #proj4 <- value@projargs
    #CRS <- as(crs, "CRS")
    wkt <- crs$wkt
    epsg <- crs$epsg
  }
  else if (is(value, "crs"))
  {
    wkt <- value$wkt
    #CRS <- as(value, "CRS")
    #proj4 <- CRS@projargs
    epsg <- crs$epsg
  }
  else if (is.character(value))
  {
    #CRS <- as(crs, "CRS")
    #proj4 <- CRS@projargs
    wkt <- crs$wkt
    epsg <- crs$epsg
  }
  else if (is.numeric(value))
  {
    epsg <- value
    #CRS <- as(crs, "CRS")
    #proj4 <- CRS@projargs
    wkt <- crs$wkt
  }
  else
    stop("'value' is not a CRS, a crs or a string or a number.", call. = FALSE)

  return(list(epsg = epsg, crs = crs, wkt = wkt))
  #return(list(epsg = epsg, CRS = CRS, crs = crs, proj4 = proj4, wkt = wkt))
}

epsg2crs <- function(epsg, fail = FALSE)
{
  crs <- tryCatch(
    {
      crs <- suppressMessages(suppressWarnings(sf::st_crs(epsg)))
      if(is.na(crs)) stop("Error")
      return(crs)
    },
    error = function(e)
    {
      if (!fail)
        return(sf::NA_crs_)
      else
        stop(paste("Invalid epsg code", epsg), call. = FALSE)
    })

  return(crs)
}

wkt2crs <- function(wkt, fail = FALSE)
{
  crs <- tryCatch(
    {
      sf::st_crs(wkt)
    },
    error = function(e)
    {
      if (!fail)
        return(sf::NA_crs_)
      else
        stop("Invalid WKT string", call. = FALSE)
    })

  return(crs)
}


