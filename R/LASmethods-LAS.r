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

#' @param data a \link[data.table:data.table]{data.table} containing the data of a las or laz file.
#' @param header a \code{list} or a \link[lidR:LASheader-class]{LASheader} containing the header of
#' a las or laz file.
#' @param proj4string projection string of class \link[sp:CRS-class]{CRS-class}.
#' @param check logical. Conformity tests while building the object.
#' @return An object of class \code{LAS}
#' @export
#' @include LASmethods-generic.r
#' @describeIn LAS-class Create objects of class LAS
LAS <- function(data, header = list(), proj4string = sp::CRS(), check = TRUE)
{
  if (is.data.frame(data))
    data.table::setDT(data)

  if (!data.table::is.data.table(data))
    stop("Invalid parameter data in constructor.")

  rlas::is_defined_coordinates(data, "stop")

  if (is(header, "LASheader"))
    header <- as.list(header)

  if (!is.list(header))
    stop("Wrong header object provided.")

  if (length(header) == 0)
    header <- rlas::header_create(data)

  header <- rlas::header_update(header, data)

  if (check & nrow(data) > 0)
  {
    rlas::check_las_validity(header, data)
    rlas::check_las_compliance(header, data)
  }

  header <- LASheader(header)

  if (is.na(proj4string@projargs))
    proj4string <- projection(header, asText = FALSE)

  las             <- new("LAS")
  las@proj4string <- proj4string
  las@bbox        <- with(header@PHB, matrix(c(`Min X`, `Min Y`, `Max X`, `Max Y`), ncol = 2, dimnames = list(c("x", "y"), c("min", "max"))))
  las@header      <- header
  las@data        <- data

  return(las)
}

setMethod("show", "LAS", function(object)
{
  size <- format(utils::object.size(object), units = "auto")
  surf <- area(object)
  npts <- nrow(object@data)
  dpts <- if (surf > 0) npts/surf else 0
  attr <- names(object@data)
  ext  <- sp::bbox(object)
  phb  <- object@header@PHB

  units <- regmatches(object@proj4string@projargs, regexpr("(?<=units=).*?(?=\\s)", object@proj4string@projargs, perl = TRUE))
  units <- if (length(units) == 0) "units" else units

  cat("class        : LAS (", phb$`File Signature`, " v", phb$`Version Major`, ".", phb$`Version Minor`, ")\n", sep = "")
  cat("point format : ", phb$`Point Data Format ID`, "\n", sep = "")
  cat("memory       :", size, "\n")
  cat("extent       :", ext[1,1], ", ", ext[1,2], ", ", ext[2,1], ", ", ext[2,2], " (xmin, xmax, ymin, ymax)\n", sep = "")
  cat("coord. ref.  :", object@proj4string@projargs, "\n")
  cat("area         : ", surf, " ", units, "\u00B2 (convex hull)\n", sep = "")
  cat("points       :", npts, "points\n")
  cat("density      : ", round(dpts, 2), " points/", units, "\u00B2\n", sep = "")
  cat("names        :", attr, "\n")
})

#' @export
#' @rdname is.empty
setMethod("is.empty", "LAS", function(object, ...)
{
  empty = if (nrow(object@data) == 0) TRUE else FALSE
  return(empty)
})


#' @rdname print
#' @aliases summary
#' @export
setMethod("summary", "LAS", function(object, ...)
{
  print(object)
  print(object@header)
})

#' @rdname print
#' @export
setMethod("print", "LAS", function(x)
{
  show(x)
})


#' Extent
#'
#' Returns an Extent object of a \code{LAS*}.
#'
#' @rdname extent
#' @param x An object of the class \code{LAS} or \code{LAScatalog}
#' @param \dots Unused
#' @return Extent object from \pkg{raster}
#' @seealso \code{\link[raster:extent]{raster::extent}}
#' @export
#' @importMethodsFrom raster extent
setMethod("extent", "LAS",
  function(x, ...) {
    return(raster::extent(min(x@data$X), max(x@data$X), min(x@data$Y), max(x@data$Y)))
  }
)


#' Inherited but modified methods from sp
#'
#' \code{LAS*} objects are \link[sp:Spatial-class]{Spatial} objects so they inherit several methods
#' from \code{sp}. However, some have modified behaviors to prevent some irrelevant modifications. Indeed,
#' a \code{LAS*} object cannot contain anything, as the content is restricted by the LAS specifications.
#' If a user attempts to use one of these functions inappropriately an informative error will be thrown.
#'
#' @param x A \code{LAS*} object
#' @param name A literal character string or a name (possibly backtick quoted).
#' @param value typically an array-like R object of a similar class as x.
#' @export
#' @rdname redefined_behaviors
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' las$Z = 2L
#' las[["Z"]] = 1:10
#' las$NewCol = 0
#' las[["NewCol"]] = 0
#' }
setMethod("$<-", "LAS", function(x, name, value)
{
  if (!name %in% names(x@data))
    stop("Addition of a new column using $ is forbidden for LAS objects. See ?lasadddata", call. = FALSE)

  if (name %in% LASFIELDS)
  {
    type1 <- storage.mode(x@data[[name]])
    type2 <- storage.mode(value)

    if (type1 != type2)
      stop(glue::glue("Trying to replace data of type {type1} by data of type {type2}: this action is not allowed"), call. = FALSE)
  }

  x@data[[name]] = value
  return(x)
})

#' @param i string, name of elements to extract or replace.
#' @param j Unused.
#' @rdname redefined_behaviors
#' @export
setMethod("[[<-", c("LAS", "ANY", "missing", "ANY"),  function(x, i, j, value)
{
  if (!i %in% names(x@data))
    stop("Addition of a new column using [[ is forbidden for LAS objects. See ?lasadddata", call. = FALSE)

  if (i %in% LASFIELDS)
  {
    type1 <- storage.mode(x@data[[i]])
    type2 <- storage.mode(value)

    if (type1 != type2)
      stop(glue::glue("Trying to replace data of type {type1} by data of type {type2}: this action is not allowed"), call. = FALSE)
  }

  x@data[[i]] = value
  return(x)
})

#' @export
#' @rdname area
setMethod("area", "LAS", function(x, ...)
{
  if (nrow(x@data) == 0)
    return(0)

  return(area_convex_hull(x@data$X, x@data$Y))
})

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

  proj4 <- gsub("\\+init=epsg:\\d+\\s", "", proj4)

  if (x@header@PHB[["Global Encoding"]][["WKT"]] == TRUE)
  {
    wkt <- rgdal::showWKT(proj4)
    wkt(x@header) <- wkt
    raster::projection(x) <- proj4
    return(x)
  }
  else
  {
    epsg <- rgdal::showEPSG(proj4)

    if (epsg == "OGRERR_UNSUPPORTED_SRS")
      stop("EPSG not found. Try to use the function epsg() manually.", call. = FALSE)

    epsg(x@header) <- epsg
    raster::projection(x) <- proj4
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

#' @rdname plot
setMethod("plot", signature(x = "LAS", y = "missing"), function(x, y, color = "Z", colorPalette = height.colors(50), bg = "black", trim = Inf, backend = c("rgl", "pcv"), clear_artifacts = TRUE, nbits = 16, ...)
{
  plot.LAS(x, y, color, colorPalette, bg, trim, backend, clear_artifacts, nbits, ...)
})

plot.LAS = function(x, y, color = "Z", colorPalette = height.colors(50), bg = "black", trim = Inf, backend = c("rgl", "pcv"), clear_artifacts = TRUE, nbits = 16, ...)
{
  if (is.empty(x)) stop("Cannot display an empty point cloud")

  col <- lazyeval::expr_text(color)
  if (substr(col, 1, 1) != "\"") color <- col

  backend <- match.arg(backend)
  pcv     <- "PointCloudViewer" %in% rownames(utils::installed.packages())

  if (backend == "pcv" & !pcv)    stop("'PointCloudViewer' package is needed. Please read documentation.")
  if (length(color) > 1)          stop("'color' should contain a single value.")

  if (color != "RGB" & !color %in% names(x@data))
    stop("'color' should refer to an attribute of the LAS data.")

  if (color == "RGB")
  {
    if (backend == "pcv")
    {
      coldata  = "RGB"
    }
    else
    {
      if (!all(c("R", "G", "B") %in% names(x@data))) stop("No 'RGB' attributes found.")

      maxcol  <- 2^nbits - 1
      coldata <- grDevices::rgb(x@data$R/maxcol, x@data$G/maxcol, x@data$B/maxcol)
    }
  }
  else
    coldata <- x@data[[color]]

  args <- list(...)
  if (is.null(args$size))
    args$size <- 1.5

  if (backend == "rgl")
  {
    if (is.numeric(coldata))
      args$col <- set.colors(coldata, colorPalette, trim)
    else if (is.character(coldata))
      args$col <- coldata
    else if (is.logical(coldata))
      args$col <- set.colors(as.numeric(coldata), colorPalette)

    args$col[is.na(args$col)] <- "lightgray"

    return(.plot_with_rgl(x, bg, coldata, clear_artifacts, args))
  }
  else
  {
    if (!is.infinite(trim)) coldata[coldata > trim] <- trim
    return(.plot_with_pcv(x, coldata, colorPalette, args))
  }
}

.plot_with_rgl = function(x, bg, coldata, clear_artifacts, args)
{
  if (clear_artifacts)
  {
    minx = min(x@data$X)
    miny = min(x@data$Y)
    with = c(list(x = x@data$X - minx, y = x@data$Y - miny, z = x@data$Z), args)
  }
  else
  {
    with = c(list(x = x@data$X, y = x@data$Y, z = x@data$Z), args)
  }

  rgl::open3d()
  rgl::rgl.bg(color = bg)
  do.call(rgl::points3d, with)

  if (clear_artifacts)
    return(invisible(c(minx, miny)))
  else
    return(invisible(c(0,0)))
}

.plot_with_pcv = function(x, coldata, colors, args)
{
  if (is.character(coldata))
  {
    if (coldata == "RGB")
      eval(parse(text = "PointCloudViewer::plot_xyzrgb(x@data$X, x@data$Y, x@data$Z, x@data$R, x@data$G, x@data$B, args$size)"))
    else
      stop("Unexpected error.")
  }
  else
  {
    id = cut(coldata, length(colors), labels = FALSE)
    eval(parse(text = "PointCloudViewer::plot_xyzcol(x@data$X, x@data$Y, x@data$Z, colors, id, args$size)"))
  }

  return(invisible())
}
