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
  las@bbox        <- with(header@PHB, matrix(c(`Min X`, `Min Y`, `Max X`, `Max Y`), ncol = 2, dimnames = list(c("x", "y"), c("min", "max"))))
  las@header      <- header
  las@data        <- data
  projection(las) <- proj4string

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
    if (is.na(proj4))
      return(x)

    wkt <- rgdal::showWKT(proj4)
    wkt(x@header) <- wkt
    raster::projection(x) <- proj4
    return(x)
  }
  else
  {
    if (is.na(proj4))
      return(x)

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
setMethod("plot", signature(x = "LAS", y = "missing"), function(x, y, color = "Z", colorPalette = height.colors(50), bg = "black", trim = Inf, backend = c("rgl", "pcv"), clear_artifacts = TRUE, nbits = 16, axis = FALSE, legend = FALSE, ...)
{
  plot.LAS(x, y, color, colorPalette, bg, trim, backend, clear_artifacts, nbits, axis, legend, ...)
})

plot.LAS = function(x, y, color = "Z", colorPalette = height.colors(50), bg = "black", trim = Inf, backend = c("rgl", "pcv"), clear_artifacts = TRUE, nbits = 16, axis = FALSE, legend = FALSE, ...)
{
  backend <- match.arg(backend)
  use_pcv <- backend == "pcv"
  use_rgl <- !use_pcv
  has_pcv <- "PointCloudViewer" %in% rownames(utils::installed.packages())
  has_col <- color %in% names(x@data)
  use_rgb <- color == "RGB"
  has_rgb <- all(c("R", "G", "B") %in% names(x@data))
  maxcol  <- 2^nbits - 1

  if (is.empty(x))         stop("Cannot display an empty point cloud")
  if (use_pcv & !has_pcv)  stop("'PointCloudViewer' package is needed. Please read documentation.")
  if (length(color) > 1)   stop("'color' should contain a single value.")
  if (!use_rgb & !has_col) stop("'color' should refer to an attribute of the LAS data.")
  if (use_rgb & !has_rgb)  stop("No 'RGB' attributes found.")

  if (use_rgb & use_pcv)
    col <- "RGB"
  else if (use_rgb & use_rgl)
    col <- grDevices::rgb(x@data[["R"]]/maxcol, x@data[["G"]]/maxcol, x@data[["B"]]/maxcol)
  else
    col <- x@data[[color]]

  args <- list(...)
  if (is.null(args$size))
    args$size <- 1.5

  if (use_rgl)
    lasplot <- .plot_with_rgl
  else
    lasplot <- .plot_with_pcv

  return(lasplot(las, bg, col, colorPalette, trim, clear_artifacts, axis, legend, args))
}

.plot_with_rgl = function(las, bg, col, pal, trim, clear_artifacts, axis, legend, args)
{
  fg   <- grDevices::col2rgb(bg)
  fg   <- grDevices::rgb(t(255 - fg)/255)
  minx <- min(las@data$X)
  miny <- min(las@data$Y)

  if (is.numeric(col))
  {
    mincol <- min(col)
    maxcol <- min(max(col), trim)
    col <- set.colors(col, pal, trim)
  }
  else if (is.character(col))
  {
    legend <- FALSE
    col <- col
  }
  else if (is.logical(col))
  {
    mincol <- 0
    maxcol <- 1
    col <- set.colors(as.numeric(col), pal)
  }

  col[is.na(col)] <- "lightgray"

  with <- c(list(x = las@data$X, y = las@data$Y, z = las@data$Z, col = col), args)

  if (clear_artifacts)
  {
    with$x <- with$x - minx
    with$y <- with$y - miny
  }

  rgl::open3d()
  rgl::rgl.bg(color = bg)
  do.call(rgl::points3d, with)

  if (axis)
  {
    rgl::axis3d("x", col = fg)
    rgl::axis3d("y", col = fg)
    rgl::axis3d("z", col = fg)
  }

  if (legend)
  {
    f <- .plot_scale_gradient(mincol, maxcol, fg, pal, bg)
    rgl::bg3d(texture = f, col = "white")
  }

  if (clear_artifacts)
    return(invisible(c(minx, miny)))
  else
    return(invisible(c(0,0)))
}

.plot_with_pcv = function(las, bg, col, pal, trim, clear_artifacts, axis, legend, args)
{
  if (is.character(col))
  {
    if (col == "RGB")
      eval(parse(text = "PointCloudViewer::plot_xyzrgb(las@data$X, las@data$Y, las@data$Z, las@data$R, las@data$G, las@data$B, args$size)"))
    else
      stop("Unexpected error.", call. = FALSE)
  }
  else
  {
    if (!is.infinite(trim)) col[col > trim] <- trim
    id <- cut(col, length(pal), labels = FALSE)
    eval(parse(text = "PointCloudViewer::plot_xyzcol(las@data$X, las@data$Y, las@data$Z, pal, id, args$size)"))
  }

  return(invisible(c(0,0)))
}


.plot_scale_gradient = function(min.col, max.col, text.col, scale.col, bg)
{
  f <- tempfile(fileext = ".png")

  png(f, 1920, 1080, bg = bg)
  layout(matrix(1:2, nrow = 1), widths = c(0.9,0.1))
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  plot(0, ann = FALSE, type = "n", axes = FALSE)
  xl <- 1 ; yb <- 1 ; xr <- 1.1 ; yt <- 2
  labels <- pretty(c(min.col, max.col))
  ncol   <- length(scale.col)
  nlab   <- length(labels)
  par(mar = c(5.1, 0.5, 4.1, 0))
  plot(NA, type = "n", ann = FALSE, xlim = c(1,2), ylim = c(1,2), xaxt = "n", yaxt = "n", bty = "n")
  rect(xl, head(seq(yb, yt, (yt - yb)/ncol), -1), xr, tail(seq(yb, yt, (yt - yb)/ncol), -1), col = scale.col, border = NA)
  mtext(labels, side = 2, at = seq(yb, yt, length.out = nlab), las = 2, cex = 1.2, col = text.col)
  dev.off()

  return(f)
}
