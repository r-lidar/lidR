# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017-2018 Jean-Romain Roussel
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

#' Create an object of class LAScatalog
#'
#' Create an object of class \link[lidR:LAScatalog-class]{LAScatalog} from a folder or a set of filename.
#' A LAScatalog is a representation of a set of las/laz files. A computer cannot load all the data at
#' once. A \code{LAScatalog} is a simple way to manage all the files sequentially. Most function from
#' \code{lidR} can seamlessly be used with a LAScatalog using the internal \code{LAScatalog} processing
#' engine. To take advantage of the \code{LAScatalog} processing engine the user must first adjust some
#' processing options using the \link[lidR:catalog_options_tools]{appropriated functions}. The careful
#' reading of the \link[lidR:LAScatalog-class]{LAScatalog class documentation} is requiered to use the
#' \code{LAScatalog} class correclty.
#'
#' @param folder string. The path of a folder containing a set of las/laz files. Can also be a vector of
#' file paths.
#' @param \dots Extra parameters to \link[base:list.files]{list.files}. Typically `recursive = TRUE`.
#'
#' @return A \code{LAScatalog} object
#'
#' @include LASmethods-generic.r
#'
#' @export
#'
#' @examples
#' # A single file LAScatalog using data provided with the package
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' ctg = catalog(LASfile)
#' plot(ctg)
#'
#' \dontrun{
#' ctg <- catalog("/path/to/a/folder/of/las/files")
#'
#' # Internal engine will compute in parallel using two cores
#' set_cores(ctg) <- 2L
#'
#' # Internal engine will process sequentially regions of interest of 500 x 500 m (clusters)
#' set_tiling_size(ctg) <- 500
#'
#' # Internal engine will align the 500 x 500 m clusters on x = 250 and y = 300
#' set_alignement(ctg) <- c(250, 300)
#'
#' # Internal engine will not display a progress estimation
#' set_progress(ctg) <- FALSE
#'
#' # Internal engine will not return results into R. Instead it will write results in files.
#' set_output_files(ctg) <- "/path/to/folder/templated_filename_{XBOTTOM}_{ID}"
#'
#' # More details in the documentation
#' help("LAScatalog-class", "lidR")
#' help("catalog_options_tools", "lidR")
#' }
catalog <- function(folder, ...)
{
  assertive::assert_is_character(folder)

  finfo <- file.info(folder)

  if (all(!finfo$isdir))
    files <- folder
  else if (!dir.exists(folder))
    stop(glue::glue("{folder} does not exist."))
  else
    files <- list.files(folder, full.names = T, pattern = "(?i)\\.la(s|z)$", ...)

  verbose("Reading files...")

  header <- LASheader(rlas::read.lasheader(files[1]))
  crs <- epsg2proj(get_epsg(header))

  headers <- lapply(files, function(x)
  {
    header <- rlas::read.lasheader(x)
    header$`Variable Length Records` <- NULL
    data.table::setDT(header)
    return(header)
  })

  headers <- data.table::rbindlist(headers)
  headers$filename <- files

  xmin <- headers$`Min X`
  xmax <- headers$`Max X`
  ymin <- headers$`Min Y`
  ymax <- headers$`Max Y`
  ids  <- as.character(seq_along(files))

  pgeom <- lapply(seq_along(ids), function(xi)
  {
    mtx <- matrix(c(xmin[xi], xmax[xi], ymin[xi], ymax[xi])[c(1, 1, 2, 2, 1, 3, 4, 4, 3, 3)], ncol = 2)
    sp::Polygons(list(sp::Polygon(mtx)), ids[xi])
  })

  Sr = sp::SpatialPolygons(pgeom, proj4string = crs)

  data.table::setDF(headers)

  res <- new("LAScatalog")
  res@bbox <- Sr@bbox
  res@proj4string <- Sr@proj4string
  res@plotOrder <- Sr@plotOrder
  res@data <- headers
  res@polygons <- Sr@polygons

  # Test for overlaps
  contour = rgeos::gUnaryUnion(res)

  actual_area = round(contour@polygons[[1]]@area, 4)
  measured_area = round(area(res), 4)

  if (actual_area < measured_area)
    message("Be careful, some tiles seem to overlap each other. lidR may return incorrect outputs with edge artifacts when processing this catalog.")

  # Test of point indexation
  laxfiles <- paste0(tools::file_path_sans_ext(res@data$filename), ".lax")
  if (any(!file.exists(laxfiles)))
    message("las or laz files are not associated with lax files. This is not mandatory but may greatly speed up some computations. See help('writelax', 'rlas').")

  return(res)
}

setMethod("show", "LAScatalog", function(object)
{
  area    <- raster::area(object)
  area.h  <- area
  npoints <- sum(object@data$`Number of point records`)
  npoints.h <- npoints
  inherit <- getClass("LAScatalog")@contains[[1]]@superClass
  ext     <- raster::extent(object)
  units   <- regmatches(object@proj4string@projargs, regexpr("(?<=units=).*?(?=\\s)", object@proj4string@projargs, perl = TRUE))
  units   <- if (length(units) == 0) "units" else units
  areaprefix <- ""
  pointprefix <- ""
  if (area > 1000*1000)
  {
    areaprefix <- "k"
    area.h  <- round(area/(1000*1000),2)
  }

  if (npoints > 1000 & npoints < 1000^2)
  {
    pointprefix <- "thouthands"
    npoints.h <- round(npoints/1000, 1)
  }
  else if (npoints >= 1000^2 & npoints < 1000^3)
  {
    pointprefix <- "millions"
    npoints.h <- round(npoints/(1000^2),2)
  }
  else if (npoints >= 1000^3)
  {
    pointprefix <- "billions"
    npoints.h <- round(npoints/(1000^3),2)
  }

  cat("class       : ", class(object), " (inherit ", inherit, ")\n", sep = "")
  cat("extent      :", ext@xmin, ",", ext@xmax, ",", ext@ymin, ",", ext@ymax, "(xmin, xmax, ymin, ymax)\n")
  cat("coord. ref. :", object@proj4string@projargs, "\n")
  cat("area        : ", area.h, " ", areaprefix, units, "\u00B2\n", sep="")
  cat("points      :", npoints.h, pointprefix, "points\n")
  cat("density     : ", round(npoints/area, 1), " points/", units, "\u00B2\n", sep = "")
  cat("num. files  :", dim(object@data)[1], "\n")
})

#' @param ... Unused
#' @param drop Unused
#' @rdname redefined_behaviors
#' @export
setMethod("[", "LAScatalog", function(x, i, j, ..., drop = TRUE) {

  ctgname <- deparse(substitute(x))
  iname   <- deparse(substitute(i))
  nargs   <- nargs()

  if (!missing(i) & !missing(j))
    stop(glue::glue("This action is not allowed for a {class(x)}. j must be missing. Maybe you meant: {ctgname}[{iname}, ]."))

  if (missing(i) & !missing(j))
    stop(glue::glue("This action is not allowed for a {class(x)}. i cannot be missing."))

  if (!missing(i) & missing(j) & nargs == 2L)
    stop(glue::glue("This action is not allowed for a {class(x)}. Maybe you meant: {ctgname}[{iname}, ]."))

  y <- callNextMethod()

  new_ctg <- new("LAScatalog")
  new_ctg@clustering_options <- x@clustering_options
  new_ctg@processing_options <- x@processing_options
  new_ctg@output_options     <- x@output_options
  new_ctg@input_options      <- x@input_options
  new_ctg@data               <- y@data
  new_ctg@polygons           <- y@polygons
  new_ctg@plotOrder          <- y@plotOrder
  new_ctg@bbox               <- y@bbox
  new_ctg@proj4string        <- y@proj4string
  return(new_ctg)
})

# #' @rdname redefined_behaviors
# #' @export
# setMethod("[<-", "LAScatalog",  function(x, i, j, value)
# {
#  stop("LAScatalog data are read from standard files and cannot be modified")
# })

#' @rdname redefined_behaviors
#' @export
setMethod("[[<-", "LAScatalog",  function(x, i, j, value)
{
  stop("LAScatalog data are read from standard files and cannot be modified")
})

#' @rdname redefined_behaviors
#' @export
setMethod("$<-", "LAScatalog", function(x, name, value)
{
  stop("LAScatalog data are read from standard files and cannot be modified")
})

#' @rdname area
#' @export
setMethod("area", "LAScatalog",  function(x, ...)
{
  x <- x@data
  area <- sum((x$`Max X` - x$`Min X`) * (x$`Max Y` - x$`Min Y`))
  return(area)
})


#' Plot LAS* objects
#'
#' Plot displays an interactive view for \link[lidR:LAScatalog-class]{LAScatalog} objects with pan and
#' zoom capabilities based on \link[mapview:mapview-package]{mapview}. If the coordinate reference
#' system (CRS) of the \code{LAScatalog} is non empty, the plot can be displayed on top of base maps
#' (satellite data, elevation, street, and so on).
#'
#' @param mapview logical. If \code{FALSE} the catalog is displayed in a regular plot from R base.
#' @method plot LAScatalog
#' @export
#' @examples
#'
#' \dontrun{
#'
#' # single file catalog using data provided in lidR
#' ctg = catalog(LASfile)
#' plot(ctg)
#' }
#' @describeIn plot plot LAScatalog
setMethod("plot", signature(x = "LAScatalog", y = "missing"), function (x, y, mapview = TRUE, ...)
{
  plot.LAScatalog(x, y, mapview, ...)
})

plot.LAScatalog = function(x, y, mapview = TRUE, ...)
{

  if(mapview & !requireNamespace("mapview", quietly = TRUE))
  {
    message("This function can be enhanced by installing the library 'mapview'.")
    mapview = FALSE
  }

  if (mapview)
  {
    LAScatalog = x
    mapview::mapview(LAScatalog, ...)
  }
  else
  {
    param = list(...)

    xmin = min(x@data$`Min X`)
    xmax = max(x@data$`Max X`)
    ymin = min(x@data$`Min Y`)
    ymax = max(x@data$`Max Y`)

    xcenter = (xmin + xmax)/2
    ycenter = (ymin + ymax)/2

    if (is.null(param$xlim)) param$xlim = c(xmin, xmax)
    if (is.null(param$ylim)) param$ylim = c(ymin, ymax)
    if (is.null(param$xlab)) param$xlab = "X"
    if (is.null(param$ylab)) param$ylab = "Y"
    if (is.null(param$asp))  param$xlab = "X"
    if (is.null(param$asp))  param$asp = 1
    if (is.null(param$col))  param$col = "white"

    param$x = xcenter
    param$y = ycenter

    do.call(graphics::plot, param)
    graphics::rect(x@data$`Min X`, x@data$`Min Y`, x@data$`Max X`, x@data$`Max Y`, col = grDevices::rgb(0, 0, 1, alpha=0.1))
  }
}