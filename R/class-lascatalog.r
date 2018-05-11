# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017 Jean-Romain Roussel
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



#' An S4 class to represent a set of .las or .laz files
#'
#' A \code{LAScatalog} object is a representation of a set of las/laz files, since a computer cannot load
#' all the data at once. A \code{LAScatalog} is a simple way to manage the entire file by reading only
#' the headers. A \code{LAScatalog} enables the user to process a large area or to selectively clip data
#' from a large area without loading the large area itself. A \code{LAScatalog} can be built with the
#' function \link{catalog}. Also a \code{LAScatalog} contains extra information that enables users to
#' control how the catalog is processed.
#'
#' @slot data data.table. A table representing the header of each file.
#' @slot crs A \link[sp:CRS]{CRS} object.
#' @slot cores integer. Numer of cores used to make parallel computations in compatible functions that
#' support a \code{LAScatalog} as input. Default is 1.
#' @slot buffer numeric. When applying a function to an entire catalog by sequentially processing
#' sub-areas (clusters) some algorithms (such as \link{grid_terrain}) require a buffer around the area
#' to avoid edge effects. Default is 15 m.
#' @slot progress logical. Display an estimation of progress while processing. Default is TRUE.
#' @slot by_file logical. This option overwrites the option \code{tiling_size}. Instead of processing
#' the catalog by arbitrary split areas, it forces processing by file. Buffering is still available.
#' Default is FALSE.
#' @slot tiling_size numeric. To process an entire catalog, the algorithm splits the dataset into
#' several square sub-areas (clusters) to process them sequentially. This is the size of each square
#' cluster. Default is 1000 (1 km^2).
#' @slot vrt character. Path to a folder. In \code{grid_*} functions, for large outputs, the functions
#' can return a lightweight virtual raster mosaic (VRT).
#' @slot opt_changed Internal use only for compatibility with older deprecated code.
#' @seealso
#' \link[lidR:catalog]{catalog}
#' @import data.table
#' @import methods
#' @include class-lasheader.r
#' @importClassesFrom sp CRS
#' @exportClass LAS
#' @useDynLib lidR, .registration = TRUE
setClass(
  Class = "LAScatalog",
  representation(
    data = "data.table",
    crs  = "CRS",
    cores = "integer",
    buffer = "numeric",
    by_file = "logical",
    progress = "logical",
    tiling_size = "numeric",
    vrt = "character",
    opt_changed = "logical"
  )
)

setMethod("initialize", "LAScatalog", function(.Object, data, crs, process = list())
{
  .Object@data  <- data
  .Object@crs   <- crs
  .Object@cores <- 1L
  .Object@buffer <- 15
  .Object@by_file <- FALSE
  .Object@progress <- TRUE
  .Object@tiling_size <- 1000
  .Object@vrt <- ""
  .Object@opt_changed <- FALSE
  return(.Object)
})

#' Build a catalog of las tiles/files
#'
#' Build a \link[lidR:LAScatalog-class]{LAScatalog} object from a folder name. A catalog is the
#' representation of a set of las files, since a computer cannot load all the data at once. A
#' catalog is a simple way to manage all the file sequentially by reading only the headers. Also a
#' catalog contains metadata tso users can configure how it should be processed.
#' @param folder string. The path of a folder containing a set of .las files
#' @param \dots Extra parameters to \link[base:list.files]{list.files}. Typically `recursive = TRUE`.
#' @param ctg A LAScatalog object.
#' @param value An appropriated value for catalog settings. See \link[lidR:LAScatalog-class]{LAScatalog}
#' @seealso
#' \link{LAScatalog-class}
#' \link[lidR:plot.LAScatalog]{plot}
#' \link{catalog_apply}
#' \link{catalog_queries}
#' @return A \code{LAScatalog} object
#' @export
catalog <- function(folder, ...)
{
  if (!is.character(folder))
    stop("'folder' must be a character string")

  finfo <- file.info(folder)

  if (all(!finfo$isdir))
    files <- folder
  else if (!dir.exists(folder))
    stop(paste(folder, " does not exist"))
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

  laxfiles <- paste0(tools::file_path_sans_ext(files), ".lax")
  if (any(!file.exists(laxfiles)))
    message("las or laz files are not associated with lax files. This is not mandatory but may greatly speed up some computations. See help('writelax', 'rlas').")

  return(new("LAScatalog", headers, crs))
}

#' @rdname catalog
#' @export
cores = function(ctg)
{
  if (!ctg@opt_changed & CATALOGOPTIONS("global_changed"))
    return(CATALOGOPTIONS("multicore"))
  else
    return(ctg@cores)
}


#' @rdname catalog
#' @export
`cores<-` = function(ctg, value)
{
  sys.cores = future::availableCores()
  value = as.integer(value)

  if(value > sys.cores) {
    message(paste0("Available cores: ", sys.cores, ". Number of cores set to ", sys.cores, "."))
    value = sys.cores
  }

  if(value < 1) {
    message("Number of cores must be positive. Number of cores set to 1.")
    value = 1L
  }

  ctg@cores <- value
  ctg@opt_changed <- TRUE
  return(ctg)
}

#' @rdname catalog
#' @export
by_file = function(ctg)
{
  if (!ctg@opt_changed & CATALOGOPTIONS("global_changed"))
    return(CATALOGOPTIONS("by_file"))
  else
    return(ctg@by_file)
}

#' @rdname catalog
#' @export
`by_file<-` = function(ctg, value)
{
  stopifnot(is.logical(value), length(value) == 1)
  ctg@by_file <- value
  ctg@opt_changed <- TRUE
  return(ctg)
}

#' @rdname catalog
#' @export
buffer = function(ctg)
{
  if (!ctg@opt_changed & CATALOGOPTIONS("global_changed"))
    return(CATALOGOPTIONS("buffer"))
  else
    return(ctg@buffer)
}

#' @rdname catalog
#' @export
`buffer<-` = function(ctg, value)
{
  stopifnot(is.numeric(value), value >= 0)
  ctg@buffer <- value
  ctg@opt_changed <- TRUE
  return(ctg)
}

#' @rdname catalog
#' @export
progress = function(ctg)
{
  if (!ctg@opt_changed & CATALOGOPTIONS("global_changed"))
    return(CATALOGOPTIONS("progress"))
  else
    return(ctg@progress)
}

#' @rdname catalog
#' @export
`progress<-` = function(ctg, value)
{
  stopifnot(is.logical(value), length(value) == 1)
  ctg@progress <- value
  ctg@opt_changed <- TRUE
  return(ctg)
}

#' @rdname catalog
#' @export
tiling_size = function(ctg)
{
  if (!ctg@opt_changed & CATALOGOPTIONS("global_changed"))
    return(CATALOGOPTIONS("tiling_size"))
  else
    return(ctg@tiling_size)
}

#' @rdname catalog
#' @export
`tiling_size<-` = function(ctg, value)
{
  stopifnot(is.numeric(value), length(value) == 1)
  ctg@tiling_size <- value
  ctg@opt_changed <- TRUE
  return(ctg)
}

#' @rdname catalog
#' @export
vrt = function(ctg)
{
  if (!ctg@opt_changed & CATALOGOPTIONS("global_changed"))
  {
    if (CATALOGOPTIONS("return_virtual_raster"))
      return(tempdir())
    else
      return("")
  }
  else
    return(ctg@vrt)
}

#' @rdname catalog
#' @export
`vrt<-` = function(ctg, value)
{
  stopifnot(is.character(value), length(value) == 1)
  ctg@vrt <- value
  ctg@opt_changed <- TRUE
  return(ctg)
}

save_vrt = function(ctg)
{
  vrt(ctg) != ""
}


setMethod("show", "LAScatalog", function(object)
{
  memsize <- format(utils::object.size(object), units = "auto")
  surface <- area(object)
  npoints <- sum(object@data$`Number of point records`)
  ext     <- extent(object)

  cat("class       : LAScatalog\n")
  cat("extent      :", ext@xmin, ",", ext@xmax, ",", ext@ymin, ",", ext@ymax, "(xmin, xmax, ymin, ymax)\n")
  cat("area        :", surface, "m\u00B2\n")
  cat("points      :", npoints, "points\n")
  cat("density     :", round(npoints/surface, 1), "points/m\u00B2\n")
  cat("num. files  :", dim(object@data)[1], "\n")
  cat("coord. ref. :", object@crs@projargs, "\n")
  cat("Processing options: \n")
  if (by_file(object)) cat(" - split the dataset using the original files as tiles\n")
  else cat(" - split the dataset into", tiling_size(object), "x", tiling_size(object), "m tiles\n")
  if (buffer(object) > 0) cat(" - each tile has a", buffer(object), "m buffer\n")
  cat(" - processing done using", cores(object), "core(s) if possible.")

})

catalog_old_compatibility = function(ctg)
{
  ctg2 = ctg

  c = cores(ctg)
  b = buffer(ctg)
  f = by_file(ctg)
  p = progress(ctg)
  t = tiling_size(ctg)
  v = vrt(ctg)

  cores(ctg2) <- c
  buffer(ctg2) <- b
  by_file(ctg2) <- f
  progress(ctg2) <- p
  tiling_size(ctg2) <- t
  vrt(ctg2) <- v

  return(ctg2)
}
