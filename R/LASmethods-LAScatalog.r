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

#' Build a catalog of las tiles/files
#'
#' Build a \link[lidR:LAScatalog-class]{LAScatalog} object from a folder name. A catalog is the
#' representation of a set of las files, since a computer cannot load all the data at once. A
#' catalog is a simple way to manage all the files sequentially by reading only the headers. Also a
#' catalog contains metadata so users can configure how it will be processed.
#'
#' @param folder string. The path of a folder containing a set of .las files
#' @param \dots Extra parameters to \link[base:list.files]{list.files}. Typically `recursive = TRUE`.
#' @param ctg A LAScatalog object.
#' @param value An appropriated value for catalog settings. See \link[lidR:LAScatalog-class]{LAScatalog}
#' @return A \code{LAScatalog} object
#' @export
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

#' @rdname catalog
#' @export
cores = function(ctg)
{
  return(ctg@processing_options$cores)
}


#' @rdname catalog
#' @export
`cores<-` = function(ctg, value)
{
  sys.cores = future::availableCores()
  value = as.integer(value)

  if(value > sys.cores) {
    message(glue::glue("Available cores: {sys.cores}. Number of cores set to {sys.cores}."))
    value = sys.cores
  }

  if(value < 1) {
    message("Number of cores must be positive. Number of cores set to 1.")
    value = 1L
  }

  ctg@processing_options$cores <- value
  return(ctg)
}

#' @rdname catalog
#' @export
by_file = function(ctg)
{
  return(ctg@clustering_options$by_file)
}

#' @rdname catalog
#' @export
`by_file<-` = function(ctg, value)
{
  stopifnot(is.logical(value), length(value) == 1)
  ctg@clustering_options$by_file <- value
  return(ctg)
}

#' @rdname catalog
#' @export
buffer = function(ctg)
{
  return(ctg@clustering_options$buffer)
}

#' @rdname catalog
#' @export
`buffer<-` = function(ctg, value)
{
  assertive::assert_is_a_number(value)
  if (value < 0) message("Negative buffers are allowed in lidR but you should do that cautiously!")
  ctg@clustering_options$buffer <- value
  return(ctg)
}

#' @rdname catalog
#' @export
progress = function(ctg)
{
  return(ctg@processing_options$progress)
}

#' @rdname catalog
#' @export
`progress<-` = function(ctg, value)
{
  assertive::assert_is_a_bool(value)
  ctg@processing_options$progress <- value
  return(ctg)
}

#' @rdname catalog
#' @export
tiling_size = function(ctg)
{
  return(ctg@clustering_options$tiling_size)
}

#' @rdname catalog
#' @export
`tiling_size<-` = function(ctg, value)
{
  assertive::assert_is_a_number(value)
  assertive::assert_all_are_non_negative(value)
  ctg@clustering_options$tiling_size <- value
  return(ctg)
}

#' @rdname catalog
#' @export
vrt = function(ctg)
{
  return(ctg@output_options$output_dir)
}

#' @rdname catalog
#' @export
`vrt<-` = function(ctg, value)
{
  assertive::assert_is_a_string(value)
  ctg@output_options$output_dir <- value
  return(ctg)
}

#' @rdname catalog
#' @export
stop_early = function(ctg)
{
  return(ctg@processing_options$stop_early)
}

#' @rdname catalog
#' @export
`stop_early<-` = function(ctg, value)
{
  assertive::assert_is_a_bool(value)
  ctg@processing_options$stop_early <- value
  return(ctg)
}

save_vrt = function(ctg)
{
  vrt(ctg) != ""
}

#' @rdname catalog
#' @export
`output_files<-` = function(ctg, value)
{
  assertive::assert_is_a_string(value)
  ctg@output_options$output_files <- value
  return(ctg)
}

#' @rdname catalog
#' @export
output_files = function(ctg)
{
  return(ctg@output_options$output_files)
}

#' @rdname catalog
#' @export
laz_compression = function(ctg)
{
  return(ctg@output_options$drivers$LAS$laz_compression)
}

#' @rdname catalog
#' @export
`laz_compression<-` = function(ctg, value)
{
  assertive::assert_is_a_bool(value)
  ctg@output_options$drivers$LAS$laz_compression <- value
  return(ctg)
}

setMethod("show", "LAScatalog", function(object)
{
  surface <- raster::area(object)
  npoints <- sum(object@data$`Number of point records`)
  inherit <- getClass("LAScatalog")@contains[[1]]@superClass
  ext     <- raster::extent(object)

  cat("class       : ", class(object), " (inherit ", inherit, ")\n", sep = "")
  cat("extent      :", ext@xmin, ",", ext@xmax, ",", ext@ymin, ",", ext@ymax, "(xmin, xmax, ymin, ymax)\n")
  cat("coord. ref. :", object@proj4string@projargs, "\n")
  cat("area        :", surface, "units\u00B2\n")
  cat("points      :", npoints, "points\n")
  cat("density     :", round(npoints/surface, 1), "points/unit\u00B2\n")
  cat("num. files  :", dim(object@data)[1], "\n")

  # cat("Processing options: \n")
  # if (by_file(object)) cat(" - split the dataset using the original files as tiles\n")
  # else cat(" - split the dataset into", tiling_size(object), "x", tiling_size(object), "m tiles\n")
  # if (buffer(object) != 0) cat(" - each tile has a", buffer(object), "m buffer\n")
  # cat(" - processing done using", cores(object), "core(s) if possible.")
})