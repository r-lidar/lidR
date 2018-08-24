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
#' all the data at once. A \code{LAScatalog} is a simple way to manage the entire dataset by reading only
#' the file headers. It enables the user to process a large area or to selectively clip data from a
#' large area without loading the large area itself. A \code{LAScatalog} can be built with the function
#' \link{catalog} and is formally an extension of \code{SpatialPolygonsDataFrame}. Thus, \strong{it is}
#' a \code{SpatialPolygonsDataFrame} that contains extra information that enables users to control
#'  how the catalog is processed (see details).
#'
#' A \code{LAScatalog} is formally a  \code{SpatialPolygonsDataFrame} extended with 3 new slots that
#' contain processing options. Each \code{lidR} function that supports a \code{LAScatalog} as
#' input will respect these processing options when it is relevant (see documentation of each repective
#' function). Internally, processing a catalog is almost always the same and relies on few steps:
#' \enumerate{
#' \item Create a set of clusters. A cluster is the representation of a region of interest.
#' \item Loop over each cluster (in parallel or not)
#' \item For each cluster, load the points inside the region of interest in R, run some R functions,
#' return the expected output.
#' \item Merge the outputs of the different clusters once they are all processed to build a continuous
#' output.
#' }
#' So basically, a \code{LAScatalog} is a built-in batch process with the specificity that \code{lidR}
#' does not loop through files but loops seamlessly through clusters that do not not necessarily match
#' with the files pattern. This way \code{lidR} can process sequentially tiny regions of interest even
#' if each file may be individually too big to fit in memory. This is also why point cloud indexation
#' with lax files may significantly speed-up the processing.\cr\cr
#' It is important to note that buffered datasets (i.e. files that overlap each other) are not natively
#' supported by \code{lidR}. When encountering such datasets the user should always filter the
#' overlap if possible. This is possible if the overlapping points are flagged, for example in the
#' 'withheld' field. Otherwise \code{lidR} will not be able to process the dataset correctly.
#'
#' @section Processing options:
#' The slot \code{@processing_options} contains a \code{list} of options that drives how a the cluster
#' (the sub-areas that are sequentially processed) are processed.
#' \itemize{
#' \item \strong{core}: interger. How many cores are used. Default is 1.
#' \item \strong{progress}: boolean. Display a progress bar and a chart of progress. Default is TRUE.
#' \item \strong{stop_early}: boolean. Stop the processsing before the end if an error occur in one of the clusters.
#' Default is TRUE.
#' }
#'
#' @section Clustering options:
#' The slot \code{@clustering_options} contains a \code{list} of options that drives how a the cluster
#' (the sub-areas that are sequentially processed) are made.
#' \itemize{
#' \item \strong{by_file}: boolean. The catalog is process sequentially by file. A clsuter is a file. Default is FALSE.
#' \item \strong{tiling_size}: numeric. The size of the cluster that will be sequentially processeed. A small size
#' allows for loading few data at a time saving computer memory. A large size allows for loading large
#' region at a time, the computation is thus ussualy faster but uses much more computer memory. Not relevent
#' if \code{by_file = TRUE}
#' \item \strong{buffer}: numeric. Each cluster can be read with an extra buffer around it to ensure there is
#' no side effect between to independent cluster and that the output is correct and continuous. This
#' is mandatory for some algorithms. Default is 0.
#' \item \strong{alignment}: numeric. A vector of size 2 (x and y coordinates, respectively) to align the
#' clustering pattern. By default the alignment is made along (0,0) meaning the edgeof a virtual tile
#' will belong on x = 0 and y = 0 and all the the others will be multiples of the tiling size. Not relevent
#' if \code{by_file = TRUE}
#' }
#'
#' @section Output options:
#' The slot \code{@output_options} contains a \code{list} of options that drives how a the cluster
#' (the sub-areas that are sequentially processed) are written (and by written we mean written in files
#' or written in R memory).
#'
#' \itemize{
#' \item \strong{output_files}: string. A complete path to a templated filename without extension (the
#' algorithm guess it for you). When several files are going to be written a single string is provided
#' with a template that is automatically fullfiled. For example this names are possible:
#' \preformatted{
#' "/home/user/als/normalized/file_{ID}_segmented"
#' "C:/user/document/als/zone52_{XLEFT}_{YBOTTOM}_confidential"
#' "C:/user/document/als/{ORIGINALFILNAME}_normalized"
#' }
#' And will generate as many files as needed with custom names for each file. The list of allowed
#' templates is described in the documentation of each function.
#' \item \strong{drivers}: list. This contains all the drivers requieres to write seamlessly Raster*,
#' Spatial*, LAS objects. This don't need to be changed if the user is not an advanced user.
#' }
#'
#' @slot processing_options list. A list that contains some settings describing how the catalog will be
#' processed (see dedicated section).
#' @slot clustering_options list. A list that contains some settings describing how the catalog will be
#' sub-divided into small cluster to be processed (see dedicated section).
#' @slot output_options list. A list that contains some settings describing how the catalog will return
#' the outputs (see dedicated section).
#' @seealso
#' \link[lidR:catalog]{catalog}
#' @import data.table
#' @import methods
#' @include class-lasheader.r
#' @importClassesFrom sp CRS
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @export
#' @useDynLib lidR, .registration = TRUE
setClass(
  Class = "LAScatalog",
  contains = "SpatialPolygonsDataFrame",
  representation(
    clustering_options = "list",
    processing_options = "list",
    output_options = "list"
  )
)

setMethod("initialize", "LAScatalog", function(.Object)
{
  callNextMethod()

  drivers = list(
    Raster = list(
      write = raster::writeRaster,
      format = "GTiff"
    ),
    LAS = list(
      write = lidR::writeLAS,
      laz_compression = FALSE
    ),
    Spatial = list(
      write = rgdal::writeOGR
    )
  )

  .Object@clustering_options <- list(
    by_file = FALSE,
    tiling_size = 500,
    buffer = 0,
    alignment = c(0,0)
  )
  .Object@processing_options <- list(
    cores = 1L,
    progress = TRUE,
    stop_early = TRUE
  )

  .Object@output_options <- list(
    output_dir = "",
    output_files = "",
    save_with_buffer = FALSE,
    merge_files = FALSE,
    drivers = drivers
  )

  return(.Object)
})

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

  # # Test for overlaps
  # contour = rgeos::gUnaryUnion(res)
  #
  # actual_area = round(contour@polygons[[1]]@area, 4)
  # measured_area = round(area(res), 4)
  #
  # if (actual_area < measured_area)
  #   message("Be careful, some tiles seem to overlap each other. lidR may return incorrect outputs with edge artifacts when processing this catalog.")
  #
  # laxfiles <- paste0(tools::file_path_sans_ext(files), ".lax")
  # if (any(!file.exists(laxfiles)))
  #   message("las or laz files are not associated with lax files. This is not mandatory but may greatly speed up some computations. See help('writelax', 'rlas').")

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
  callNextMethod(object)
  memsize <- format(utils::object.size(object), units = "auto")
  surface <- raster::area(object)
  npoints <- sum(object@data$`Number of point records`)

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