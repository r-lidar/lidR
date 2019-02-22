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
#' Create an object of class \link[lidR:LAScatalog-class]{LAScatalog} from a folder or a set of filenames.
#' A LAScatalog is a representation of a set of las/laz files. A computer cannot load all the data at
#' once. A \code{LAScatalog} is a simple way to manage all the files sequentially. Most functions from
#' \code{lidR} can be used seamlessly with a LAScatalog using the internal \code{LAScatalog} processing
#' engine. To take advantage of the \code{LAScatalog} processing engine the user must first adjust some
#' processing options using the \link[lidR:catalog_options_tools]{appropriated functions}. Careful
#' reading of the \link[lidR:LAScatalog-class]{LAScatalog class documentation} is required to use the
#' \code{LAScatalog} class correctly.
#'
#' @param folder string. The path of a folder containing a set of las/laz files. Can also be a vector of
#' file paths.
#' @param \dots Extra parameters to \link[base:list.files]{list.files}. Typically `recursive = TRUE`.
#'
#' @return A \code{LAScatalog} object
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
#' # Internal engine will sequentially process regions of interest of size 500 x 500 m (clusters)
#' opt_chunk_size(ctg) <- 500
#'
#' # Internal engine will align the 500 x 500 m clusters on x = 250 and y = 300
#' opt_alignment(ctg) <- c(250, 300)
#'
#' # Internal engine will not display a progress estimation
#' opt_progress(ctg) <- FALSE
#'
#' # Internal engine will not return results into R. Instead it will write results in files.
#' opt_output_files(ctg) <- "/path/to/folder/templated_filename_{XBOTTOM}_{ID}"
#'
#' # More details in the documentation
#' help("LAScatalog-class", "lidR")
#' help("catalog_options_tools", "lidR")
#' }
catalog <- function(folder, ...)
{
  assert_is_character(folder)

  finfo <- file.info(folder)

  if (all(!finfo$isdir))
    files <- normalizePath(folder)
  else if (!dir.exists(folder))
    stop(glue::glue("{folder} does not exist."))
  else
    files <- list.files(folder, full.names = T, pattern = "(?i)\\.la(s|z)$", ...)

  verbose("Reading files...")

  header <- LASheader(rlas::read.lasheader(files[1]))
  crs    <- projection(header, asText = FALSE)

  headers <- lapply(files, function(x)
  {
    header        <- LASheader(rlas::read.lasheader(x))
    epsg          <- epsg(header)
    PHB           <- header@PHB
    PHB$EPSG      <- epsg
    names(PHB)    <- make.names(names(PHB))
    names(PHB)[4] <- "GUID"

    # Compatibility with rlas 1.3.0
    if (!is.null( PHB[["Number.of.points.by.return"]]))
    {
      PHB[["Number.of.1st.return"]] <- PHB[["Number.of.points.by.return"]][1]
      PHB[["Number.of.2nd.return"]] <- PHB[["Number.of.points.by.return"]][2]
      PHB[["Number.of.3rd.return"]] <- PHB[["Number.of.points.by.return"]][3]
      PHB[["Number.of.4th.return"]] <- PHB[["Number.of.points.by.return"]][4]
      PHB[["Number.of.5th.return"]] <- PHB[["Number.of.points.by.return"]][5]
      PHB[["Number.of.points.by.return"]] <- NULL
      PHB[["Global.Encoding"]] <- NULL
    }

    return(PHB)
  })

  headers <- data.table::rbindlist(headers)
  headers$filename <- files

  xmin <- headers$Min.X
  xmax <- headers$Max.X
  ymin <- headers$Min.Y
  ymax <- headers$Max.Y
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

  if (is.overlapping(res))
    message("Be careful, some tiles seem to overlap each other. lidR may return incorrect outputs with edge artifacts when processing this catalog.")

  #if (!is.indexed(res))
    #message("las or laz files are not associated with lax files. This is not mandatory but may greatly speed up some computations. See help('writelax', 'rlas').")

  return(res)
}

#' @rdname extent
#' @export
#' @importMethodsFrom raster extent
setMethod("extent", "LAScatalog",
          function(x, ...) {
            return(raster::extent(min(x@data$Min.X), max(x@data$Max.X), min(x@data$Min.Y), max(x@data$Max.Y)))
          }
)

#' @param ... Unused
#' @param drop Unused
#' @rdname redefined_behaviors
#' @export
setMethod("[", "LAScatalog", function(x, i, j, ..., drop = TRUE) {

  ctgname <- deparse(substitute(x))
  iname   <- deparse(substitute(i))
  nargs   <- nargs()

  if (!missing(i) & !missing(j))
    stop(glue::glue("This action is not allowed for a {class(x)}. j must be missing. Maybe you meant: {ctgname}[{iname}, ]."), call. = FALSE)

  if (missing(i) & !missing(j))
    stop(glue::glue("This action is not allowed for a {class(x)}. i cannot be missing."), call. = FALSE)

  if (!missing(i) & missing(j) & nargs == 2L)
    stop(glue::glue("This action is not allowed for a {class(x)}. Maybe you meant: {ctgname}[{iname}, ]."), call. = FALSE)

  y <- callNextMethod()

  new_ctg <- new("LAScatalog")
  new_ctg@chunk_options <- x@chunk_options
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
  stop("LAScatalog data are read from standard files and cannot be modified", call. = FALSE)
})

#' @rdname redefined_behaviors
#' @export
setMethod("$<-", "LAScatalog", function(x, name, value)
{
  stop("LAScatalog data are read from standard files and cannot be modified", call. = FALSE)
})
