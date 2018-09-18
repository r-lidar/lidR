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

#' An S4 class to represent a catalog of .las or .laz files
#'
#' A \code{LAScatalog} object is a representation of a set of las/laz files. A computer cannot load
#' all the data at once. A \code{LAScatalog} is a way to manage and process an entire dataset.  It
#' enables the user to process a large area or to selectively clip data from a large area without
#' loading the large area itself. A \code{LAScatalog} can be built with the function \link{catalog}
#' and is formally an extension of \code{SpatialPolygonsDataFrame}. Thus, \strong{it is}
#' a \code{SpatialPolygonsDataFrame} that contains extra data to enables users to control finely how
#' the catalog is processed (see details).
#'
#' A \code{LAScatalog} is formally a \code{SpatialPolygonsDataFrame} extended with new slots that
#' contain processing options. In \code{lidR}, each function that supports a \code{LAScatalog} as
#' input will respect these processing options. Internally, processing a catalog is almost always the
#' same and relies on few steps:
#' \enumerate{
#' \item Create a set of clusters. A cluster is an arbitrary defined region of interest (ROI) of the
#' catalog. All together the clusters make a wall-to-wall set of ROIs that encompass the whole dataset.
#' \item Loop over each cluster (in parallel or not)
#' \item For each cluster, load the points inside the region of interest in R, run some R functions,
#' return the expected output.
#' \item Merge the outputs of the different clusters once they are all processed to build a continuous
#' wall-to-wall output.
#' }
#' So basically, a \code{LAScatalog} is an object that allows for built-in batch process with the specificity
#' that \code{lidR} does not loop through files but loops seamlessly through clusters that do not not
#' necessarily match with the files pattern. This way \code{lidR} can process sequentially tiny regions
#' of interest even if each file may be individually too big to fit in memory. This is also why point
#' cloud indexation with \code{lax} files may significantly speed-up the processing.\cr\cr
#' It is important to note that buffered catalog (i.e. with files that overlap each other) are not
#' natively supported by \code{lidR}. When encountering such datasets the user should always filter the
#' overlap if possible. This is possible if the overlapping points are flagged, for example in the
#' 'withheld' attribute. Otherwise \code{lidR} will not be able to process the dataset correctly.
#'
#' @slot processing_options list. A list that contains some settings describing how the catalog will be
#' processed (see dedicated section).
#'
#' @slot clustering_options list. A list that contains some settings describing how the catalog will be
#' sub-divided into small cluster to be processed (see dedicated section).
#'
#' @slot output_options list. A list that contains some settings describing how the catalog will return
#' the outputs (see dedicated section).
#'
#' @slot input_options list. A list that contains parameter to pass to \link{readLAS} (see dedicated section).
#'
#' @section Processing options:
#' The slot \code{@processing_options} contains a \code{list} of options that drives how a the cluster
#' (the sub-areas that are sequentially processed) are processed.
#' \itemize{
#' \item \strong{core}: integer. How many cores are used. Default is 1. In \code{lidR} algorithms are
#' all single core algorithms. What is parallelized is the number of clusters process together. Thus
#' by using 4 cores, 4 points cloud are loaded at once using 4 times more memory. Thus it is not
#' always pertinent to set \code{core > 1}.
#' \item \strong{progress}: boolean. Display a progress bar and a chart of progress. Default is TRUE.
#' Progress estimation can be enhanced by installing the package \code{progress}.
#' \item \strong{stop_early}: boolean. Stop the processsing if an error occur in a clusters. If \code{FALSE}
#' the process can run until the end removing cluster that failed. Default is TRUE and the user should
#' not change that.
#' \item \strong{wall.to.wall} logical. The LAScatalog processing engine always guarantee to return a continuous
#' output without edge effect assuming that the catalog is a wall-to-wall catalog. To do so, some options
#' are controlled internally to prevent agains bad settings such as buffer = 0 for some algorithm that
#' requires buffer. In rare case it might be useful to disable these controls. If \code{wall.to.wall = FALSE}
#' controls are disabled and wall-to-wall output are not anymore guaranteed.
#' }
#'
#' @section Clustering options:
#' The slot \code{@clustering_options} contains a \code{list} of options that drives how a the cluster
#' (the sub-areas that are sequentially processed) are made.
#' \itemize{
#' \item \strong{tiling_size}: numeric. The size of the cluster that will be sequentially processed.
#' A small size allows for loading few data at a time saving computer memory. A large size allows for
#' loading large region at a time. The computation is  usually faster but uses much more computer
#' memory. If \code{tiling_size = 0} the catalog is processed sequentially \emph{by file}. A cluster
#' is a file. Default is 0 i.e. by default the processing engine respects existing tiling pattern.
#' \item \strong{buffer}: numeric. Each cluster can be read with an extra buffer around it to ensure there is
#' no side effect between to independent clusters and that the output is a continuous wall-to-wall output.
#' This is mandatory for some algorithms. Default is 30.
#' \item \strong{alignment}: numeric. A vector of size 2 (x and y coordinates, respectively) to align the
#' clustering pattern. By default the alignment is made along (0,0) meaning that the edge of a virtual cluster
#' will belong on x = 0 and y = 0 and all the the others will be multiples of the tiling size. Not relevent
#' if \code{tiling_size = 0}.
#' }
#'
#' @section Output options:
#' The slot \code{@output_options} contains a \code{list} of options that drives how a the cluster
#' (the sub-areas that are sequentially processed) are written. By "written" we mean written in files
#' or written in R memory.
#'
#' \itemize{
#' \item \strong{output_files}: string. If \code{output_files = ""} outputs are returned to R. However
#' if \code{output_files} is a string the outputs will not be returned to R but it will be written in files.
#' This is useful if the output is too big to be returned in R. A path to a templated filename
#' without extension (the algorithm guess it for you) is expected. When several files are going to be
#' written a single string is provided with a template that is automatically fullfilled. For example
#' these file names are possible:
#' \preformatted{
#' "/home/user/als/normalized/file_{ID}_segmented"
#' "C:/user/document/als/zone52_{XLEFT}_{YBOTTOM}_confidential"
#' "C:/user/document/als/{ORIGINALFILNAME}_normalized"
#' }
#' And will generate as many files name as needed with custom names for each file. The list of allowed
#' templates is described in the documentation of each function.
#' \item \strong{drivers}: list. This contains all the drivers requiered to write seamlessly Raster*,
#' Spatial*, LAS objects. This don't need to be changed if the user is not an advanced user.
#' }
#'
#' @section Input options:
#' The slot \code{@input_options} contains a \code{list} of options that are passed to the function
#' \link{readLAS}. Indeed the \code{readLAS} function is not called directly by the user but by the
#' internal processing engine. User can propagate these options throught the \code{LAScatalog} settings.
#'
#' \itemize{
#' \item \strong{select}: string. The \code{select} option. Usually this option is not respected because
#' each functions knows which data must be loaded or not. This is documented in each function.
#' \item \strong{filter}: string. The \code{filter} option
#' }
#'
#' @import data.table
#' @import methods
#' @importClassesFrom sp CRS
#' @importClassesFrom sp SpatialPolygonsDataFrame
#'
#' @export
#'
#' @useDynLib lidR, .registration = TRUE
setClass(
  Class = "LAScatalog",
  contains = "SpatialPolygonsDataFrame",
  representation(
    clustering_options = "list",
    processing_options = "list",
    output_options = "list",
    input_options = "list"
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
    ),
    SimpleFeature = list(
      write = sf::st_write
    ),
    DataFrame = list(
      write = data.table::fwrite
    )
  )

  .Object@clustering_options <- list(
    tiling_size = 0,
    buffer = 30,
    alignment = c(0,0)
  )

  .Object@processing_options <- list(
    cores = 1L,
    progress = TRUE,
    stop_early = TRUE,
    wall.to.wall = TRUE
  )

  .Object@output_options <- list(
    output_files = "",
    drivers = drivers
  )

  .Object@input_options <- list(
    select = "*",
    filter = ""
  )

  return(.Object)
})
