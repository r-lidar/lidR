# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016 Jean-Romain Roussel
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



#' Apply a function to a set of tiles using several cores.
#'
#' The function behaves differently on the MS Windows and Unix platforms, so please read
#' the documentation carefully, particularly the "Details", "Unix" and "Windows" sections.
#' This function provides an immediately usable parallel computing tool, but users comfortable
#' with multi-core processes should use their own code for greater flexibility. When a user
#' has a LAS dataset organized into several tiles, it applies a user function to each tile.
#' The "Examples" section describes the procedure to apply functions to each file, beginning
#' with data loading (see example). The function automatically detects your operating system and
#' applies the best parallelization method for your system. Unix mechanism is more powerful.
#' However it is not compatible with Windows (see "Unix" and "Windows" sections). The Windows
#' mechanism is more complex to use.\cr\cr
#' \strong{Warning:} there is no buffer mechanism to protect the process against edge artifacts.
#' See section "Edge artifacts".
#'
#' @section Unix:
#'
#' On the Unix platform (GNU/Linux and Mac), parallelization relies on the fork-exec technique
#' (see \link[parallel:mclapply]{mclapply}). This means that each child process
#' has access to the parent process memory. For example, you can call functions from \code{.GlobalEnv}
#' or any other environment. If code written for Unix is run on Windows it will work,
#' but with only one core, like a normal loop. If Unix users want to share their code
#' with Windows users, it would be better to force the function to use the clustering method.
#'
#' @section Windows:
#'
#' On the Windows platform (MS Windows), parallelization relies on the cluster technique
#' (see \link[parallel:parLapplyLB]{parLapplyLB}). This works for both Unix and Windows
#' but it is much more memory-intensive and also not very user-friendly, as the user must
#' manually export any required objects. This means that each child process cannot access the parent process memory.
#' If you want to use only a single core, the function uses the \code{unix}
#' mode which works like a regular loop (non-parallel computing).
#'
#' @section Edge artifacts:
#'
#' It is very important to take precautions to avoid 'edge artifacts' when processing LiDAR tiles.
#' If the points from neighboring tiles are not included during certain processes, this could create 'edge artifacts'
#' at the edges of the tiles. For example, empty or incomplete pixels in a rasterization process. The lidR package
#' does not provide internal tools to deal with buffers as it is designed for experimental purposes rather than as a
#' professional product. Users could, for example, filter the invalid/corrupted data at the edge of the tiles from the
#' output.
#'
#' @aliases catalog_apply
#' @param x  A \link[lidR:catalog]{Catalog} object
#' @param func A function which has one parameter: the name of a .las or .laz file (see example)
#' @param platform character. Can be \code{"windows"} or \code{"unix"}. Default is autodetect.
#' See sections "Details", "Unix" and  "Windows".
#' @param mc.cores integer. Number of cores used. Default is the number of cores you have on your computer.
#' @param combine character. The function used to merge the outputs of the \code{func} function.
#' @param varlist charaters vector. For \code{'windows'} mode, character vector of names of objects to export.
#' @examples
#' \dontrun{
#' # Visit http://jean-romain.github.io/lidR/wiki for more examples
#' # about this function
#'
#' # 1. build a project
#' project = catalog("folder")
#' plot(project)
#'
#' # 2. load the shapefile you need to filter your points (if needed).
#' lake = rgdal::readOGR("folder", "shapefile")
#'
#' # 3 build the function which analyses a tile (a file).
#' # This function input is only the path of a .las file
#' # see the following template
#' analyse_tile = function(LASFile)
#' {
#'   # Load the data
#'   lidar = readLAS(LASFile)
#'
#'   # Associate geographic data with lidar points (if needed)
#'   lidar %<>% lasclassify(lake, field="lake")
#'
#'   # filter lake
#'   lidar %<>% lasfilter(lake == FALSE)
#'
#'   # compute standard metrics
#'   metrics = grid_metrics(lidar, .stdmetrics)
#'
#'   return(metrics)
#' }
#'
#' #### UNIX #####
#' # This code works only on Unix platforms because it relies on shared memory
#' # between all processes. See below for a Windows-compatible code.
#'
#' # 4. Process the project. By default it detects how many cores you have. But you
#' # can add an optional parameter mc.core = 3.
#' output = project %>% catalog_apply(analyse_tile)
#'
#' #### WINDOWS #####
#' # This code works on both Unix and Windows platforms. But it is more
#' # memory-intensive and more complex on Windows (here the example is simple enough
#' # so it does not change many things)
#'
#' # 4. Process the project. By default it detects how many cores you have. But you can add
#' # an optional parameter mc.core = 3.
#' export = c("readLAS", "lasclassify", "grid_metrics", "myMetrics", "lake", "lasfilter", "%<>%")
#' output = project %>% catalog_apply(analyse_tile, varlist = export, platform = "windows")
#' }
#' @seealso
#' \link[parallel:mclapply]{mclapply}
#' \link[parallel:parLapplyLB]{parLapplyLB}
#' @export
catalog_apply = function(x, func, platform=.Platform$OS.type, mc.cores = parallel::detectCores(), combine = "rbind", varlist = "")
{
  cat("Begin parallel processing... \n")

  ti = Sys.time()

  files = x$filename

  if(platform == "unix" | mc.cores == 1)
  {
    cat("Platform mode: unix (fork-exec)\n")
    cat("Num. of cores:", mc.cores, "\n\n")
    out = parallel::mclapply(files, func, mc.preschedule = FALSE, mc.cores = mc.cores)
  }
  else
  {
    cat("Platform mode: windows (cluster)\n")
    cat("Num. of cores:", mc.cores, "\n\n")
    cl <- parallel::makeCluster(getOption("cl.cores", mc.cores))
    parallel::clusterExport(cl, varlist, envir = environment())
    out = parallel::parLapplyLB(cl, files, func)
    parallel::stopCluster(cl)
  }

  out = do.call(combine, out)
  gc()

  tf = Sys.time()
  cat("Process done in", round(difftime(tf, ti, units="min"), 1), "min\n\n")

  return(out)
}
