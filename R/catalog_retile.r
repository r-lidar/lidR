# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2018 Jean-Romain Roussel
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


#' Retile a LAScatalog
#'
#' Splits or merges files to reshape the original catalog files (.las or .laz) into smaller or larger
#' files. It also enables the addition or removal of a buffer around the tiles. The function first
#' displays the layout of the new tiling pattern and then asks the user to validate the command.\cr
#' Internally the function reads and writes the clusters defined by the internal processing options
#' of a \link[lidR:LAScatalog-class]{LAScatalog} processing engine. Thus the function is flexible and
#' enables the user to retile the dataset, retile while adding or removing a buffer (negative buffers are
#' allowed), or optionally to compress the data by retiling without changing the pattern but by changing
#' the format (las/laz).\cr\cr
#' Note that this function is not actually very useful since \code{lidR} manages everything
#' (clipping, processing, buffering, ...) internally using the proper options. Thus, retiling may be
#' useful for working in other softwares for example, but not in \code{lidR}.
#'
#' @template LAScatalog
#'
#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{tiling_size}: Size of the new tiles.
#' \item \strong{buffer}: Load new tiles with a buffer. Usually 0 is the expected value.
#' \item \strong{alignment}: Alignment of the new tiles.
#' \item \strong{cores}: How many cores are used. \code{catalog_retile} streams the data (nothing is
#' loaded at th R level). The maximum number of core can be safely used.
#' \item \strong{progress}: Displays a progression estimation.
#' \item \strong{output_files*}: Mandatory. The new tiles will be written in files new files.
#' \item \strong{laz_compression}: save \code{las} or \code{laz} files.
#' \item select: \code{catalog_retile} preserve the file format anyway.
#' \item \strong{filter}: Retile and save only points of interest.
#' }
#'
#' @param ctg A \link[lidR:catalog]{LAScatalog} object
#'
#' @return A new \code{LAScatalog} object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ctg = catalog("path/to/catalog")
#'
#' # Create a new set of .las files 500 by 500 wide in the folder
#' # path/to/new/catalog/ and iteratively named Forest_1.las, Forest_2.las
#' # Forest_3.las, and so on.
#'
#' set_buffer(ctg) <- 0
#' set_tiling_size(ctg) <- 500
#' set_output_files(ctg) <- "path/to/new/catalog/Forest_{ID}
#' newctg = catalog_retile(ctg)
#'
#' # Create a new set of .las files equivalent to the original one
#' # but extended with a 50 m buffer in the folder path/to/new/catalog/
#' # and iteratively named named after the original files.
#'
#' set_buffer(ctg) <- 50
#' set_tiling_size(ctg) <- 0
#' set_output_files(ctg) <- "path/to/new/catalog/{ORIGINALFILENAME}_buffered
#' newctg = catalog_retile(ctg)
#'
#' # Create a new set of compressed .laz file equivalent to the original one keeping only
#' # first returns above 2 m
#'
#' set_buffer(ctg) <- 0
#' set_tiling_size(ctg) <- 0
#' set_laz_compression(ctg) <- TRUE
#' set_filter(ctg) <- "-keep_first -drop_z_below 2"
#' newctg = catalog_retile(ctg)
#' }
catalog_retile = function(ctg)
{
  interact <- getOption("lidR.interactive")

  if (get_output_files(ctg) == "")
    stop("This function requieres that the LAScatalog provides an output file template. See  help(\"LAScatalog-class\", \"lidR\")", call. = FALSE)

  laz  <- get_laz_compression(ctg)
  path <- get_output_files(ctg)
  path <- if(laz) paste0(path, ".laz") else paste0(path, ".las")
  ctg@output_options$output_files <- path
  path <- dirname(path)

  if(interact)
  {
    set_progress(ctg) <- TRUE

    clusters  <- catalog_makecluster(ctg)

    text = "This is how the catalog will be reshaped (see plots). Do you want to continue?"
    choices = c("yes","no")

    cat(text)
    choice = utils::menu(choices)

    if (choice == 2)
      return(invisible(NULL))
  }

  if(!dir.exists(path))
    dir.create(path, recursive = TRUE)

  files <- list.files(path, pattern = "(?i)\\.la(s|z)$")

  if(length(files) > 0)
    stop("The output folder already contains .las or .laz files. Operation aborted.", call. = FALSE)

  reshape_func = function(cluster)
  {
    streamLAS(cluster, cluster@save)
    ret = 0
    class(ret) <- "lidr_internal_skip_write"
    return(ret)
  }

  catalog_apply2(ctg, reshape_func, need_buffer = FALSE, check_alignement = FALSE, drop_null = TRUE, need_output_file = TRUE)

  return(catalog(path))
}