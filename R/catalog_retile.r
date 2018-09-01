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


#' Retile a catalog
#'
#' Splits or merges files to reshape the original catalog files (.las or .laz) into smaller or larger
#' files. It also enables the addition or removal of a buffer around the tiles. The new files are written
#' in a dedicated folder. The function first displays the layout of the new tiling pattern and then asks
#' the user to validate the command.
#'
#' Internally the function reads and writes the clusters defined by the internal processing options of a
#' \link[lidR:LAScatalog-class]{LAScatalog} (see also \link{catalog}). Thus the function is flexible and
#' enables the user to retile the dataset, retile while adding or removing a buffer (negative buffers are
#' allowed), or optionally to compress the data by retiling without changing the pattern but by changing
#' the format (las/laz).\cr\cr
#'
#' Note that this function is not actually very useful since \code{lidR} manages everything
#' (clipping, processing, buffering, ...) internally using the proper options. Thus, retiling may be useful
#' for working in other software for example, but not in \code{lidR}.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-catalog_apply
#'
#' @param ctg  A \link[lidR:catalog]{LAScatalog} object
#'
#' @return A new \code{LAScatalog} object
#' @seealso \link{catalog}
#' @export
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
#' # Being flexible this function can also compress a catalog but this is
#' # not really useful since laszip from LAStools is a free and open source
#' # program.
#'
#' set_buffer(ctg) <- 0
#' set_tiling_size(ctg) <- 0
#' set_laz_compression(ctg) <- TRUE
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