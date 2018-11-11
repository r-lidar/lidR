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
#' @param ctg  A \link[lidR:catalog]{LAScatalog} object
#' @param path string. The folder where the new files should be saved.
#' @param prefix character. The prefix of the written file name. It can be missing, in which case if the
#' catalog is processed by file, the original name of the file will be retained.
#' @param ext character. The format of the written files. Can be ".las" or ".laz".
#' @param alignment numeric vector. A vector of size 2 (x and y coordinates, respectively) to align the
#' pattern. By default the alignment is made along (0,0) as in all LAScatalog processes, meaning the edge
#' of a virtual tile will belong on x = 0 and y = 0 and all the the others will be multiples of the
#' tiling size.
#' @param ... extra parameter 'filter' to pass to \link{readLAS} (readLAS is not actually called but
#' the parameter can be passed anyway).
#'
#' @return A new \code{LAScatalog} object
#' @seealso \link{catalog}
#' @export
#' @examples
#' \dontrun{
#' ctg = catalog("path/to/catalog")
#'
#' # Create a new set of .las files 500 by 500 wide in the folder
#' # path/to/new/catalog/ and iteratively named Forest_001.las, Forest_002.las
#' # Forest_003.las, and so on.
#'
#' buffer(ctg) = 0
#' by_file(ctg) = FALSE
#' tiling_size(ctg) = 500
#' newctg = catalog_retile(ctg, "path/to/new/catalog", "Forest_")
#'
#' # Create a new set of .las files equivalent to the original one
#' # but extended with a 50 m buffer in the folder path/to/new/catalog/
#' # and iteratively named named after the original files.
#'
#' buffer(ctg) = 50
#' by_file(ctg) = TRUE
#' newctg = catalog_retile(ctg, "path/to/new/catalog")
#'
#' # Being flexible this function can also compress a catalog but this is
#' # not really useful since laszip from LAStools is a free and open source
#' # program.
#'
#' buffer(ctg) = 0
#' by_file(ctg) = TRUE
#' newctg = catalog_retile(ctg, "path/to/compressed/file",  ext = "laz")
#' }
catalog_retile = function(ctg, path, prefix, ext = c("las", "laz"), alignment = c(0,0), ...)
{
  assertive::assert_is_all_of(ctg, "LAScatalog")
  assertive::assert_is_character(path)
  assertive::assert_is_numeric(alignment)
  assertive::assert_is_of_length(alignment, 2)

  if(!missing(prefix))
    assertive::is_character(prefix)
  else
    prefix = ""

  format           <- match.arg(ext)
  interact         <- LIDROPTIONS("interactive")
  ncores           <- cores(ctg)
  progress         <- progress(ctg)
  stopearly        <- stop_early(ctg)

  clusters <- catalog_makecluster(ctg, 1, alignment)

  for (i in 1:length(clusters))
  {
    if (by_file(ctg) && prefix == "")
      clusters[[i]]@name <- tools::file_path_sans_ext(basename(ctg@data$filename[i]))
    else
      clusters[[i]]@name <- sprintf("%05d", as.numeric(i))
  }

  if(interact)
  {
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

  cluster_apply(clusters, reshape_func, ncores, progress, stopearly, path = path, prefix = prefix, ext = format, ...)

  return(catalog(path))
}

reshape_func = function(cluster, path, prefix, ext, ...)
{
  ofile = paste0(path, "/", prefix, cluster@name , ".", ext)
  streamLAS(cluster, ofile, ...)

  header = rlas::read.lasheader(ofile)

  if (header$`Number of point records` == 0)
  {
    file.remove(ofile)
    return(NULL)
  }

  return(0)
}