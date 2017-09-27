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




#' Reshape (retile) a catalog
#'
#' This function splits or merges files to reshape the original catalog files (.las or .laz)
#' into smaller or larger files. The new files are written in a dedicated folder. The function
#' first displays the pattern of the new tiling and then asks the user to validate the command.
#'
#' @param ctg  A \link[lidR:catalog]{LAScatalog} object
#' @param size scalar. The size of the new tiles.
#' @param path string. The folder where the new files should be saved.
#' @param prefix character. The initial part of the name of the written files.
#' @param ext character. The format of the written files. Can be ".las" or ".laz".
#'
#' @return A new catalog object
#' @seealso \link{catalog}
#' @export
#' @examples
#' \dontrun{
#' ctg = catalog("path/to/catalog")
#'
#' # Create a new set of .las files 500 by 500 wide in the folder
#' # path/to/new/catalog/ and iteratively named Forest_1.las, Forest_2.las
#' # Forest_3.las, and so on.
#' newctg = catalog_reshape(ctg, 500, "path/to/new/catalog", "Forest_")
#' }
catalog_reshape = function(ctg, size, path, prefix, ext = c("las", "laz"))
{
  ext <- match.arg(ext)

  ncores  = CATALOGOPTIONS("multicore")
  interact = LIDROPTIONS("interactive")

  # Create a pattern of clusters to be sequentially processed
  clusters = catalog_makecluster(ctg, 1, 0, FALSE, size)

  if(interact)
  {
    text = paste0("This is how the catalog will be reshaped. Do you want to continue?")
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
    stop("The output folder already contains .las or .laz files. Operation aborted.")

  ti = Sys.time()

  # Computations done within sequential or parallel loop in .getMetrics
  if (ncores == 1)
  {
    output = lapply(clusters, reshape_func, path = path, prefix = prefix, ext = ext)
  }
  else
  {
    cat("Begin parallel processing... \n")
    cat("Num. of cores:", ncores, "\n\n")

    cl = parallel::makeCluster(ncores, outfile = "")
    parallel::clusterExport(cl, varlist = c(utils::lsf.str(envir = globalenv()), ls(envir = environment())), envir = environment())
    output = parallel::parLapply(cl, clusters, fun = reshape_func, path = path, prefix = prefix, ext = ext)
    parallel::stopCluster(cl)
  }

  tf = Sys.time()
  cat("Process done in", round(difftime(tf, ti, units="min"), 1), "min\n\n")

  return(catalog(path))
}

reshape_func = function(cluster, path, prefix, ext)
{
  ofile = paste0(path, "/", prefix, cluster@name , ".", ext)
  streamLAS(cluster, ofile)
  return(NULL)
}