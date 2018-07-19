#' Deprecated function(s) in the lidR package
#'
#' These functions are provided for compatibility with older version of
#' the lidR package.  They may eventually be completely
#' removed.
#' @rdname lidR-deprecated
#' @name lidR-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @docType package
lasdecimate <- function(...) {
  .Deprecated("lasfilterdecimate")
  lasfilterdecimate(...)
}

bool = function() { settings::inlist(TRUE, FALSE) }

CATALOGOPTIONS <- settings::options_manager(
  return_virtual_raster = FALSE,
  buffer = 15,
  by_file = FALSE,
  multicore = parallel::detectCores(),
  progress = TRUE,
  memory_limit_warning = 5e8,
  tiling_size = 1000,
  global_changed = FALSE,

  .allowed = list(
    return_virtual_raster  = bool(),
    buffer = settings::inrange(0, Inf),
    by_file = bool(),
    multicore = settings::inrange(1, Inf),
    progress = bool(),
    memory_limit_warning = settings::inrange(0, Inf),
    tiling_size = settings::inrange(0, Inf),
    global_changed = bool()
  )
)


#' Options Settings for the \link{catalog} tools (deprecated)
#'
#' Allow the user to set and examine a variety of global options that affect the way in which
#' lidR processes an entire catalog. This function is deprecated. See \link{catalog} instead.
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set
#' options.
#'
#' @section Supported options:
#' The following options are supported:
#' \itemize{
#'  \item{\code{progress} (\code{logical}) Display progress bar. Default is TRUE. }
#'  \item{\code{buffer} (numeric) - When applying a function to an entire catalog
#'  sequentially processing sub-areas (clusters) some algorithms (such as \link{grid_terrain})
#'  require a buffer around the area to avoid edge effects. Default is 15 m.}
#'  \item{\code{multicore} (numeric) - For parallel processes, fix the number of
#'  cores to use. Default is the number of cores you have.}
#'  \item{\code{tiling_size} (numeric) - To process an entire catalog, the algorithm splits the
#'  dataset into several square sub-areas (clusters) to process them sequentially. This is the
#'  size of each square cluster. Default is 1000 (1 km^2).}
#'  \item{\code{by_file} (logical) - This option overwrites the option \code{tiling_size}. Instead
#'  of processing the catalog by arbitrary split areas, it forces processing by file. Buffering
#'  is still available.}
#'  \item{\code{return_virtual_raster} (logical) - Functions which return raster-like
#'  data such as \link{grid_metrics}, \link{grid_terrain} and other \code{grid_*} functions
#'  may return huge amounts of data for large catalogs or high resolution data (typically
#'  \code{grid_terrain} with a resolution of 1 meter). Switching this option to \code{TRUE}
#'  enables storage of the data on the hard disk and returns a lightweight virtual raster mosaic.}
#'  \item{\code{memory_limit_warning} (numeric) - When applying a function to an entire
#'  catalog, an internal function tries to estimate the size of the output before running the
#'  algorithm in an attempt to prevent memory overflow. This value (in bytes) is the threshold
#'  before a warning is given. Default is 5e8 (500 Mb). Set to \code{Inf} to disable.}
#'  }
#'
#' @examples
#' \dontrun{
#' catalog_options(multicore = 2)
#' catalog_options(buffer = 40)
#' catalog_options()
#'
#' # Reset default options
#' catalog_reset()
#' }
#' @export
catalog_options <- function(...)
{
  .Deprecated("lasarea", package="lidR", "'catalog_options' is deprecated and will be removed in version 1.7. Use catalog properties instead. See help('catalog') and help('LAScatalog-class')")
  settings::stop_if_reserved(...)
  CATALOGOPTIONS(...)
  CATALOGOPTIONS(global_changed = TRUE)
}

#' @export
#' @rdname catalog_options
catalog_reset = function() { settings::reset(CATALOGOPTIONS) }

# ================= OLD ===================== #

#' Reshape (retile) a catalog
#'
#' This function is supersed by \link{catalog_retile} that can do the same and much more.
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
  assertive::assert_is_all_of(ctg, "LAScatalog")
  assertive::assert_is_a_number(size)
  assertive::assert_all_are_positive(size)
  assertive::is_character(path)
  assertive::is_character(prefix)

  format           <- match.arg(ext)
  interact         <- LIDROPTIONS("interactive")
  buffer(ctg)      <- 0
  by_file(ctg)     <- FALSE
  tiling_size(ctg) <- size
  ncores           <- cores(ctg)
  progress         <- progress(ctg)
  stopearly        <- stop_early(ctg)

  clusters <- catalog_makecluster(ctg, 1)

  if(interact)
  {
    text = "This is how the catalog will be reshaped. Do you want to continue?"
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

  cluster_apply(clusters, reshape_func, ncores, progress, stopearly, path = path, prefix = prefix, ext = format)

  return(catalog(path))
}