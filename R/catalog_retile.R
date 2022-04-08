#' Retile a LAScatalog
#'
#' Splits or merges files to reshape the original files collection (.las or .laz) into smaller or larger
#' files. It also enables the addition or removal of a buffer around the tiles. Internally, the
#' function reads and writes the chunks defined by the internal processing options of a
#' \link[=LAScatalog-class]{LAScatalog}. Thus, the function is flexible and enables the user to retile
#' the dataset, retile while adding or removing a buffer (negative buffers are allowed), or optionally
#' to compress the data by retiling without changing the pattern but by changing
#' the format (las/laz). **This function is does not load the point cloud into R memory** It streams
#' from input file(s) to output file(s) and can be applied to large point-cloud with low memory
#' computer.\cr\cr
#' Note that this function is not actually very useful because `lidR` manages everything
#' (clipping, processing, buffering, ...) internally using the proper options. Thus, retiling may be
#' useful for working in other software, for example, but not in `lidR`
#'
#' @section Non-supported LAScatalog options:
#' The option `select` is not supported and not respected because it always preserves the file
#' format and all the attributes. `select = "*"` is imposed internally.
#'
#' @param ctg A \link[=LAScatalog-class]{LAScatalog} object
#'
#' @return A new \code{LAScatalog} object
#'
#' @export
#' @md
#'
#' @examples
#' \donttest{
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' ctg = readLAScatalog(LASfile)
#' plot(ctg)
#'
#' # Create a new set of 200 x 200 m.las files with first returns only
#' opt_chunk_buffer(ctg) <- 0
#' opt_chunk_size(ctg) <- 200
#' opt_filter(ctg) <- "-keep_first"
#' opt_chunk_alignment(ctg) <- c(275, 90)
#' opt_output_files(ctg) <- paste0(tempdir(), "/retile_{XLEFT}_{YBOTTOM}")
#'
#' # preview the chunk pattern
#' plot(ctg, chunk = TRUE)
#'
#' newctg = catalog_retile(ctg)
#'
#' plot(newctg)
#'
#' # Create a new set of 200 x 200 m.las files
#' # but extended with a 50 m buffer in the folder
#'
#' opt_chunk_buffer(ctg) <- 25
#' opt_chunk_size(ctg) <- 200
#' opt_filter(ctg) <- ""
#' opt_chunk_alignment(ctg) <- c(275, 90)
#' opt_output_files(ctg) <- paste0(tempdir(), "/{XLEFT}_{YBOTTOM}_buffered")
#' newctg = catalog_retile(ctg)
#'
#' plot(newctg)
#' }
#'
#' \dontrun{
#' # Create a new set of compressed .laz file equivalent to the original, keeping only
#' # first returns above 2 m
#'
#' opt_chunk_buffer(ctg) <- 0
#' opt_chunk_size(ctg) <- 0
#' opt_laz_compression(ctg) <- TRUE
#' opt_filter(ctg) <- "-keep_first -drop_z_below 2"
#' opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}_first_2m")
#' newctg = catalog_retile(ctg)
#' }
catalog_retile = function(ctg)
{
  laz  <- opt_laz_compression(ctg)
  path <- opt_output_files(ctg)
  path <- if (laz) paste0(path, ".laz") else paste0(path, ".las")
  ctg@output_options$output_files <- path
  path <- dirname(path)

  reshape_func = function(cluster)
  {
    streamLAS(cluster, cluster@save)

    if (file.exists(cluster@save))
    {
      ret <- structure(cluster@save, class = "lidr_internal_skip_write")
      return(ret)
    }
    else
    {
      return(NULL)
    }
  }

  opt_wall_to_wall(ctg) <- FALSE

  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE, automerge = TRUE)
  return(catalog_apply(ctg, reshape_func, .options = options))
}
