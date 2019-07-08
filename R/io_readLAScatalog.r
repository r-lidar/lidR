#' Create an object of class LAScatalog
#'
#' Create an object of class \link[lidR:LAScatalog-class]{LAScatalog} from a folder or a set of filenames.
#' A LAScatalog is a representation of a set of las/laz files. A computer cannot load all the data at
#' once. A \code{LAScatalog} is a simple way to manage all the files sequentially. Most functions from
#' \code{lidR} can be used seamlessly with a LAScatalog using the internal \code{LAScatalog} processing
#' engine. To take advantage of the \code{LAScatalog} processing engine the user must first adjust some
#' processing options using the \link[lidR:catalog_options_tools]{appropriated functions}. Careful
#' reading of the \link[lidR:LAScatalog-class]{LAScatalog class documentation} is required to use the
#' \code{LAScatalog} class correctly.\cr\cr \code{catalog()} is softly deprecated for \code{readLAScatalog()}.
#'
#' @param folder string. The path of a folder containing a set of las/laz files. Can also be a vector of
#' file paths.
#' @param progress boolean. Display a progress bar.
#' @param \dots Extra parameters to \link[base:list.files]{list.files}. Typically `recursive = TRUE`.
#'
#' @return A \code{LAScatalog} object
#'
#' @export
#'
#' @examples
#' # A single file LAScatalog using data provided with the package
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' ctg = readLAScatalog(LASfile)
#' plot(ctg)
#'
#' \dontrun{
#' ctg <- readLAScatalog("/path/to/a/folder/of/las/files")
#'
#' # Internal engine will sequentially process chunks of size 500 x 500 m (clusters)
#' opt_chunk_size(ctg) <- 500
#'
#' # Internal engine will align the 500 x 500 m chunks on x = 250 and y = 300
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
readLAScatalog <- function(folder, progress = FALSE, ...)
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

  if (progress)
  {
    pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
    i  <- 0
  }

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

    if (progress)
    {
      i <<- i + 1
      utils::setTxtProgressBar(pb, i)
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

#' @export
#' @name readLAScatalog
catalog <- function(folder, ...) { readLAScatalog(folder, FALSE, ...) }
