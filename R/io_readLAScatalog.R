#' Create an object of class LAScatalog
#'
#' Create an object of class \link[=LAScatalog-class]{LAScatalog} from a folder
#' or a collection of filenames. A LAScatalog is a representation of a collection
#' of las/laz files. A computer cannot load all the data at once. A \code{LAScatalog}
#' is a simple way to manage all the files sequentially. Most functions from
#' `lidR` can be used seamlessly with a LAScatalog using the internal
#' `LAScatalog` processing engine. To take advantage of the `LAScatalog`
#' processing engine the user must first adjust some processing options using the
#' \link[=engine_options]{appropriate functions}. Careful reading of the
#' \link[=LAScatalog-class]{LAScatalog class documentation} is required to use the
#' `LAScatalog` class correctly.\cr\cr
#' `readLAScatalog` is the original function and always works. Using one of the `read*LAScatalog` functions
#' adds information to the returned object to register a point-cloud type. Registering the correct point
#' type **may** improve the performance of some functions by enabling users to select an appropriate spatial index.
#' See \link[=lidR-spatial-index]{spatial indexing}. Notice that by legacy and for backwards-compatibility
#' reasons `readLAScatalog()` and `readALSLAScatalog()` are equivalent because lidR was originally designed
#' for ALS and thus the original function `readLAScatalog()` was (supposedly) used for ALS.
#'
#' @param folder string. The path of a folder containing a set of las/laz files.
#' Can also be a vector of file paths.
#' @param progress,select,filter,chunk_size,chunk_buffer Easily accessible processing
#' options tuning. See \link{LAScatalog-class} and \link{engine_options}.
#' @param \dots Extra parameters to \link[base:list.files]{list.files}. Typically
#' `recursive = TRUE`. Propagates also to `readLAScatalog`
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
#' ctg <- readLAScatalog("</path/to/folder/of/las/>")
#'
#' # Internal engine will sequentially process chunks of size 500 x 500 m
#' opt_chunk_size(ctg) <- 500
#'
#' # Internal engine will align the 500 x 500 m chunks on x = 250 and y = 300
#' opt_alignment(ctg) <- c(250, 300)
#'
#' # Internal engine will not display a progress estimation
#' opt_progress(ctg) <- FALSE
#'
#' # Internal engine will not return results into R.
#' # Instead it will write results in files.
#' # Files will be named e.g.
#' # filename_256000_1.ext
#' # filename_257000_2.ext
#' # filename_258000_3.ext
#' # ...
#' opt_output_files(ctg) <- "/path/filename_{XBOTTOM}_{ID}"
#'
#' # More details in the documentation
#' help("LAScatalog-class", "lidR")
#' help("engine_options", "lidR")
#' }
#' @md
readLAScatalog <- function(folder, progress = TRUE, select = "*", filter = "", chunk_size = 0, chunk_buffer = 30, ...)
{
  assert_is_character(folder)

  finfo <- file.info(folder)

  if (all(is.na(finfo$isdir)))
    stop(glue::glue("'{folder}' does not exist."), call. = FALSE)
  else if (all(!finfo$isdir))
    files <- normalizePath(folder)
  else
  {
    p <- list(...)
    p$path <- folder
    p$full.names <- TRUE
    if (is.null(p$pattern)) p$pattern <- "(?i)\\.la(s|z)$"
    files <- do.call(list.files, p)
  }

  verbose("Reading files...")

  if (length(files) == 1L && tools::file_ext(files) == "vpc")
  {
    files = normalizePath(files)
    headers = read_vpc(files)
    files = sapply(headers, function(x) x$filename)
    crs = sf::st_crs(headers[[1]]$CRS)
  }
  else
  {
    header <- LASheader(rlas::read.lasheader(files[1]))
    crs    <- st_crs(header)
    phblab <- make.names(names(phb(header)))
    phblab[4] <- "GUID"

    # Delayed progress bar
    t0 <- Sys.time()
    pb <- NULL
    i  <- 0

    headers <- lapply(files, function(x)
    {
      header <- rlas::read.lasheader(x)
      if (length(header) == 0)
      {
        warning(paste0("Corrupted file ", x, " is not readable and was skipped in the LAScatalog"))
        return(NULL)
      }

      header        <- LASheader(header)
      PHB           <- header@PHB
      names(PHB)    <- phblab

      if (use_wktcs(header))
        PHB[["CRS"]] <- wkt(header)
      else
        PHB[["CRS"]] <- epsg(header)

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

      if (progress && Sys.time() - t0 > getOption("lidR.progress.delay")) {
        if (is.null(pb))
          pb <<- utils::txtProgressBar(min = 0, max = length(files), initial = i, style = 3)

        utils::setTxtProgressBar(pb, i)
      }

      i <<- i + 1

      return(PHB)
    })
  }

  rm = sapply(headers, is.null)
  files = files[!rm]
  headers <- data.table::rbindlist(headers)
  headers$filename <- files
  data.table::setDF(headers)

  xmin <- headers$Min.X
  xmax <- headers$Max.X
  ymin <- headers$Min.Y
  ymax <- headers$Max.Y
  zmin <- headers$Min.Z
  zmax <- headers$Max.Z
  ids  <- as.character(seq_along(files))

  geom <- lapply(seq_along(ids), function(xi) {
    mtx <- matrix(c(xmin[xi], xmax[xi], ymin[xi], ymax[xi])[c(1, 1, 2, 2, 1, 3, 4, 4, 3, 3)], ncol = 2)
    sf::st_polygon(list(mtx))
  })

  geom <-sf::st_sfc(geom)
  sf::st_crs(geom) <- crs
  headers <- sf::st_set_geometry(headers, geom)

  res <- new("LAScatalog")
  res@data <- headers

  opt_filter(res) <- filter
  opt_select(res) <- select
  opt_chunk_size(res) <- chunk_size
  opt_chunk_buffer(res) <- chunk_buffer
  opt_progress(res) <- progress

  xrange = xmax - xmin
  yrange = ymax - ymin
  zrange = zmax - zmin
  area = sum(xrange*yrange)
  if (area > 0)
  {
    n = sum(res$Number.of.point.records)
    density = n/area
    zratio = min(zrange/xrange, zrange/yrange)
  }
  else
  {
    zratio = 0
    density = 0
  }


  if (is.na(zratio)) # VPC file
    res@index <- LIDRALSINDEX
  else if (zratio < 10/100)
    res@index <- LIDRALSINDEX
  else if ((zratio >= 10/100 & density > 100) || density > 1000)
    res@index <- LIDRTLSINDEX
  else
    res@index <- LIDRALSINDEX



  if (is.overlapping(res))
    message("Be careful, some tiles seem to overlap each other. lidR may return incorrect outputs with edge artifacts when processing this catalog.")

  # if (!is.indexed(res))
  # message("las or laz files are not associated with lax files. This is not mandatory but may greatly speed up some computations. See help('writelax', 'rlas').")

  return(res)
}

#' @export
#' @rdname readLAScatalog
readALScatalog = function(folder, ...)
{
  ctg <- readLAScatalog(folder, ...)
  ctg@index <- LIDRALSINDEX
  return(ctg)
}


#' @export
#' @rdname readLAScatalog
readTLScatalog = function(folder, ...)
{
  ctg <- readLAScatalog(folder, ...)
  ctg@index <- LIDRTLSINDEX
  return(ctg)
}

#' @export
#' @name readLAScatalog
catalog <- function(folder, ...) { readLAScatalog(folder, FALSE, ...) }

read_vpc <- function(f)
{
  vpc = rjson::fromJSON(file = f)
  features = vpc$features
  headers = vector("list", length(features))
  for (i in seq_along(features))
  {
    feature = features[[i]]
    if (length(feature$properties[["proj:bbox"]]) == 6)
    {
      headers[[i]]$Min.X = feature$properties[["proj:bbox"]][1]
      headers[[i]]$Min.Y = feature$properties[["proj:bbox"]][2]
      headers[[i]]$Min.Z = feature$properties[["proj:bbox"]][3]
      headers[[i]]$Max.X = feature$properties[["proj:bbox"]][4]
      headers[[i]]$Max.Y = feature$properties[["proj:bbox"]][5]
      headers[[i]]$Max.Z = feature$properties[["proj:bbox"]][6]
    }
    else if (length(feature$properties[["proj:bbox"]]) == 4)
    {
      headers[[i]]$Min.X = feature$properties[["proj:bbox"]][1]
      headers[[i]]$Min.Y = feature$properties[["proj:bbox"]][2]
      headers[[i]]$Max.X = feature$properties[["proj:bbox"]][3]
      headers[[i]]$Max.Y = feature$properties[["proj:bbox"]][4]
    }
    else
    {
      stop("Invalid VPC file with no valid bbox")
    }

    headers[[i]]$Number.of.point.records = feature$properties[["pc:count"]]

    path = feature$assets$data$href[1]

    if (!is_absolute(path)) #771
    {
      relative_path <- path
      parent = dirname(f)
      absolute_path <- file.path(parent, relative_path)
      absolute_path <- normalizePath(absolute_path)
    }
    else
    {
      absolute_path = path
    }

    headers[[i]]$Min.Z = NA_real_
    headers[[i]]$Max.Z = NA_real_
    headers[[i]]$filename = absolute_path

    wkt = feature$properties[["proj:wkt2"]]
    epsg = feature$properties[["proj:epsg"]]

    if (!is.null(wkt))
      headers[[i]]$CRS = wkt
    else if (!is.null(epsg))
      headers[[i]]$CRS = epsg
    else
      headers[[i]]$CRS = 0
  }

  return (headers)
}

is_absolute <- function(path)
{
  if (.Platform$OS.type == "windows") {
    grepl("^[a-zA-Z]:\\\\|^\\\\", path)  # Absolute paths start with "C:\" or "\\" on Windows
  } else {
    substr(path, 1, 1) == "/"  # Absolute paths start with "/" on Unix-like systems
  }
}
