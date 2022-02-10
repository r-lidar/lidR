#' An S4 class to represent a collection of .las or .laz files
#'
#' A `LAScatalog` object is a representation of a collection of las/laz files. A `LAScatalog` is
#' a way to manage and batch process a lidar coverage. It allows the user to process a large area, or to
#' selectively clip data from a large area without loading all the data into computer memory.
#' A `LAScatalog` can be built with the function \link{readLAScatalog}.
#'
#' A `LAScatalog` contains an `sf` object to store the geometry and metadata. It is extended with slots
#' that contain processing options. In `lidR`, each function that supports a `LAScatalog` as
#' input will respect these processing options. Internally, processing a catalog is almost always the
#' same and relies on a few steps:\cr
#' 1. Define chunks. A chunk is an arbitrarily-defined region of interest (ROI) of the
#' collection. Altogether, the chunks are a wall-to-wall set of ROIs that encompass the whole dataset.
#' 2. Loop over each chunk (in parallel or not).
#' 3. For each chunk, load the points inside the ROI into R, run some R functions,
#' return the expected output.
#' 4. Merge the outputs of the different chunks once they are all processed to build a continuous
#' (wall-to-wall) output.
#'
#' So basically, a `LAScatalog` is an object that allows for batch processing but with the specificity
#' that `lidR` does not loop through LAS or LAZ files, but loops seamlessly through chunks that do not
#' necessarily match with the file pattern. This way `lidR` can sequentially process tiny ROIs even if
#' each file may be individually too big to fit in memory. This is also why point cloud indexation
#' with `lax` files may significantly speed-up the processing.\cr\cr
#' It is important to note that catalogs with files that overlap each other are not natively supported
#' by `lidR`. When encountering such datasets the user should always filter any overlaps if
#' possible. This is possible if the overlapping points are flagged, for example in the
#' 'withheld' attribute. Otherwise `lidR` will not be able to process the dataset correctly.
#'
#' @slot data sf. An `sf` `data.frame` with the bounding box of each file as well as all the information
#' read from the header of each LAS/LAZ file.
#'
#' @slot processing_options list. A list that contains some settings describing how the collection will be
#' processed (see dedicated section).
#'
#' @slot chunk_options list. A list that contains some settings describing how the collection will be
#' sub-divided into chunks to be processed (see dedicated section).
#'
#' @slot output_options list. A list that contains some settings describing how the collection will return
#' the outputs (see dedicated section).
#'
#' @slot input_options list. A list of parameters to pass to \link{readLAS} (see dedicated section).
#'
#' @slot index list. See \link[=lidR-spatial-index]{spatial indexing}.
#'
#' @section Processing options:
#' The slot `@processing_options` contains a `list` of options that determine how chunks
#' (the sub-areas that are sequentially processed) are processed.
#'
#' - **progress**: boolean. Display a progress bar and a chart of progress. Default is TRUE.
#' Progress estimation can be enhanced by installing the package `progress`. See \link{opt_progress}.
#' - **stop_early**: boolean. Stop the processing if an error occurs in a chunk. If `FALSE`
#' the process can run until the end, removing chunks that failed. Default is TRUE and the user should
#' have no reason to change this. See \link{opt_stop_early}.
#' - **wall.to.wall** logical. The catalog processing engine always guarantees to return a
#' continuous output without edge effects, assuming that the catalog is a wall-to-wall catalog. To do
#' so, some options are checked internally to guard against bad settings, such as `buffer = 0` for an
#' algorithm that requires a buffer. In rare cases it might be useful to disable these controls. If
#' `wall.to.wall = FALSE` controls are disabled and wall-to-wall outputs cannot be guaranteed.
#' See \link{opt_wall_to_wall}
#'
#' @section Chunk options:
#' The slot `@chunk_options` contains a `list` of options that determine how chunks
#' (the sub-areas that are sequentially processed) are made.
#'
#' - **chunk_size**: numeric. The size of the chunks that will be sequentially processed.
#' A small size allows small amounts of data to be loaded at once, saving computer memory.
#' With big chunks the computation is usually faster but uses much more memory. If `chunk_size = 0` the
#' chunk pattern is build using the file pattern. The chunks are expecting to be wall-to-wall coverage,
#' which means that `chunk_size = 0` has meaning only if the files are not overlapping. Default is 0 i.e.
#' by default the processing engine respects the existing tiling pattern. See \link{opt_chunk_size}.
#' - **buffer**: numeric. Each chunk can be read with an extra buffer around it to ensure there are
#' no edge effects between two independent chunks and that the output is continuous. This is mandatory for
#' some algorithms. Default is 30. See \link{opt_chunk_buffer}.
#' - **alignment**: numeric. A vector of size 2 (x and y coordinates, respectively) to align the
#' chunk pattern. By default the alignment is made along (0,0), meaning that the edge of the first chunk
#' will belong on x = 0 and y = 0 and all the the other chunks will be multiples of the chunk size.
#' Not relevant if `chunk_size = 0`. See \link{opt_chunk_alignment}.
#' - **drop**: integers. A vector of integers that specify the IDs of the chunks that should not be
#' created. This is designed to enable users to restart a computation that failed without reprocessing
#' everything. See \link{opt_restart<-}. Technically, this option may be used for partial processing of
#' a collection, but it generally should not be. Partial processing is already a feature of the engine. See
#' [this vignette](https://cran.r-project.org/package=lidR/vignettes/lidR-LAScatalog-engine.html#partial-processing)
#'
#' @section Output options:
#' The slot `@output_options` contains a `list` of options that determine how chunks
#' (the sub-areas that are sequentially processed) are written. By "written" we mean written to files
#' or written in R memory.
#'
#' - **output_files**: string. If `output_files = ""` outputs are returned in R. Otherwise, if
#' `output_files` is a string the outputs will be written to files.
#' This is useful if the output is too big to be returned in R. A path to a filename template
#' without a file extension (the engine guesses it for you) is expected. When several files are going to be
#' written a single string is provided with a template that is automatically filled. For example,
#' the following file names are possible:
#' \preformatted{
#' "/home/user/als/normalized/file_{ID}_segmented"
#' "C:/user/document/als/zone52_{XLEFT}_{YBOTTOM}_confidential"
#' "C:/user/document/als/{ORIGINALFILNAME}_normalized"
#' }
#' This option will generate as many filenames as needed with custom names for each file. The allowed
#' templates are \code{{XLEFT}, {XRIGHT}, {YBOTTOM}, {YTOP}, {ID}, {XCENTER},
#' {YCENTER}, {ORIGNALFILENAME}}. See \link{opt_output_files}.
#' - **drivers**: list. This contains all the drivers required to seamlessly write `Raster*`,
#' `SpatRaster`, `stars`, `Spatial*`, `sf`, and `LAS` objects. It is recommended that only advanced
#' users change this option. A dedicated page describes the drivers in \link{lidR-LAScatalog-drivers}.
#' - **merge**: boolean. Multiple objects are merged into a single object at the end of the processing.
#' See \link{opt_merge}.
#'
#' @section Input options:
#' The slot `@input_options` contains a `list` of options that are passed to the function
#' \link{readLAS}. Indeed, the `readLAS` function is not called directly by the user but by the
#' internal processing engine. Users can propagate these options through the `LAScatalog` settings.
#'
#' - **select**: string. The option `select`. Usually this option is not respected because
#' each function knows which data must be loaded or not. This is documented in each function. See
#' \link{opt_select}.
#' - **filter**: string. The option `filter`. See \link{opt_filter}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Build a catalog
#' ctg <- readLAScatalog("filder/to/las/files/", filter =  "-keep_first")
#'
#'
#' # Summary gives a summary of how the catalog will be processed
#' summary(ctg)
#'
#' # We can seamlessly use lidR functions
#' hmean <- pixel_metrics(ctg, ~~mean(Z), 20)
#' ttops <- tree_detection(ctg, lmf(5))
#'
#' # For low memory config it is probably advisable not to load entire files
#' # and process chunks instead
#' opt_chunk_size(ctg) <- 500
#'
#' # Sometimes the output is likely to be very large
#' # e.g. large coverage and small resolution
#' dtm <- rasterize_terrain(ctg, 1, tin())
#'
#' # In that case it is advisable to write the output(s) to files
#' opt_output_files(ctg) <- "path/to/folder/DTM_chunk_{XLEFT}_{YBOTTOM}"
#'
#' # Raster will be written to disk. The list of written files is returned
#' # or, in this specific case, a virtual raster mosaic.
#' dtm <- rasterize_terrain(ctg, 1, tin())
#'
#' # When chunks are files the original names of the las files can be preserved
#' opt_chunk_size(ctg) <- 0
#' opt_output_files(ctg) <- "path/to/folder/DTM_{ORIGINALFILENAME}"
#' dtm <- rasterize_terrain(ctg, 1, tin())
#'
#' # For some functions, files MUST be written to disk. Indeed, it is certain that R cannot
#' # handle the entire output.
#' opt_chunk_size(ctg) <- 0
#' opt_output_files(ctg) <- "path/to/folder/{ORIGINALFILENAME}_norm"
#' opt_laz_compression(ctg) <- TRUE
#' new_ctg <- normalize_height(ctg, tin())
#'
#' # The user has access to the catalog engine through the functions catalog_apply
#' # and catalog_map
#' output <- catalog_apply(ctg, FUN, ...)
#' }
#' @md
setClass(
  Class = "LAScatalog",
  representation(
    data = "sf",
    chunk_options = "list",
    processing_options = "list",
    output_options = "list",
    input_options = "list",
    index = "list"
  )
)

setMethod("initialize", "LAScatalog", function(.Object)
{
  drivers = list(
    Raster = list(
      write = raster::writeRaster,
      extension = ".tif",
      object = "x",
      path = "filename",
      param = list(format = "GTiff", NAflag = -999999)
    ),
    stars = list(
      write = stars::write_stars,
      extension = ".tif",
      object = "obj",
      path = "dsn",
      param = list(NA_value = -999999)
    ),
    SpatRaster = list(
      write = terra::writeRaster,
      extension = ".tif",
      object = "x",
      path = "filename",
      param = list(overwrite=FALSE,
                   NAflag = -999999)
    ),
    SpatVector = list(
      write = terra::writeVector,
      extension = ".shp",
      object = "x",
      path = "filename",
      param = list(overwrite=FALSE)
    ),
    LAS = list(
      write = lidR::writeLAS,
      extension = ".las",
      object = "las",
      path = "file",
      param = list()
    ),
    Spatial = list(
      write = writeSpatial,
      extension = ".shp",
      object = "x",
      path = "filename",
      param = list(overwrite = FALSE)
    ),
    sf = list(
      write = sf::st_write,
      extension = ".shp",
      object = "obj",
      path = "dsn",
      param = list(quiet = TRUE)
    ),
    data.frame = list(
      write = data.table::fwrite,
      extension = ".txt",
      object = "x",
      path = "file",
      param = list()
    )
  )

  data <- structure(list(
    File.Signature = "LASF",
    File.Source.ID = 0L,
    GUID = "",
    Version.Major = 1L,
    Version.Minor = 2L,
    System.Identifier = "",
    Generating.Software = "",
    File.Creation.Day.of.Year = 0L,
    File.Creation.Year = 0L,
    Header.Size = 227L,
    Offset.to.point.data = 321,
    Number.of.variable.length.records = 1,
    Point.Data.Format.ID = 1L,
    Point.Data.Record.Length = 28L,
    Number.of.point.records = 0L,
    X.scale.factor = 0.01,
    Y.scale.factor = 0.01,
    Z.scale.factor = 0.01,
    X.offset = 0,
    Y.offset = 0,
    Z.offset = 0,
    Max.X = 0,
    Min.X = 0,
    Max.Y = 0,
    Min.Y = 0,
    Max.Z = 0,
    Min.Z = 0,
    CRS = 0L,
    Number.of.1st.return = 0L,
    Number.of.2nd.return = 0L,
    Number.of.3rd.return = 0L,
    Number.of.4th.return = 0L,
    Number.of.5th.return = 0L,
    filename = ""),
    row.names = c(NA, -1L), class = "data.frame")

  data <- sf::st_sf(data, geometry = sf::st_sfc(lapply(1, function(x) sf::st_polygon() )))
  data <- data[-1,]

  .Object@data <- data

  .Object@chunk_options <- list(
    size = 0,
    buffer = 30,
    alignment = c(0,0),
    drop = NULL
  )

  .Object@processing_options <- list(
    progress = TRUE,
    stop_early = TRUE,
    wall_to_wall = TRUE
  )

  .Object@output_options <- list(
    output_files = "",
    drivers = drivers,
    merge = TRUE
  )

  .Object@input_options <- list(
    select = "*",
    filter = "",
    alt_dir = ""
  )

  .Object@index <- LIDRDEFAULTINDEX

  return(.Object)
})


