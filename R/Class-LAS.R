#' An S4 class to represent the header of .las or .laz files
#'
#' An S4 class to represent the header of .las or .laz files according to the
#' \href{https://www.asprs.org/wp-content/uploads/2019/07/LAS_1_4_r15.pdf}{LAS file format specifications}.
#' A `LASheader` object contains a `list` in the slot `@PHB` with
#' the data read from the Public Header Block, a `list` in the slot `@VLR` with
#' the data read from the Variable Length Records and a `list` in the slot `EVLR` with the data read
#' from the Extended Variable Lenght Records.
#'
#' @slot PHB list. Represents the Public Header Block
#' @slot VLR list. Represents the Variable Length Records
#' @slot EVLR list. Represents the Extended Variable Length Records
#'
#' @exportClass LASheader
#' @md
setClass(
  Class = "LASheader",
  representation(PHB = "list", VLR = "list", EVLR = "list")
)

setMethod("initialize", "LASheader", function(.Object, data = list())
{
  if (is.data.frame(data))
  {
    data <- rlas::header_create(data)
    if (data[["X scale factor"]] == 1e-8)
      warning("A scale factor of 1e-8 was infered from the data when creating a LASheader. Data is likely to be over precise", call. = FALSE)
  }

  stopifnot(is.list(data))

  vlr <- list()
  if (!is.null(data[["Variable Length Records"]]))
    vlr <- data[["Variable Length Records"]]

  evlr <- list()
  if (!is.null(data[["Extended Variable Length Records"]]))
    evlr <- data[["Extended Variable Length Records"]]

  .Object@PHB <- data
  .Object@PHB[["Variable Length Records"]] <- NULL
  .Object@PHB[["Extended Variable Length Records"]] <- NULL
  .Object@VLR <- vlr
  .Object@EVLR <- evlr

  return(.Object)
})


#' An S4 class to represent a .las or .laz file
#'
#' Class LAS is the representation of a las/laz file according to the
#' \href{https://www.asprs.org/wp-content/uploads/2019/07/LAS_1_4_r15.pdf}{LAS file format specifications}.
#'
#' A `LAS` object contains a `data.table` with the data read from a `las/laz` file and
#' a \link[=LASheader-class]{LASheader} (see the ASPRS documentation for the
#' \href{https://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}
#' for more information). Because las files are standardized the table of attributes read from the las/laz file
#' is also standardized. Columns are named:
#' \itemize{
#' \item{`X`, `Y`, `Z` (numeric)}
#' \item{`gpstime` (numeric)}
#' \item{`Intensity` (integer)}
#' \item{`ReturnNumber`, `NumberOfReturns` (integer)}
#' \item{`ScanDirectionFlag` (integer)}
#' \item{`EdgeOfFlightline` (integer)}
#' \item{`Classification` (integer)}
#' \item{`Synthetic_flag`,`Keypoint_flag`, `Withheld_flag`  (logical)}
#' \item{`ScanAngleRank`/`ScanAngle`  (integer/numeric)}
#' \item{`UserData` (integer)}
#' \item{`PointSourceID` (integer)}
#' \item{`R`,`G`,`B`, `NIR` (integer)}
#' }
#'
#' @slot crs Object of class \link[sf:st_crs]{crs} from sf.
#' @slot data Object of class \link[data.table:data.table]{data.table}. Point cloud data according to the
#' \href{https://www.asprs.org/wp-content/uploads/2019/07/LAS_1_4_r15.pdf}{LAS file format}
#' @slot header Object of class \link[=LASheader-class]{LASheader}. LAS file header according to the
#' \href{https://www.asprs.org/wp-content/uploads/2019/07/LAS_1_4_r15.pdf}{LAS file format}
#' @slot index list. See \link[=lidR-spatial-index]{spatial indexing}.
#'
#' @export
#' @md
#' @examples
#' # Read a las/laz file
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las <- readLAS(LASfile)
#' las
#'
#' # Creation of a LAS object out of external data
#' data <- data.frame(X = runif(100, 0, 100),
#'                    Y = runif(100, 0, 100),
#'                    Z = runif(100, 0, 20))
#'
#' # 'data' has many decimal digits
#' data
#'
#' # Create a default header and quantize *by reference*
#' # the coordinates to fit with offset and scale factors
#' cloud <- LAS(data)
#'
#' # 'data' has been updated and coordinates were quantized
#' data
#' cloud
#'
#' # Be careful when providing a header the function assumes that
#' # it corresponds to the data and won't quantize the coordinates
#' data <- data.frame(X = runif(100, 0, 100),
#'                    Y = runif(100, 0, 100),
#'                    Z = runif(100, 0, 20))
#' header <- header(las)
#'
#' # This works but triggers warnings and creates an invalid LAS object
#' cloud <- LAS(data, header)
#'
#' las_check(cloud)
#' @seealso
#' \link{readLAS}
setClass(
  Class = "LAS",
  representation(data = "data.table", header = "LASheader", crs = "crs", index = "list")
)

setMethod("initialize", "LAS", function(.Object)
{
  x      <- numeric(0)
  data   <- data.table::data.table(X = x, Y = x, Z = x)
  header <- suppressWarnings(rlas::header_create(data))
  header$`System Identifier` <- "lidR R package"
  header$`Generating Software` <- "lidR R package"
  header$`Min X` <- 0
  header$`Max X` <- 0
  header$`Min Y` <- 0
  header$`Max Y` <- 0
  header$`Min Z` <- 0
  header$`Max Z` <- 0
  header$`X offset` <- 0
  header$`Y offset` <- 0
  header$`Z offset` <- 0

  .Object@crs         <- sf::NA_crs_
  .Object@header      <- LASheader(header)
  .Object@data        <- data
  .Object@index       <- LIDRDEFAULTINDEX

  return(.Object)
})

