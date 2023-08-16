#' @param data a \link[data.table:data.table]{data.table} containing the data of a las or laz file.
#' @param header a \code{list} or a \link[=LASheader-class]{LASheader} containing the header of
#' a las or laz file.
#' @param crs crs object of class \link[sf:st_crs]{crs} from sf
#' @param check logical. Conformity tests while building the object.
#' @param index list with two elements \code{list(sensor = 0L, index = 0L)}.
#' See \link[=lidR-spatial-index]{spatial indexing}
#' @param ... internal use
#' @return An object of class \code{LAS}
#' @export
#' @describeIn LAS-class creates objects of class LAS. The original data is updated by reference to
#' quantize the coordinates according to the scale factor of the header if no header is provided.
#' In this case the scale factor is set to 0.001
LAS <- function(data, header = list(), crs = sf::NA_crs_, check = TRUE, index = NULL, ...)
{
  .N <- X <- Y <- Z <- NULL

  # This is a repair feature for lidR v3 to lidR v4
  #nocov start
  if (is(data, "LAS"))
  {
    if (methods::.hasSlot(data, "proj4string"))
    {
      pts  <- payload(data)
      phb  <- phb(data)
      vlr  <- vlr(data)
      crs  <- st_crs(data)
      evlr <- evlr(data)
      head <- c(phb, vlr, evlr)
      return(LAS(pts, head, crs = crs, check = FALSE))
    }


    return(data)
  }

  # For backward compatibility
  dots <- list(...)
  if (!is.null(dots$proj4string))
  {
    warning("Argument proj4string is deprecated. Use argument crs instead.", call. = TRUE)
    crs <- sf::st_crs(dots$proj4string)
  }
  #nocov end

  if (is(data, "lasmetrics3d"))
    class(data) <- class(data)[-1]

  if (is.data.frame(data) & !data.table::is.data.table(data))
    data.table::setDT(data)

  if (!data.table::is.data.table(data))
    stop("Invalid parameter data in constructor.")

  if (is.null(dots$no_attr_name_check))
    names(data) <- fix_name_convention(names(data))

  rlas::is_defined_coordinates(data, "stop")
  rlas::is_valid_XYZ(data, "stop")

  if (is(header, "LASheader"))
    header <- as.list(header)

  if (!is.list(header))
    stop("Wrong header object provided.")

  if (length(header) == 0) {
    xyzfactor <- 0.001
    header <- rlas::header_create(data)
    header[["X scale factor"]] <- xyzfactor
    header[["Y scale factor"]] <- xyzfactor
    header[["Z scale factor"]] <- xyzfactor
    xoffset <- floor(header[["X offset"]])
    yoffset <- floor(header[["Y offset"]])
    zoffset <- 0
    header[["X offset"]] <- xoffset
    header[["Y offset"]] <- yoffset
    header[["Z offset"]] <- zoffset

    if (nrow(data) > 0)
    {
      fast_quantization(data[["X"]], xyzfactor, xoffset)
      fast_quantization(data[["Y"]], xyzfactor, yoffset)
      fast_quantization(data[["Z"]], xyzfactor, zoffset)
      message(glue::glue("Creation of a LAS object from data but without a header:
      Scale factors were set to {xyzfactor} and XYZ coordinates were quantized to fit the scale factors."))
    }
  }

  header <- rlas::header_update(header, data)

  if (check & nrow(data) > 0)
  {
    rlas::is_defined_offsets(header, "stop")
    rlas::is_defined_scalefactors(header, "stop")
    rlas::is_defined_filesourceid(header, "stop")
    rlas::is_defined_version(header, "stop")
    rlas::is_defined_globalencoding(header, "stop")
    rlas::is_defined_date(header, "stop")

    rlas::is_defined_coordinates(data, "stop")

    rlas::is_valid_scalefactors(header, "warning")
    rlas::is_valid_globalencoding(header, "stop")
    rlas::is_valid_date(header, "stop")
    rlas::is_valid_pointformat(header, "stop")
    rlas::is_valid_extrabytes(header, "stop")

    rlas::is_valid_Intensity(data, "stop")
    rlas::is_valid_ReturnNumber(header, data, "stop")
    rlas::is_valid_NumberOfReturns(header, data, "stop")
    rlas::is_valid_ScanDirectionFlag(data, "stop")
    rlas::is_valid_EdgeOfFlightline(data, "stop")
    rlas::is_valid_Classification(data, header, "stop")
    rlas::is_valid_ScannerChannel(data, "stop")
    rlas::is_valid_SyntheticFlag(data, "stop")
    rlas::is_valid_KeypointFlag(data, "stop")
    rlas::is_valid_WithheldFlag(data, "stop")
    rlas::is_valid_OverlapFlag(data, "stop")
    rlas::is_valid_ScanAngleRank(data, "stop")
    rlas::is_valid_ScanAngle(data, "stop")
    rlas::is_valid_UserData(data, "stop")
    rlas::is_valid_gpstime(data, "stop")
    rlas::is_valid_PointSourceID(data, "stop")
    rlas::is_valid_RGB(data, "stop")
    rlas::is_valid_NIR(data, "stop")

    rlas::is_NIR_in_valid_format(header, data, "warning")
    rlas::is_gpstime_in_valid_format(header, data, "warning")
    rlas::is_RGB_in_valid_format(header, data, "warning")
    rlas::is_ScanAngle_in_valid_format(header, data, "warning")
    rlas::is_ScannerChannel_in_valid_format(header, data, "warning")
    rlas::is_extrabytes_in_accordance_with_data(header, data, "warning")

    rlas::is_compliant_ReturnNumber(data, "warning")
    rlas::is_compliant_NumberOfReturns(data, "warning")
    rlas::is_compliant_RGB(data, "warning")
    rlas::is_compliant_ScanAngleRank(data, "warning")
    rlas::is_compliant_ScanAngle(data, "warning")
    rlas::is_compliant_ReturnNumber_vs_NumberOfReturns(data, "warning")
    rlas::is_XY_larger_than_bbox(header, data, "warning")
    rlas::is_number_of_points_in_accordance_with_header(header, data, "warning")
    rlas::is_number_of_points_by_return_in_accordance_with_header(header, data, "warning")
    rlas::is_XY_smaller_than_bbox(header, data, "warning")
    rlas::is_Z_in_bbox(header, data, "warning")
    rlas::is_RGB_in_valid_format(header, data, "warning")
    rlas::is_NIR_in_valid_format(header, data, "warning")
    rlas::is_gpstime_in_valid_format(header, data, "warning")
    rlas::is_ScanAngle_in_valid_format(header, data, "warning")

    xscale <- header[["X scale factor"]]
    yscale <- header[["Y scale factor"]]
    zscale <- header[["Z scale factor"]]
    xoffset <- header[["X offset"]]
    yoffset <- header[["Y offset"]]
    zoffset <- header[["Z offset"]]

    if (!is.quantized(data[["X"]], xscale, xoffset, sample = TRUE))
      warning("Detection of quantization errors for X", call. = FALSE)
    if (!is.quantized(data[["Y"]], yscale, yoffset, sample = TRUE))
      warning("Detection of quantization errors for Y", call. = FALSE)
    if (!is.quantized(data[["Z"]], zscale, zoffset, sample = TRUE))
      warning("Detection of quantization errors for Z", call. = FALSE)
  }

  header <- LASheader(header)

  if (is.na(crs))
    crs <- st_crs(header)

  if (is.null(index))
    index <- LIDRDEFAULTINDEX

  index$xprt <- NULL

  las        <- new("LAS")
  las@header <- header
  las@data   <- data
  las@crs    <- crs
  las@index  <- index

  if (isTRUE(sf::st_is_longlat(crs)))
    warning("Point-clouds with lon/lat coordinates are not well supported.", call. = FALSE)

  return(las)
}

#' Extract or Replace Parts of a LAS* Object
#'
#' Operators acting on \code{LAS*} objects. However, some have modified behaviors to prevent some
#' irrelevant modifications. Indeed, a \code{LAS*} object cannot contain anything, as the content
#' is restricted by the LAS specifications. If a user attempts to use one of these functions
#' inappropriately an informative error will be thrown.
#'
#' @param x A \code{LAS*} object
#' @param name A literal character string or a name (possibly backtick quoted).
#' @param value typically an array-like R object of a similar class as x.
#' @param i string, name of elements to extract or replace.
#' @param j Unused.
#'
#' @name Extract
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las = readLAS(LASfile)
#'
#' las$Intensity
#' las[["Z"]]
#' las[["Number of points by return"]]
#'
#' \dontrun{
#' las$Z = 2L
#' las[["Z"]] = 1:10
#' las$NewCol = 0
#' las[["NewCol"]] = 0
#' }
NULL

#' @export
#' @rdname Extract
setMethod("$", "LAS", function(x, name) { x[[name]] })

#' @export
#' @rdname Extract
setMethod("[[", c("LAS", "ANY", "missing"), function(x, i, j, ...) {

  if (is.character(i) && !i %in% names(x@data))
    return(x@header[[i]])

  return(x@data[[i]])
})

#' @export
#' @rdname Extract
setMethod("$<-", "LAS", function(x, name, value)
{
  x[[name]] <- value
  return(x)
})

#' @export
#' @rdname Extract
setMethod("[[<-", c("LAS", "ANY", "missing", "ANY"),  function(x, i, j, value)
{
  if (i %in% names(header(x)))
  {
    x@header[[i]] <- value
    return(x)
  }

  if (!i %in% names(x))
    stop("Addition of a new column using [[ is forbidden for LAS objects. See ?add_attribute", call. = FALSE)

  if (i %in% LASATTRIBUTES)
  {
    type1 <- storage.mode(x@data[[i]])
    type2 <- storage.mode(value)

    if (type1 != type2)
      stop(glue::glue("Trying to replace data of type {type1} by data of type {type2}: this action is not allowed"), call. = FALSE)
  }

  if (!i %in% names(x@data))
    stop("Addition of a new column using $ is forbidden for LAS objects. See ?add_attribute", call. = FALSE)

  if (i %in% LASATTRIBUTES)
  {
    type1 <- storage.mode(x@data[[i]])
    type2 <- storage.mode(value)

    if (type1 != type2)
      stop(glue::glue("Trying to replace data of type {type1} by data of type {type2}: this action is not allowed"), call. = FALSE)
  }

  for (coordinate_name in c("X", "Y", "Z"))
  {
    if (i == coordinate_name)
    {
      scale_string <- paste(i, "scale factor")
      offset_string <- paste(i, "offset")
      scale  <- x[[scale_string]]
      offset <- x[[offset_string]]
      valid_range <- storable_coordinate_range(scale, offset)
      value_range <- range(value)

      if (value_range[1] < valid_range[1] | value_range[2] > valid_range[2])
        stop(glue::glue("Trying to store values ranging in [{value_range[1]}, {value_range[2]}] but storable range is [{valid_range[1]}, {valid_range[2]}]"), call. = FALSE)

      if(!is.quantized(value, scale, offset))
        value <- quantize(value, scale, offset, FALSE)
    }
  }

  x@data[[i]] = value

  if (i %in% c("X", "Y", "Z"))
    x <- las_update(x)

  return(x)
})

#' @export
#' @rdname Extract
setMethod("[", c("LAS", "numeric"),  function(x, i)
{
  return(smart_subset(x, i))
})

#' @export
#' @rdname Extract
setMethod("[", c("LAS", "logical"),  function(x, i)
{
  return(smart_subset(x, i))
})

#' @export
#' @rdname Extract
setMethod("[", c("LAS", "sf"),  function(x, i)
{
  return(x[sf::st_geometry(i)])
})

#' @export
#' @rdname Extract
setMethod("[", c("LAS", "sfc"),  function(x, i)
{
  if (length(i) > 1)
    stop("A single geometry feature is supported for subsetting. See clip_roi() for more options", call. = FALSE)

  i <- i[[1]]

  if (!is(i, "POLYGON") && !is(i, "MULTIPOLYGON"))
    stop("Only POLYGONS are supported for subsetting. See clip_roi() for more options", call. = FALSE)

  return(clip_roi(x, i))
})

fix_name_convention <- function(names)
{
  input <- tolower(names)
  lasattr <- tolower(LASATTRIBUTES)
  idx <- match(input, lasattr)
  new <- LASATTRIBUTES[idx]
  changed <- new != names
  for (i in which(changed)) message(glue::glue("Attribute '{names[i]}' renamed '{new[i]}' to match with default attribute names."))
  return(new)
}

smart_subset <- function(las, i)
{
  data <- payload(las)

  # Start a copy by preserving addresses
  # data[, c("X", "Y", "Z")] makes deeps copies
  new_data <- data.frame(X = data[["X"]], Y = data[["Y"]], Z = data[["Z"]])
  data.table::setDT(new_data)

  # Subset the coodinate
  new_data <- new_data[i]

  # Iteratively add subseted elements in a way that preserve ALTREP
  attr <- names(las)
  attr <- attr[!attr %in% c("X", "Y", "Z")]
  for (name in attr) new_data[[name]] <- data[[name]][i]

  new_las <- LAS(new_data, las@header, st_crs(las), FALSE, las@index, no_attr_name_check = TRUE)
  new_las <- las_update(new_las)
  return(new_las)
}
