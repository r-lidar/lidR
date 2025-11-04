#' Add attributes into a LAS object
#'
#' A \link[=LAS-class]{LAS} object represents a las file in R. According to the
#' \href{https://www.asprs.org/divisions-committees/lidar-division/laser-las-file-format-exchange-activities}{LAS specifications}
#' a las file contains a core of defined attributes, such as XYZ coordinates, intensity, return number,
#' and so on, for each point. It is possible to add supplementary attributes.
#'
#' Users cannot assign names that are the same as the names of the core attributes. These functions are
#' dedicated to adding data that are not part of the LAS specification. For example, \code{add_lasattribute(las, x, "R")}
#' will fail because \code{R} is a name reserved for the red channel of a .las file that contains RGB
#' attributes. Use \code{add_lasrgb} instead.
#' \describe{
#' \item{\code{add_attribute}}{Simply adds a new column in the data but does not update the header. Thus the LAS
#' object is not strictly valid. These data will be temporarily usable at the R level but will not
#' be written in a las file with \link{writeLAS}.}
#'
#'\item{ \code{add_lasattribute}}{Does the same as \code{add_attribute} but automatically updates the header of
#' the LAS object. Thus, the LAS object is valid and the new data is considered as "extra bytes". This new
#' data will be written in a las file with \link{writeLAS}.}
#'
#' \item{\code{add_lasattribute_manual}}{Allows the user to manually write all the extra bytes metadata.
#' This function is reserved for experienced users with a good knowledge of the LAS specifications.
#' The function does not perform tests to check the validity of the information.
#' When using \code{add_lasattribute} and \code{add_lasattribute_manual}, \code{x} can only be of type numeric,
#' (\code{integer} or \code{double}). It cannot be of type \code{character} or \code{logical} as these are
#' not supported by the LAS specifications. The types that are supported in lidR are types 0 to 10
#' (Table 24 on page 25 of the specification). Types greater than 10 are not supported.}
#'
#' \item{\code{add_lasrgb}}{Adds 3 columns named RGB and updates the point format of the LAS object
#' for a format that supports RGB attributes. If the RGB values are ranging from 0 to 255 they are
#' automatically scaled on 16 bits.}
#' }
#'
#' @param las An object of class \link[=LAS-class]{LAS}
#' @param x a vector that needs to be added in the LAS object. For \code{add_lasattribute*} it can
#' be missing (see details).
#' @param name character. The name of the extra bytes attribute to add in the file.
#' @param desc character. A short description of the extra bytes attribute to add in the file (32 characters).
#' @param type character. The data type of the extra bytes attribute. Can be "uchar", "char", "ushort",
#' "short", "uint", "int", "uint64", "int64", "float", "double".
#' @param scale,offset numeric. The scale and offset of the data. NULL if not relevant.
#' @param NA_value numeric or integer. NA is not a valid value in a las file. At time of writing it will
#' be replaced by this value that will be considered as NA. NULL if not relevant.
#' @param R,G,B,NIR integer. RGB and NIR values. Values are automatically scaled to 16 bits if they are
#' coded on 8 bits (0 to 255).
#'
#' @return An object of class \link[=LAS-class]{LAS}
#'
#' @name add_attribute
#'
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las <- readLAS(LASfile, select = "xyz")
#'
#' print(las)
#' print(header(las))
#'
#' x <- 1:30
#'
#' las <- add_attribute(las, x, "mydata")
#' print(las)        # The las object has a new attribute called "mydata"
#' print(header(las)) # But the header has not been updated. This new data will not be written
#'
#' las <- add_lasattribute(las, x, "mydata2", "A new data")
#' print(las)        # The las object has a new attribute called "mydata2"
#' print(header(las)) # The header has been updated. This new data will be written
#'
#' # Optionally if the data is already in the LAS object you can update the header skipping the
#' # parameter x
#' las <- add_attribute(las, x, "newattr")
#' las <- add_lasattribute(las, name = "newattr", desc = "Amplitude")
#' print(header(las))
#'
#' # Remove an extra bytes attribute
#' las <- remove_lasattribute(las, "mydata2")
#' print(las)
#' print(header(las))
#'
#' las <- remove_lasattribute(las, "mydata")
#' print(las)
#' print(header(las))
NULL

#' @export
#' @rdname add_attribute
add_attribute = function(las, x, name)
{
  stopifnotlas(las)
  assert_is_a_string(name)
  stopif_forbidden_name(name)

  las@data[[name]] <- x
  return(las)
}

#' @export
#' @rdname add_attribute
add_lasattribute = function(las, x, name, desc)
{
  stopifnotlas(las)
  assert_is_a_string(name)
  assert_is_a_string(desc)
  stopif_forbidden_name(name)

  if (missing(x))
  {
    if (!name %in% names(las))
      stop(glue::glue("{name} is not an attribute of the LAS object."), call. = FALSE)

    x <- las@data[[name]]

    if (!is.numeric(x))
      stop(glue::glue("'{name}' must be numeric. LAS format specifications do not allow storing of '{class(las@data[[name]])}' extra bytes."), call. = FALSE)
  }
  else
  {
    assert_is_vector(x)

    if (!is.numeric(x))
      stop(glue::glue("'x' must be numeric. LAS format specifications do not allow storing of '{class(x)}' extra bytes."), call. = FALSE)

    las <- add_attribute(las, x, name)
  }

  header     <- as.list(las@header)
  new_header <- rlas::header_add_extrabytes(header, x, name, desc)
  new_header <- LASheader(new_header)
  las@header <- new_header
  return(las)
}

#' @export
#' @rdname add_attribute
add_lasattribute_manual = function(las, x, name, desc, type, offset = NULL, scale = NULL, NA_value = NULL)
{
  stopifnotlas(las)
  assert_is_a_string(name)
  assert_is_a_string(desc)
  assert_is_a_string(type)
  stopif_forbidden_name(name)

  types = c("uchar", "char", "ushort", "short", "uint", "int", "uint64", "int64", "float", "double")
  type = match.arg(type, types)
  type = which(type == types)

  if (missing(x))
  {
    if (!name %in% names(las))
      stop(glue::glue("{name} is not an attribute of the LAS object."), call. = FALSE)

    x <- las@data[[name]]

    if (!is.numeric(x))
      stop(glue::glue("'{name}' must be numeric. LAS format specifications do not allow storing of '{class(las@data[[name]])}' extra bytes."), call. = FALSE)
  }
  else
  {
    assert_is_vector(x)

    if (!is.numeric(x))
      stop(glue::glue("'x' must be numeric. LAS format specifications do not allow storing of '{class(x)}' extra bytes."), call. = FALSE)

    las <- add_attribute(las, x, name)
  }

  header     <- as.list(las@header)

  new_header <- rlas::header_add_extrabytes_manual(header, name, desc, type, offset, scale, NULL, NULL, NA_value)
  new_header <- LASheader(new_header)
  las@header <- new_header
  return(las)
}

#' @export
#' @rdname add_attribute
add_lasrgb <- function(las, R, G, B)
{
  stopifnotlas(las)
  stopifnot(is.integer(R), is.integer(G), is.integer(B))
  assert_are_same_length(R, G)
  assert_are_same_length(R, B)
  assert_is_of_length(R, npoints(las))

  maxr <- max(R, na.rm = TRUE)
  maxg <- max(G, na.rm = TRUE)
  maxb <- max(B, na.rm = TRUE)

  scale <- 1L
  if (maxr <= 255 & maxg <= 255 & maxb <= 255)
    scale <- 257L

  las@data$R <- R*scale
  las@data$G <- G*scale
  las@data$B <- B*scale

  format <- las@header@PHB[["Point Data Format ID"]]

  if (format %in% c(2,3,8))
  {
    # nothing to do
  }
  else if ("NIR" %in% names(las))
  {
    format <- 8L
  }
  else if ("gpstime" %in% names(las))
  {
    format <- 3L
  }
  else
  {
    format <- 2L
  }

  las@header@PHB[["Point Data Format ID"]] <- format
  return(las)
}

#' @export
#' @rdname add_attribute
add_lasnir <- function(las, NIR)
{
  stopifnotlas(las)
  stopifnot(is.integer(NIR))
  assert_is_of_length(NIR, npoints(las))

  maxnir <- max(NIR, na.rm = TRUE)

  scale <- 1L
  if (maxnir <= 255)
    scale <- 257L

  las@data$NIR <- NIR*scale
  las@header@PHB[["Point Data Format ID"]] <- 8L
  return(las)
}

#' @export
#' @rdname add_attribute
remove_lasattribute = function(las, name)
{
  stopifnotlas(las)
  assert_is_a_string(name)
  stopif_forbidden_name(name)

  if (!name %in% names(las))
  {
     message(glue::glue("{name} is not an attribute of the LAS object."))
     return(las)
  }

  eb <- las@header@VLR$Extra_Bytes$`Extra Bytes Description`[[name]]

  if (is.null(eb))
  {
    #message(glue::glue("{name} is not an extrabytes attribute of the LAS object."))
    las@data[[name]] <- NULL
    return(las)
  }
  else
  {
    las@header@VLR$Extra_Bytes$`Extra Bytes Description`[[name]] <- NULL
    las@data[[name]] <- NULL

    if (length(las@header@VLR$Extra_Bytes$`Extra Bytes Description`) == 0)
      las@header@VLR$Extra_Bytes <- NULL

    return(las)
  }
}

# type = 0 : undocumented
# type = 1 : unsigned char
# type = 2 : char
# type = 3 : unsigned short
# type = 4 : short
# type = 5 : unsigned int
# type = 6 : int
# type = 7 : unsigned int64
# type = 8 : int64
# type = 9 : float  (try not to use)
# type = 10 : double (try not to use)
