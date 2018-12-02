#' Add attributes into a LAS object
#'
#' A \link[lidR:LAS-class]{LAS} object represents a .las file in R. According to the
#' \href{https://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS specifications}
#' a las file contains a core of defined attributes, such as XYZ coordinates, intensity, return number,
#' and so on for each point. It is possible to add supplementary attributes. The functions \code{lasadd*}
#' enable the user to add new attributes (see details).
#'
#' Users cannot assign names that are the same as the names of the core attributes. These functions are dedicated
#' to adding data not part of the LAS specification. For example, \code{lasaddextrabytes(las, x, "R")}
#' will fail because \code{R} is a name reserved for the red channel of las file that contains RGB attributes.\cr\cr
#' \code{lasadddata} simply adds a new column in the data but does not update the header. Thus the LAS
#' object is not strictly valid. These data will be temporarily usable at the R level but will not
#' be written in a las file with \link{writeLAS}.\cr\cr
#' \code{lasaddextrabytes} does the same as \code{lasadddata} but automatically updates the header of the
#' LAS object. Thus, the LAS object is valid and the new data is considered as "extra bytes". This new
#' data will be written in a las file with \link{writeLAS}.\cr\cr
#' \code{lasaddextrabytes_manual} allows the user to manually write all the extra bytes metadata.
#' This function is reserved for experienced users with a good knowledge of the LAS specifications.
#' The function does not perform tests to check the validity of the information.\cr\cr
#' When using \code{lasaddextrabytes} and \code{lasaddextrabytes_manual}, \code{x} can only be of type numeric
#' (\code{integer} or \code{double}). It cannot be of types \code{character} or \code{logical} as these are
#' not supported by the las specifications. The types that are supported in lidR are types 0 to 10
#' (table 24 page 25 of the specification). Types greater than 10 are not supported.
#'
#' @param las An object of class \link[lidR:LAS-class]{LAS}
#' @param x a vector that needs to be added in the LAS object. For \code{lasaddextrabytes*} it can
#' be missing (see details).
#' @param name character. The name of the extra bytes attribute to add in the file.
#' @param desc character. A short description of the extra bytes attribute to add in the file (32 characters).
#' @param type character. The data type of the extra bytes attribute. Can be \code{"uchar", "char", "ushort", "short", "uint", "int", "uint64", "int64", "float", "double"}.
#' @param scale,offset numeric. The scale and offset of the data. NULL if not relevant.
#' @param NA_value numeric or integer. NA is not a valid value in a las file. At time of writing it will
#' be replaced by this value that will be considered as NA. NULL if not relevant.
#'
#' @return An object of class \link[lidR:LAS-class]{LAS}
#'
#' @name lasaddattribute
#'
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las <- readLAS(LASfile, select = "xyz")
#'
#' print(las)
#' print(las@header)
#'
#' x <- 1:30
#'
#' las <- lasadddata(las, x, "mydata")
#' print(las)        # The las object has a new attribute called "mydata"
#' print(las@header) # But the header has not been updated. This new data will not be written
#'
#' las <- lasaddextrabytes(las, x, "mydata2", "A new data")
#' print(las)        # The las object has a new attribute called "mydata2"
#' print(las@header) # The header has been updated. This new data will be written
#'
#' # Optionally if the data is already in the LAS object you can update the header skipping the
#' # parameter x
#' las <- lasaddextrabytes(las, name = "mydata", desc = "Amplitude")
#' print(las@header)
#'
#' # Remove an extra bytes attribute
#' las <- lasremoveextrabytes(las, "mydata2")
#' print(las)
#' print(las@header)
#'
#' las <- lasremoveextrabytes(las, "mydata")
#' print(las)
#' print(las@header)
NULL

#' @export
#' @rdname lasaddattribute
lasadddata = function(las, x, name)
{
  stopifnotlas(las)
  assert_is_a_string(name)
  stopif_forbidden_name(name)

  las@data[[name]] <- x
  return(las)
}

#' @export
#' @rdname lasaddattribute
lasaddextrabytes = function(las, x, name, desc)
{
  stopifnotlas(las)
  assert_is_a_string(name)
  assert_is_a_string(desc)
  stopif_forbidden_name(name)

  if (missing(x))
  {
    if (!name %in% names(las@data))
      stop(glue::glue("{name} is not an attribute of the LAS object."))

    x <- las@data[[name]]

    if (!is.numeric(x))
      stop(glue::glue("'{name}' must be numeric. LAS format specifications do not allow storing of '{class(las@data[[name]])}' extra bytes."))
  }
  else
  {
    assert_is_vector(x)

    if (!is.numeric(x))
      stop(glue::glue("'x' must be numeric. LAS format specifications do not allow storing of '{class(x)}' extra bytes."))

    las <- lasadddata(las, x, name)
  }

  header     <- as.list(las@header)
  new_header <- rlas::header_add_extrabytes(header, x, name, desc)
  new_header <- LASheader(new_header)
  las@header <- new_header
  return(las)
}

#' @export
#' @rdname lasaddattribute
lasaddextrabytes_manual = function(las, x, name, desc, type, offset = NULL, scale = NULL, NA_value = NULL)
{
  stopifnotlas(las)
  assert_is_a_string(name)
  assert_is_a_string(desc)
  assert_is_a_string(type)
  stopif_forbidden_name(name)

  type = match.arg(type, c("uchar", "char", "ushort", "short", "uint", "int", "uint64", "int64", "float", "double"))

  if (missing(x))
  {
    if (!name %in% names(las@data))
      stop(glue::glue("{name} is not an attribute of the LAS object."))

    x <- las@data[[name]]

    if (!is.numeric(x))
      stop(glue::glue("'{name}' must be numeric. LAS format specifications do not allow storing of '{class(las@data[[name]])}' extra bytes."))
  }
  else
  {
    assert_is_vector(x)

    if (!is.numeric(x))
      stop(glue::glue("'x' must be numeric. LAS format specifications do not allow storing of '{class(x)}' extra bytes."))

    las <- lasadddata(las, x, name)
  }

  header     <- as.list(las@header)
  new_header <- rlas::header_add_extrabytes_manual(header, name, desc, type, offset, scale, min(x, na.rm = TRUE), max(x, na.rm = TRUE), NA_value)
  new_header <- LASheader(new_header)
  las@header <- new_header
  return(las)
}

#' @export
#' @rdname lasaddattribute
lasremoveextrabytes = function(las, name)
{
  stopifnotlas(las)
  assert_is_a_string(name)
  stopif_forbidden_name(name)

  if (!name %in% names(las@data))
  {
     message(glue::glue("{name} is not an attribute of the LAS object."))
     return(las)
  }

  eb <- las@header@VLR$Extra_Bytes$`Extra Bytes Description`[[name]]

  if (is.null(eb))
  {
    message(glue::glue("{name} is not an extrabytes attribute of the LAS object."))
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

lasupdateheader = function(las)
{
  stopifnotlas(las)

  header     <- as.list(las@header)
  new_header <- rlas::header_update(header, las@data)
  new_header <- LASheader(new_header)
  las@header <- new_header
  return(las)
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
