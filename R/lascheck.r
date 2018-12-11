# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2018 Jean-Romain Roussel
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

#' Inspect a LAS object
#'
#' Performs a deep inspection of a LAS or LAScatalog object and prints a report.\cr\cr
#' For a LAS object it checks:
#' \itemize{
#' \item if the point cloud is valid according to las specification
#' \item if the header is valid according to las specification
#' \item if the point cloud is in accordance with the header
#' \item if the point cloud has duplicated points and degenerated ground points
#' \item it the coordinate reference sytem is correctly recorded
#' \item if some pre-processing, such as normalization or ground filtering, is already done.
#' }
#' For a LAScatalog object it checks:
#' \itemize{
#' \item if the headers are consistent across files
#' \item if the files are overlapping
#' \item if some pre-processing, such as normalization, is already done.
#' }
#' For the pre-processing tests the function only makes an estimation and may not be correct.
#'
#' @template param-las
#' @export
lascheck = function(las)
{
  UseMethod("lascheck", las)
}

#' @export
lascheck.LAS = function(las)
{
  Classification <- Z <- NULL

  data <- las@data
  phb  <- las@header@PHB
  vlr  <- las@header@VLR
  g    <- glue::glue

  if (requireNamespace("crayon", quietly = TRUE))
  {
    green = crayon::green
    red = crayon::red
    orange = crayon::yellow
    silver = crayon::silver
  }
  else
  {
    green <- red <- orange <- silver <- function(x) { return(x) }
  }

  h1    <- function(x) {cat("\n", x)}
  h2    <- function(x) {cat("\n  -", x)}
  ok    <- function()  {cat(green(" ok"))}
  fail  <- function(x) {cat("\n", red(g("   error: {x}")))}
  warn  <- function(x) {cat("\n", orange(g("   warning: {x}")))}
  skip  <- function()  {cat(silver(g(" skipped")))}
  no    <- function()  {cat(red(g(" no")))}
  yes   <- function()  {cat(green(g(" yes")))}
  maybe <- function()  {cat(orange(g(" maybe")))}

  # ==== data =====

  h1("Checking the data")

  h2("Checking coordinates...")

  if (is.null(data$X) | is.null(data$Y) | is.null(data$Z))
    fail("Invalid data: missing coordinates X or Y or Z")
  else
    ok()

  h2("Checking coordinates type...")

  if (!is.double(data$X) | !is.double(data$Y) | !is.double(data$Z))
    fail("Invalid data: coordinates are not of type double")
  else
    ok()

  h2("Checking attributes type...")

  attributes <- c("gpstime")
  which      <- c()

  for (attr in attributes)
  {
    if (!is.null(data[[attr]]))
    {
      if (!is.numeric(data[[attr]]))
        which = append(which, attr)
    }
  }

  if (length(which) > 0)
  {
    which  = paste(which, collapse = ", ")
    string = paste("The following attributes are not of type integer:", which)
    fail(string)
  }
  else
    ok()

  h2("Checking attributes type...")

  attributes <- c("Intensity", "ReturnNumber", "NumberOfReturns", "ScanDirectionFlag", "EdgeOfFlightline", "Classification", "UserData", "ScanAngle", "PointSourceID", "R", "G", "B", "NIR")
  which      <- c()

  for (attr in attributes)
  {
    if (!is.null(data[[attr]]))
    {
      if (!is.numeric(data[[attr]]))
        which = append(which, attr)
    }
  }

  if (length(which) > 0)
  {
    which  = paste(which, collapse = ", ")
    string = paste("The following attributes are not of type integer:", which)
    fail(string)
  }
  else
    ok()

  h2("Checking attributes type...")

  attributes <- c("Synthe tic_flag", "Keypoint_flag", "Withheld_flag")
  which      <- c()

  for (attr in attributes)
  {
    if (!is.null(data[[attr]]))
    {
      if (!is.logical(data[[attr]]))
        which = append(which, attr)
    }
  }

  if (length(which) > 0)
  {
    which  = paste(which, collapse = ", ")
    string = paste("The following attributes are not of type logical:", which)
    fail(string)
  }
  else
    ok()

  h2("Checking ReturnNumber validity...")

  if (!is.null(data$ReturnNumber))
  {
    s = fast_countequal(data$ReturnNumber, 0L)

    if (s > 0)
      fail(g("Invalid data: {s} points with a return number equal to 0 found."))
    else
      ok()
  }
  else
    skip()

  h2("Checking NumberOfReturns validity...")

  if (!is.null(data$NumberOfReturns))
  {
    s = fast_countequal(data$NumberOfReturns, 0L)

    if (s > 0)
      fail(g("Invalid data: {s} points with a number of returns equal to 0 found."))
    else
      ok()
  }
  else
    skip()

  h2("Checking ReturnNumber vs. NumberOfReturns...")

  if (!is.null(data$ReturnNumber) & !is.null(data$NumberOfReturns))
  {
    s = sum(data$ReturnNumber > data$NumberOfReturns)

    if (s > 0)
      fail(g("Invalid data: {s} points with a 'return number' greater than the 'number of returns'."))
    else
      ok()
  }
  else
    skip()

  h2("Checking Classification validity...")

  if (!is.null(data$Classification))
  {
    s = fast_countequal(data$Classification, 0L)

    if (s > 0 & s < nrow(data))
      warn(g("{s} unclassified points found."))
    else
      ok()
  }
  else
    skip()

  h2("Checking RGB validity...")

  attributes <- c("R", "G", "B")
  which = c()

  if (any(attributes %in% names(data)))
  {
    for (attr in attributes)
    {
      if (!is.null(data[[attr]]))
      {
        max = max(data[[attr]])

        if (max > 0 & max <= 255)
          which = append(which, attr)
      }
    }

    if (length(which) > 0)
      warn("Invalid data: RGB colors are recorded on 8 bits instead of 16 bits.")
    else
      ok()
  }
  else
    skip()

  h2("Checking absence of NAs...")

  nas = data[, lapply(.SD, anyNA)]
  nas = unlist(as.list(nas))
  nas = nas[nas == TRUE]

  if (length(nas) > 0)
  {
    which  = names(nas)
    which  = paste(nas, collapse = ", ")
    string = paste("The following attributes contain NAs:", which)
    fail(string)
  }
  else
    ok()


  h2("Checking duplicated points...")

  s = sum(duplicated(data, by = c("X", "Y", "Z")))

  if (s > 0)
    warn(g("{s} points are duplicated and share XYZ coordinates with other points"))
  else
    ok()

  h2("Checking degenerated ground points...")

  if (!is.null(data$Classification))
  {
    s = fast_countequal(data$Classification, 2L)

    if (s > 0)
    {
      gnd = data[Classification == 2L]

      s1 = duplicated(gnd, by = c("X", "Y", "Z"))
      s2 = duplicated(gnd, by = c("X", "Y"))  & !s1
      s1 = sum(s1)
      s2 = sum(s2)

      if (s1 == 0 & s2 == 0)
        ok()
      else
      {
        if (s1 > 0)
          warn(g("There were {s1} degenerated ground points. Some X Y Z coordinates were repeated."))

        if (s2 > 0)
          warn(g("There were {s2} degenerated ground points. Some X Y coordinates were repeated but with different Z coordinates."))

      }
    }
    else
      skip()
  }
  else
    skip()

  # ==== header ====

  h1("Checking the header")

  h2("Checking header completeness...")

  attributes <- c("X offset", "Y offset", "Z offset", "X scale factor", "Y scale factor", "Z scale factor", "File Source ID",
                  "Version Major", "Version Minor", "File Creation Day of Year", "File Creation Year", "Point Data Format ID",
                  "X scale factor", "Y scale factor", "Z scale factor")
  which      <- c()

  for (attr in attributes)
  {
    if (is.null(phb[[attr]]))
      which = append(which, attr)
  }

  if (length(which) > 0)
  {
    which  = paste(which, collapse = ", ")
    string = paste("Invalid header block: the following fields are missing:", which)
    fail(string)
  }
  else
    ok()

  h2("Checking scale factor validity...")

  failure = FALSE

  if (!is.null(phb[["X scale factor"]]) & !is.null(phb[["Y scale factor"]]) & !is.null(phb[["Z scale factor"]]))
  {
    s = c(1,10,100,1000,10000)
    valid = c(1/s, 0.5/s, 0.25/s)

    if (!phb[["X scale factor"]] %in% valid)
    { fail(paste0("Invalid header: X scale factor should be factor ten of 0.1 or 0.5 or 0.25 and not ", phb[["X scale factor"]])) ; failure = TRUE }

    if (!phb[["Y scale factor"]] %in% valid)
    { fail(paste0("Invalid header: Y scale factor should be factor ten of 0.1 or 0.5 or 0.25 and not ", phb[["Y scale factor"]])) ; failure = TRUE }

    if (!phb[["Z scale factor"]] %in% valid)
    { fail(paste0("Invalid header: Z scale factor should be factor ten of 0.1 or 0.5 or 0.25 and not ", phb[["Z scale factor"]])) ; failure = TRUE }

    if (!failure)
      ok()
  }
  else
    skip()

  h2("Checking Point Data Format ID validity...")

  if (!is.null(phb[["Point Data Format ID"]]))
  {
    if (phb[["Point Data Format ID"]] %in% c(4,5,9,10))
      warn(paste0("Invalid header: The point data format ", phb[["Point Data Format ID"]], " is not supported yet."))
    else if (phb[["Point Data Format ID"]] < 0 | phb[["Point Data Format ID"]] > 10)
      fail(paste0("Invalid header: The point data format ", phb[["Point Data Format ID"]], " is invalid."))
    else
      ok()
  }
  else
    skip()

  h2("Checking extra bytes attributes validity...")

  if (!is.null(vlr$Extra_Bytes$`Extra Bytes Description`))
  {
    failure = FALSE
    for (extra_byte in vlr$Extra_Bytes$`Extra Bytes Description`)
    {
      if (is.null(extra_byte[["options"]]))
      { fail("Invalid header: 'options' is missing in extra bytes description") ; failure = TRUE }

      if (!is.integer(extra_byte[["options"]]))
      { fail("Invalid header: 'options' must be an integer in extra bytes description") ; failure = TRUE }

      if (is.null(extra_byte[["data_type"]]))
      { fail("Invalid header: 'data_type' is missing in extra bytes description") ; failure = TRUE }

      if (extra_byte[["data_type"]] < 1L & extra_byte[["data_type"]] > 10L)
      { fail("Invalid header: 'data_type' must be between 1 and 10 in extra bytes description") ; failure = TRUE }

      if (is.null(extra_byte[["name"]]))
      { fail("Invalid header: 'name' is missing in extra bytes description") ; failure = TRUE }

      if (!is.character(extra_byte[["name"]]) | length(extra_byte[["name"]]) > 1L)
      { fail("Invalid header: 'name' must be a string in extra bytes description") ; failure = TRUE }

      if (nchar(extra_byte[["name"]]) > 32L)
      { fail("Invalid header: 'name' must be a string of length < 32 in extra bytes description") ; failure = TRUE }

      if (is.null(extra_byte[["description"]]))
      { fail("Invalid header: 'description' is missing in extra bytes description") ; failure = TRUE }

      if (!is.character(extra_byte[["description"]]) | length(extra_byte[["description"]]) > 1L)
      { fail("Invalid header: 'description' must be a string in extra bytes description") ; failure = TRUE }

      if (nchar(extra_byte[["description"]]) > 32L)
      { fail("Invalid header: 'description' must be a string of length < 32 in extra bytes description") ; failure = TRUE }

      options = extra_byte[["options"]]
      options = as.integer(intToBits(options))[1:5]

      has_no_data = options[1] == 1L;
      has_min = options[2] == 1L;
      has_max = options[3] == 1L;
      has_scale = options[4] == 1L;
      has_offset = options[5] == 1L;

      if (is.null(extra_byte[["min"]]) & has_min)
      { fail("Invalid header: 'min' is missing in extra bytes description") ; failure = TRUE }

      if (is.null(extra_byte[["max"]]) & has_max)
      { fail("Invalid header: max' is missing in extra bytes description") ; failure = TRUE }

      if (is.null(extra_byte[["scale"]]) & has_scale)
      { fail("Invalid header: 'scale' is missing in extra bytes description") ; failure = TRUE }

      if (is.null(extra_byte[["offset"]]) & has_offset)
      { fail("Invalid header: 'offset' is missing in extra bytes description") ; failure = TRUE }
    }

    if (!failure)
      ok()
  }
  else
    ok()

  h2("Checking coordinate reference sytem...")

  code = epsg(las)
  lasproj = las@proj4string
  codeproj = tryCatch(sp::CRS(glue::glue("+init=epsg:{code}")), error = function(e) return(sp::CRS()))
  failure = FALSE

  if (code != 0)
  {
    if (is.na(codeproj@projargs))
    { fail("EPSG code unknown.") ; failure = TRUE }

    if (is.na(codeproj@projargs) & !is.na(lasproj@projargs))
    { fail("ESPG code and proj4string do not match.") ; failure = TRUE }

    if (!is.na(codeproj@projargs) & is.na(lasproj@projargs))
    { fail("ESPG code and proj4string do not match.") ; failure = TRUE }

    if (!is.na(codeproj@projargs) & !is.na(lasproj@projargs))
    {
      if (codeproj@projargs != lasproj@projargs)
      { fail("ESPG code and proj4string do not match.") ; failure = TRUE }
    }

    if (!failure)
      ok()
  }
  else
  {
    if (!is.na(lasproj@projargs))
    { warn("A proj4string found but no EPSG code in the header") ; failure = TRUE }

    if (!failure)
      ok()
  }



  # ==== data vs header ====

  h1("Checking header vs data adequacy")

  h2("Checking attributes vs. point format...")

  format = phb$`Point Data Format ID`
  fields = names(data)
  failure = FALSE

  if (!is.null(format))
  {
    if ("NIR" %in% fields & format != 8)
    { fail("Invalid file: the data contains a 'NIR' attribute but point data format is not set to 8.") ; failure = TRUE }

    if ("gpstime" %in% fields & !format %in% c(1,3,6,7,8))
    { fail("Invalid file: the data contains a 'gpstime' attribute but point data format is not set to 1, 3, 6, 7 or 8.") ; failure = TRUE }

    if (any(c("R", "G", "B") %in% fields) & !format %in% c(2,3,8))
    { fail("Invalid file: the data contains 'RGB' attributes but point data format is not set to 2, 3 or 8.") ; failure = TRUE }

    if (!failure)
      ok()
  }
  else
    skip()

  h2("Checking header bbox vs. actual content...")

  failure = FALSE

  if (max(data$X) > phb$`Max X` | max(data$Y) > phb$`Max Y` | min(data$X) < phb$`Min X` | min(data$Y) < phb$`Min Y`)
  { fail("Invalid file: some points are outside the bounding box defined by the header") ; failure = TRUE }

  if (max(data$Z) > phb$`Max Z` |  min(data$Z) < phb$`Min Z`)
  { fail("Invalid file: some points are outside the elevation range defined by the header") ; failure = TRUE }

  if (!failure)
    ok()

  h2("Checking header number of points vs. actual content...")

  if (dim(data)[1] != phb["Number of point records"])
    fail(paste0("Invalid file: header states the file contains ", phb["Number of point records"], " points but ", nrow(data), " were found."))
  else
    ok()

  h2("Checking header return number vs. actual content...")

  failure = FALSE

  if ("ReturnNumber" %in% fields)
  {
    number_of <- fast_table(data$ReturnNumber, 5L)

    if (phb["Number of 1st return"] != number_of[1])
    { fail(paste0("Invalid file: the header states the file contains ", phb["Number of 1st return"], " 1st returns but ", number_of[1], " were found.")) ; failure = TRUE }

    if (phb["Number of 2nd return"] != number_of[2])
    { fail(paste0("Invalid file: the header states the file contains ", phb["Number of 2nd return"], " 2nd returns but ", number_of[2], " were found.")) ; failure = TRUE }

    if (phb["Number of 3rd return"] != number_of[3])
    { fail(paste0("Invalid file: the header states the file contains ", phb["Number of 3rd return"], " 3rd returns but ", number_of[3], " were found.")) ; failure = TRUE }

    if (phb["Number of 4th return"] != number_of[4])
    { fail(paste0("Invalid file: the header states the file contains ", phb["Number of 4th return"], " 4th returns but ", number_of[4], " were found.")) ; failure = TRUE }

    if (phb["Number of 5th return"] != number_of[5])
    { fail(paste0("Invalid file: the header states the file contains ", phb["Number of 5th return"], " 5th returns but ", number_of[5], " were found.")) ; failure = TRUE }

    if (!failure)
      ok()
  }
  else
    skip()

  # ==== Preprocessing ====

  h1("Checking preprocessing already done ")

  h2("Checking ground classification...")

  if (!is.null(data$Classification))
  {
    s = fast_countequal(data$Classification, 2L)

    if (s > 0)
      yes()
    else
      no()
  }
  else
    no()

  h2("Checking normalization...")

  min = grid_metrics(las, ~min(Z), res = 20)
  mean_min = mean(abs(min[]), na.rm = TRUE)

  if (mean_min <= 0.1)
    yes()
  else if (mean_min > 0.1 & mean_min < 1)
    maybe()
  else
    no()

  h2("Checking negative outliers...")

  s = fast_countbelow(data$Z, 0)

  if (s > 0)
    warn(g("{s} points below 0"))
  else
    ok()

  h2("Checking flightline classification...")

  if (!is.null(data$PointSourceID))
  {
    s = fast_countequal(data$PointSourceID, 0L)

    if (s == nrow(data))
      no()
    else if (s > 0 & s < nrow(data))
      maybe()
    else
      yes()
  }
  else
    skip()

  return(invisible())
}

#' @export
lascheck.LAScatalog = function(las)
{
  data <- las@data
  g    <- glue::glue

  if (requireNamespace("crayon", quietly = TRUE))
  {
    green = crayon::green
    red = crayon::red
    orange = crayon::yellow
    silver = crayon::silver
  }
  else
  {
    green <- red <- orange <- silver <- function(x) { return(x) }
  }

  h1    <- function(x) {cat("\n", x)}
  h2    <- function(x) {cat("\n  -", x)}
  ok    <- function()  {cat(green(" ok"))}
  fail  <- function(x) {cat("\n", red(g("   error: {x}")))}
  warn  <- function(x) {cat("\n", orange(g("   warning: {x}")))}
  skip  <- function()  {cat(silver(g(" skipped")))}
  no    <- function()  {cat(red(g(" no")))}
  yes   <- function()  {cat(green(g(" yes")))}
  maybe <- function()  {cat(orange(g(" maybe")))}

  # ==== data =====

  h1("Checking headers consistency")

  h2("Checking file version consistency...")

  s = length(unique(paste0(data$Version.Major, ".", data$Version.Minor)))

  if (s > 1L)
    warn("Inconsistent file versions")
  else
    ok()

  h2("Checking scale consistency...")

  s1 = length(unique(data$X.scale.factor))
  s2 = length(unique(data$Y.scale.factor))
  s3 = length(unique(data$Z.scale.factor))

  if (s1 + s2 + s3 > 3L)
    warn("Inconsistent scale factors")
  else
    ok()

  h2("Checking offset consistency...")

  s1 = length(unique(data$X.offset))
  s2 = length(unique(data$Y.offset))
  s3 = length(unique(data$Z.offset))

  if (s1 + s2 + s3 > 3L)
    warn("Inconsistent offsets")
  else
    ok()

  h2("Checking point type consistency...")

  s = length(unique(data$Point.Data.Format.ID))

  if (s > 1L)
    warn("Inconsistent point formats")
  else
    ok()

  h2("Checking VLR consistency...")

  s = length(unique(data$Number.of.variable.length.record))

  if (s > 1L)
    fail("Inconsistent number of VLR")
  else
    ok()

  h2("Checking CRS consistency...")

  s = length(unique(data$EPSG))

  if (s > 1L)
    fail("Inconsistent EPSG codes")
  else
    ok()

  h1("Checking the headers")

  h2("Checking scale factor validity...")

  failure = FALSE

  s = c(1,10,100,1000,10000)
  valid = c(1/s, 0.5/s, 0.25/s)

  if (any(!data$X.scale.factor %in% valid))
  { fail("Invalid header: X scale factor should be factor ten of 0.1 or 0.5 or 0.25") ; failure = TRUE }

  if (any(!data$Y.scale.factor %in% valid))
  { fail("Invalid header: Y scale factor should be factor ten of 0.1 or 0.5 or 0.25") ; failure = TRUE }

  if (any(!data$Z.scale.factor %in% valid))
  { fail("Invalid header: Z scale factor should be factor ten of 0.1 or 0.5 or 0.25") ; failure = TRUE }

  if (!failure)
    ok()

  h2("Checking Point Data Format ID validity...")

  if (any(data$Point.Data.Format.ID %in% c(4,5,9,10)))
    warn("Invalid headers: point data format not supported yet.")
  else if (any(data$Point.Data.Format.ID < 0 | data$Point.Data.Format.ID > 10))
    fail("Invalid header: point data format invalid.")
  else
    ok()

  h1("Checking preprocessing already done ")

  h2("Checking negative outliers...")

  s = sum(data$Min.Z < 0)

  if (s > 0)
    warn(g("{s} file(s) with points below 0"))
  else
    ok()

  h2("Checking normalization...")

  mean_min = mean(abs(data$Min.Z))

  if (mean_min <= 0.1)
    yes()
  else if (mean_min > 0.1 & mean_min < 2)
    maybe()
  else
    no()

  h1("Checking the geometry")

  h2("Checking overlapping tiles...")

  if (is.overlapping(las))
    warn("Some tiles seem to overlap each other")
  else
    ok()

  h2("Checking point indexation...")

  if (is.indexed(las))
    yes()
  else
    no()
}
