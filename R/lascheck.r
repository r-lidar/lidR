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
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' lascheck(las)
#' @export
lascheck = function(las)
{
  UseMethod("lascheck", las)
}

#' @export
lascheck.LAS = function(las)
{
  data <- las@data
  head <- as.list(las@header)
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

  h1    <- function(x)   {cat("\n", x)}
  h2    <- function(x)   {cat("\n  -", x)}
  ok    <- function()    {cat(green(" ok"))}
  skip  <- function()    {cat(silver(g(" skipped")))}
  no    <- function()    {cat(red(g(" no")))}
  yes   <- function()    {cat(green(g(" yes")))}
  maybe <- function()    {cat(orange(g(" maybe")))}

  fail  <- function(msg)
  {
    if (length(msg) == 0)
    {
      ok()
    }
    else
    {
      for (x in msg)
      {
        cat("\n", red(g("   {x}")))
      }
    }
  }

  warn  <- function(msg)
  {
    if (length(msg) == 0)
    {
      ok()
    }
    else
    {
      for (x in msg)
      {
        cat("\n", orange(g("   {x}")))
      }
    }
  }


  # ==== data =====

  h1("Checking the data")

  h2("Checking coordinates...")

  fail(rlas::is_defined_coordinates(data, "vector"))

  h2("Checking coordinates type...")

  fail(rlas::is_valid_XYZ(data, "vector"))

  h2("Checking attributes type...")

  msg = character(0)
  msg = c(msg, rlas::is_valid_gpstime(data, "vector"))
  msg = c(msg, rlas::is_valid_Intensity(data, "vector"))
  msg = c(msg, rlas::is_valid_ReturnNumber(data, head, "vector"))
  msg = c(msg, rlas::is_valid_EdgeOfFlightline(data, "vector"))
  msg = c(msg, rlas::is_valid_Classification(data, head, "vector"))
  msg = c(msg, rlas::is_valid_UserData(data, "vector"))
  msg = c(msg, rlas::is_valid_ScanAngleRank(data, "vector"))
  msg = c(msg, rlas::is_valid_ScanAngle(data, "vector"))
  msg = c(msg, rlas::is_valid_PointSourceID(data, "vector"))
  msg = c(msg, rlas::is_valid_RGB(data, "vector"))
  msg = c(msg, rlas::is_valid_NIR(data, "vector"))
  msg = c(msg, rlas::is_valid_SyntheticFlag(data, "vector"))
  msg = c(msg, rlas::is_valid_KeypointFlag(data, "vector"))
  msg = c(msg, rlas::is_valid_WithheldFlag(data, "vector"))

  fail(msg)


  h2("Checking ReturnNumber validity...")

  warn(rlas::is_compliant_ReturnNumber(data, "vector"))

  h2("Checking NumberOfReturns validity...")

  warn(rlas::is_compliant_NumberOfReturns(data, "vector"))

  h2("Checking ReturnNumber vs. NumberOfReturns...")

  warn(rlas::is_compliant_ReturnNumber_vs_NumberOfReturns(data, "vector"))

  h2("Checking RGB validity...")

  warn(rlas::is_compliant_RGB(data, "vector"))

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
      Classification <- NULL
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

  h2("Checking attribute population...")

  msg = character(0)

  if (!is.null(data[["gpstime"]]))
  {
    s = all(data[["gpstime"]] == 0)

    if (s == nrow(data))
      msg = c(msg, g("'gpstime' attribute is not populated."))
  }

  if (!is.null(data[["PointSourceID"]]))
  {
    s = fast_countequal(data[["PointSourceID"]], 0L)

    if (s == nrow(data))
      msg = c(msg, g("'PointSourceID' attribute is not populated."))
  }

  if (!is.null(data[["ScanDirectionFlag"]]))
  {
    s = fast_countequal(data[["ScanDirectionFlag"]], 0L)

    if (s == nrow(data))
      msg = c(msg, g("'ScanDirectionFlag' attribute is not populated."))
  }

  if (!is.null(data[["EdgeOfFlightline"]]))
  {
    s = fast_countequal(data[["EdgeOfFlightline"]], 0L)

    if (s == nrow(data))
      msg = c(msg, g("'EdgeOfFlightline' attribute is not populated."))
  }

  warn(msg)


  # ==== header ====

  h1("Checking the header")

  h2("Checking header completeness...")

  msg = character(0)
  msg = c(msg, rlas::is_defined_offsets(head, "vector"))
  msg = c(msg, rlas::is_defined_scalefactors(head, "vector"))
  msg = c(msg, rlas::is_defined_version(head, "vector"))
  msg = c(msg, rlas::is_defined_pointformat(head, "vector"))
  msg = c(msg, rlas::is_defined_date(head, "vector"))
  msg = c(msg, rlas::is_defined_globalencoding(head, "vector"))

  fail(msg)

  h2("Checking scale factor validity...")

  fail(rlas::is_valid_scalefactors(head, "vector"))

  h2("Checking Point Data Format ID validity...")

  fail(rlas::is_valid_pointformat(head, "vector"))

  h2("Checking extra bytes attributes validity...")

  fail(rlas::is_valid_extrabytes(head, "vector"))

  h2("Checking coordinate reference sytem...")

  code    = epsg(las)
  swkt    = wkt(las)
  lasproj = las@proj4string
  failure = FALSE

  if (code != 0)
  {
    codeproj = tryCatch(
    {
      proj <- sp::CRS(glue::glue("+init=epsg:{code}"))
      proj <- gsub("\\+init=epsg:\\d+\\s", "", proj@projargs)
      sp::CRS(proj)
    }, error = function(e) return(sp::CRS()))

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
  else if (swkt != "")
  {
    codeproj = tryCatch(sp::CRS(rgdal::showP4(swkt)), error = function(e) return(sp::CRS()))

    if (is.na(codeproj@projargs))
    { fail("WKT OGC CS not understood by rgdal") ; failure = TRUE }

    if (is.na(codeproj@projargs) & !is.na(lasproj@projargs))
    { fail("WKT OGC CS and proj4string do not match.") ; failure = TRUE }

    if (!is.na(codeproj@projargs) & is.na(lasproj@projargs))
    { fail("WKT OGC CS code and proj4string do not match.") ; failure = TRUE }

    if (!is.na(codeproj@projargs) & !is.na(lasproj@projargs))
    {
      if (codeproj@projargs != lasproj@projargs)
      { fail("WKT OGC CS and proj4string do not match.") ; failure = TRUE }
    }

    if (!failure)
      ok()
  }
  else
  {
    if (!is.na(lasproj@projargs))
    { warn("A proj4string found but no CRS in the header") ; failure = TRUE }

    if (!failure)
      ok()
  }

  # ==== data vs header ====

  h1("Checking header vs data adequacy")

  h2("Checking attributes vs. point format...")

  msg = character(0)
  msg = c(msg, rlas::is_NIR_in_valid_format(head, data, "vector"))
  msg = c(msg, rlas::is_gpstime_in_valid_format(head, data, "vector"))
  msg = c(msg, rlas::is_RGB_in_valid_format(head, data, "vector"))

  fail(msg)

  h2("Checking header bbox vs. actual content...")

  msg = character(0)
  msg = c(msg, rlas::is_XY_larger_than_bbox(head, data, "vector"))
  msg = c(msg, rlas::is_XY_smaller_than_bbox(head, data, "vector"))
  msg = c(msg, rlas::is_Z_in_bbox(head, data, "vector"))

  warn(msg)

  h2("Checking header number of points vs. actual content...")

  warn(rlas::is_number_of_points_in_accordance_with_header(head, data, "vector"))

  h2("Checking header return number vs. actual content...")

  warn(rlas::is_number_of_points_by_return_in_accordance_with_header(head, data, "vector"))

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

  return(invisible(las))
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

  return(invisible(las))
}
