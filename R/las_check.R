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
#' @param print logical. By default, prints a report into standard ouptut. If \code{print = FALSE}
#' the functions returns a \code{list} with two elements named 'warnings' and 'errors' containing a
#' vector with the reported warnings and errors.
#' @param ... Use \code{deep = TRUE} on a LAScatalog only. Instead of a shallow inspection it reads
#' all the files and performs a deep inspection.
#'
#' @return A list with two elements named \code{warnings} and \code{errors}. This list is returned
#' invisibly if \code{print = TRUE}. If \code{deep = TRUE} a nested list is returned with one element
#' per file.
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile)
#' las_check(las)
#' @export
#' @family las utilities
las_check = function(las, print = TRUE, ...)
{
  UseMethod("las_check", las)
}

#' @export
las_check.LAS = function(las, print = TRUE, ...)
{
  assert_is_a_bool(print)

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
    green <- red <- orange <- silver <- function(x) { return(x) } # nocov
  }

  h1    <- function(x)   {if (print) cat("\n", x)}
  h2    <- function(x)   {if (print) cat("\n  -", x)}
  ok    <- function()    {if (print) cat(green(" \u2713"))}
  skip  <- function()    {if (print) cat(silver(g(" skipped")))}
  no    <- function()    {if (print) cat(red(g(" no")))}
  yes   <- function()    {if (print) cat(green(g(" yes")))}
  maybe <- function()    {if (print) cat(orange(g(" maybe")))}

  warnings <- character(0)
  errors <- character(0)

  fail  <- function(msg) {
    if (print) {
      if (length(msg) == 0) {
        ok()
      } else {
        for (x in msg) cat("\n", red(g("   \u2717 {x}")))
      }
    }

    if (length(msg) > 0) { for (x in msg) errors <<- append(errors, x) }
  }

  warn  <- function(msg) {
    if (print) {
      if (length(msg) == 0) {
        ok()
      } else {
        for (x in msg) cat("\n", orange(g("  \u26A0 {x}")))
      }
    }

    if (length(msg) > 0) { for (x in msg) warnings <<- append(warnings, x) }
  }

  xscale <- las@header@PHB$`X scale factor`
  xoffset <- las@header@PHB$`X offset`
  yscale <- las@header@PHB$`Y scale factor`
  yoffset <- las@header@PHB$`Y offset`
  zscale <- las@header@PHB$`Z scale factor`
  zoffset <- las@header@PHB$`Z offset`

  # ==== data =====

  h1("Checking the data")

  h2("Checking coordinates...")

  fail(rlas::is_defined_coordinates(data, "vector"))

  h2("Checking coordinates type...")

  fail(rlas::is_valid_XYZ(data, "vector"))

  h2("Checking coordinates range...")

  xvalid_range <- storable_coordinate_range(xscale, xoffset)
  yvalid_range <- storable_coordinate_range(yscale, yoffset)
  zvalid_range <- storable_coordinate_range(zscale, zoffset)
  xrange <- range(data$X)
  yrange <- range(data$Y)
  zrange <- range(data$Z)

  failure = FALSE

  if (xrange[1] < xvalid_range[1] | xrange[2] > xvalid_range[2])
  {
    fail(glue::glue("X coordinates range in [{xrange[1]}, {xrange[2]}] but storable range is  [{xvalid_range[1]}, {xvalid_range[2]}]"))
    failure = TRUE
  }

  if (yrange[1] < yvalid_range[1] | yrange[2] > yvalid_range[2])
  {
    fail(glue::glue("Y coordinates range in [{yrange[1]}, {yrange[2]}] but storable range is  [{yvalid_range[1]}, {yvalid_range[2]}]"))
    failure = TRUE
  }

  if (zrange[1] < zvalid_range[1] | zrange[2] > zvalid_range[2])
  {
    fail(glue::glue("Z coordinates range in [{zrange[1]}, {zrange[2]}] but storable range is  [{zvalid_range[1]}, {zvalid_range[2]}]"))
    failure = TRUE
  }

  if (failure == FALSE)
    ok()

  h2("Checking coordinates quantization...")

  i <- fast_countunquantized(las$X, xscale, xoffset)
  j <- fast_countunquantized(las$Y, yscale, yoffset)
  k <- fast_countunquantized(las$Z, zscale, zoffset)

  if (i + j + k > 0)
  {
    if (i > 0) fail(glue::glue("{i} X coordinates were not stored with a resolution compatible with scale factor {xscale} and offset {xoffset}"))
    if (j > 0) fail(glue::glue("{j} Y coordinates were not stored with a resolution compatible with scale factor {yscale} and offset {yoffset}"))
    if (k > 0) fail(glue::glue("{k} Z coordinates were not stored with a resolution compatible with scale factor {zscale} and offset {zoffset}"))
  }
  else
  {
    ok()
  }

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
  whichnas = names(nas)
  whichnas = paste(whichnas, collapse = ", ")

  if (length(nas) > 0)
  {
    string = paste("The following attributes contain NAs:", whichnas)
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

    if (s)
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

  h2("Checking gpstime incoherances")

  if (!is.null(data[["gpstime"]]) && !is.null(data[["ReturnNumber"]]))
  {
    s1 <- C_check_gpstime(data[["gpstime"]], data[["ReturnNumber"]])
    if (s1 > 0)
      fail(g("{s1} pulses (points with the same gpstime) have points with identical ReturnNumber"))
    else
      ok()
  }
  else
    skip()


  h2("Checking flag attributes...")

  msg = character(0)

  if (!is.null(data[["Withheld_flag"]]))
  {
    s = sum(data[["Withheld_flag"]])

    if (s > 0)
      msg = c(msg, g("{s} points flagged 'withheld'."))
  }

  if (!is.null(data[["Synthetic_flag"]]))
  {
    s = sum(data[["Synthetic_flag"]])

    if (s > 0)
      msg = c(msg, g("{s} points flagged 'synthetic'."))
  }

  if (!is.null(data[["Keypoint_flag"]]))
  {
    s = sum(data[["Keypoint_flag"]])

    if (s > 0)
      msg = c(msg, g("{s} points flagged 'keypoint'."))
  }

  warn(msg)

  h2("Checking user data attribute...")

  msg = character(0)

  if (!is.null(data[["UserData"]]))
  {
    s = sum(data[["UserData"]] != 0)

    if (s > 0)
      warn(g("{s} points have a non 0 UserData attribute. This probably has a meaning."))
    else
      ok()
  }
  else
  {
    skip()
  }

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

  h2("Checking point data format ID validity...")

  fail(rlas::is_valid_pointformat(head, "vector"))

  h2("Checking extra bytes attributes validity...")

  fail(rlas::is_valid_extrabytes(head, "vector"))

  h2("Checking the bounding box validity...")

  bbx = c(head[["Min X"]], head[["Max X"]])
  bby = c(head[["Min Y"]], head[["Max Y"]])
  bbz = c(head[["Min Z"]], head[["Max Z"]])

  i <- fast_countunquantized(bbx, xscale, xoffset)
  j <- fast_countunquantized(bby, yscale, yoffset)
  k <- fast_countunquantized(bbz, zscale, zoffset)

  if (i + j + k > 0)
  {
    if (i > 0) fail("Stored resolution of 'Min X' and/or 'Max X' not compatible with 'X offset' and 'X scale factor'")
    if (j > 0) fail("Stored resolution of 'Min Y' and/or 'Max Y' not compatible with 'Y offset' and 'Y scale factor'")
    if (k > 0) fail("Stored resolution of 'Min Z' and/or 'Max Z' not compatible with 'Z offset' and 'Z scale factor'")
  }
  else
  {
    ok()
  }

  h2("Checking coordinate reference sytem...")

  code    <- if (use_epsg(las)) epsg(las) else 0
  swkt    <- wkt(las)
  lasproj <- las@proj4string
  failure <- FALSE

  if (use_epsg(las) && code != 0)
  {
    codeproj <- epsg2CRS(code)

    if (is.na(codeproj@projargs))
    { fail(glue::glue("EPSG code {code} unknown.")) ; failure = TRUE }

    if (is.na(codeproj@projargs) && !is.na(lasproj@projargs))
    { warn(glue::glue("EPSG code is unknown but a proj4string found.")) ; failure = TRUE }

    if (!is.na(codeproj@projargs) && is.na(lasproj@projargs))
    { warn("ESPG code is valid but no proj4string found.") ; failure = TRUE }

    if (!is.na(codeproj@projargs) && !is.na(lasproj@projargs))
    {
      if (codeproj@projargs != lasproj@projargs)
      { fail("ESPG code and proj4string do not match.") ; failure = TRUE }
    }

    if (!failure)
      ok()
  }

  if (use_wktcs(las) && swkt != "")
  {
    codeproj = wkt2CRS(swkt)

    if (is.na(codeproj@projargs))
    { fail("WKT OGC CS not understood by rgdal") ; failure = TRUE }

    if (is.na(codeproj@projargs) & !is.na(lasproj@projargs))
    { warn("WKT OGC CS not understood by rgdal but a proj4string found.") ; failure = TRUE }

    if (!is.na(codeproj@projargs) & is.na(lasproj@projargs))
    { warn("WKT OGC CS is valid but no proj4string found.") ; failure = TRUE }

    if (!is.na(codeproj@projargs) & !is.na(lasproj@projargs))
    {
      if (codeproj@projargs != lasproj@projargs)
      { fail("WKT OGC CS and proj4string do not match.") ; failure = TRUE }
    }

    if (!failure)
      ok()
  }

  if (use_epsg(las) && swkt != "")
  { fail("Global encoding WKT bits set to 0 but a WKT string found in the header.") ; failure = TRUE }

  if (use_wktcs(las) && code != 0)
  { fail("Global encoding WKT bits set to 1 but an epsg code found in the header.") ; failure = TRUE }

  if (code == 0 & swkt == "")
  {
    if (!is.na(lasproj@projargs))
    { warn("A proj4string found but no CRS in the header.") ; failure = TRUE }

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

  if (any(c("X", "Y", "Z") %in% whichnas)) {
    skip()
  }
  else {
    msg = character(0)
    msg = c(msg, rlas::is_XY_larger_than_bbox(head, data, "vector"))
    msg = c(msg, rlas::is_XY_smaller_than_bbox(head, data, "vector"))
    msg = c(msg, rlas::is_Z_in_bbox(head, data, "vector"))
    warn(msg)
  }

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
    skip()

  h2("Checking normalization...")

  if (any(c("X", "Y", "Z") %in% whichnas)) {
    skip()
  }
  else
  {
    min = grid_metrics(las, ~min(Z), res = 20)
    mean_min = mean(abs(min[]), na.rm = TRUE)

    if (mean_min <= 0.1)
      yes()
    else if (mean_min > 0.1 & mean_min < 1)
      maybe()
    else
      no()
  }

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

  warnerr = list(
    warnings = warnings,
    errors = errors)

  if (print)
    return(invisible(warnerr))
  else
    return(warnerr)
}

las_check.LAScluster = function(las, print = TRUE, ...)
{
  f <- basename(las@files)
  if (length(f) > 1) stop("Internal error: several files in the LAScluster. Please report this issue")

  if (print)
  {
    title <- glue::glue("Checking the file: {f}")
    n = nchar(title)
    header <- strrep("_", n)
    cat("\n\n")
    cat(header)
    cat("\n")
    cat(title)
    cat("\n")
    cat(header)
    cat("\n")
  }

  x <- readLAS(las)
  return(las_check(x, print))
}

#' @export
las_check.LAScatalog = function(las, print = TRUE, deep = FALSE, ...)
{
  assert_is_a_bool(print)
  assert_is_a_bool(deep)

  # Deep inspection of each file
  if (deep)
  {
    opt_chunk_size(las) <- 0
    opt_chunk_buffer(las) <- 0
    opt_select(las) <- "*"
    opt_filter(las) <- ""
    opt_wall_to_wall(las) <- FALSE
    if (print) opt_progress(las) <-  FALSE
    out <- catalog_apply(las, las_check, print = print)
    names(out) <- basename(las@data[["filename"]])

    if (print)
      return(invisible(out))
    else
      return(out)
  }


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
    green <- red <- orange <- silver <- function(x) { return(x) } # nocov
  }

  warnings <- character(0)
  errors <- character(0)

  h1    <- function(x) {if (print) cat("\n", x)}
  h2    <- function(x) {if (print) cat("\n  -", x)}
  ok    <- function()  {if (print) cat(green(" \u2713"))}
  fail  <- function(x) {if (print) { cat("\n", red(g("   \u2717 {x}"))) } ; errors <<- append(errors, x)}
  warn  <- function(x) {if (print) { cat("\n", orange(g("   \u26A0 {x}"))) } ; warnings <<- append(warnings, x)}
  #skip  <- function()  {cat(silver(g(" skipped")))}
  no    <- function()  {if (print) cat(red(g(" no")))}
  yes   <- function()  {if (print) cat(green(g(" yes")))}
  maybe <- function()  {if (print) cat(orange(g(" maybe")))}

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

  s = length(unique(data[["CRS"]]))

  if (s > 1L)
    fail("Inconsistent CRS accross files")
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

  warnerr = list(
    warnings = warnings,
    errors = errors)

  if (print)
    return(invisible(warnerr))
  else
    return(warnerr)
}
