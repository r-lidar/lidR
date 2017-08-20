# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016 Jean-Romain Roussel
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



#' Read .las or .laz files
#'
#' Read .las or .laz files in format 1 to 3 according to LAS specification and return an
#' object of class LAS. If several files are given the returned LAS object is considered
#' as one LAS file. The information retained in the header will be read from the first file
#' in the list. The optional parameters enable the user to save a lot of memory by choosing
#' to load only fields or points requiered. These internal options are much more memory efficient
#' than any other R code.
#'
#' The 'select' argument specifies which data will actually be loaded. For example, 'xyzia'
#' means that the x, y, and z coordinates, the intensity and the scan angle will be loaded.
#' The supported entries are t - gpstime, a - scan angle, i - intensity, n - number of returns,
#' r - return number, c - classification, u - user data, p - point source ID, e - edge of
#' flight line flag, d - direction of scan flag, R - red channel of RGB color, G - green
#' channel of RGB color, B - blue channel of RGB color and * - is the wildcard and enable
#' to load everythings. \cr
#' x, y, z are implicite and always loaded. 'xyzia' is equivalent to 'ia' and an empty string is
#' equivalent to 'xyz' but \code{select = "xyz"} is more readable and explicite than.
#' \code{select = ""}.\cr Three extra data can be computed on the fly with the folowing flags:
#' P - pulse id, F - flightline id and C - color string (see \link[lidR:LAS-class]{Class LAS})\cr\cr
#' The 'filter' argument enable to filter the point cloud while reading. This is much more
#' efficient than \link{lasfilter} in many ways. If the desired filters are known before to read
#' the file, the internal filters should always be prefered. The avaible filters are those
#' from \code{LASlib} and can be found running the following command: rlas:::lasfilterusage()
#'
#' @param files array of characters or a \link[lidR:catalog]{Catalog} object
#' @param select character. select only columns of interest to save memory (see details)
#' @param filter character. streaming filters - filter data while reading the file (see details)
#' @param ... compatibility with former arguments from lidR (<= 1.2.1)
#'
#' @return A LAS object
#' @export readLAS
#' @seealso
#' \link[lidR:LAS-class]{Class LAS}
#' \link[lidR:catalog]{Catalog}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' las = readLAS(LASfile, select = "xyz")
#' las = readLAS(LASfile, select = "xyzi", filter = "-keep_first")
#' las = readLAS(LASfile, select = "xyziar", filter = "-keep_first -drop_z_below 0")
readLAS = function(files, select = "xyztinrcaRGBP", filter = "", ...)
{
  `%is_in%` <- function(char, str) !is.na(stringr::str_match(str, char)[1,1])

  if (is(files, "Catalog"))
    files <- files$filename

  # ===================================
  # rlas version 1.1.5 has new options
  # which lead to incompatibilities.
  # ===================================

  vrlas = utils::packageVersion("rlas")
  rlas_gt_1.1.4 = vrlas > package_version("1.1.4")

  # ==================
  # Test the files
  # ==================

  valid <- file.exists(files)
  islas <- tools::file_ext(files) %in% c("las", "laz", "LAS", "LAZ")

  if (sum(valid) == 0 | sum(islas) == 0) {
    stop(paste0("File(s) not supported"), call. = FALSE)
  }

  if (sum(!valid) > 0) {
    warning(paste0("File(s) ", p$files, " not found"), call. = FALSE)
    files <- files[valid]
  }

  if (sum(!islas) > 0) {
    warning(paste0("File(s) ", p$files, " not supported"), call. = FALSE)
    files <- files[islas]
  }

  files = normalizePath(files)

  # ==================
  # New syntax parsing
  # ==================

  t <- i <- r <- n <- s <- d <- e <- c <- a <- u <- p <- RGB <- P <- Fl <- C <- FALSE

  if ("\\*" %is_in% select)
    select = "xyztirndecaupRGBPFC"

  if ("i" %is_in% select) i <- TRUE
  if ("t" %is_in% select) t <- TRUE
  if ("r" %is_in% select) r <- TRUE
  if ("n" %is_in% select) n <- TRUE
  if ("d" %is_in% select) d <- TRUE
  if ("e" %is_in% select) e <- TRUE
  if ("c" %is_in% select) c <- TRUE
  if ("a" %is_in% select) a <- TRUE
  if ("u" %is_in% select) u <- TRUE
  if ("p" %is_in% select) p <- TRUE
  if ("R" %is_in% select) RGB <- TRUE
  if ("G" %is_in% select) RGB <- TRUE
  if ("B" %is_in% select) RGB <- TRUE
  if ("P" %is_in% select) P <- TRUE
  if ("F" %is_in% select) Fl <- TRUE
  if ("C" %is_in% select) C <- TRUE

  # ===========================
  # Former syntax compatibility
  # ===========================

  oldparam = list(...)
  oldnames = c("Intensity", "ReturnNumber", "NumberOfReturns", "ScanDirectionFlag", "EdgeOfFlightline", "Classification", "ScanAngle", "UserData", "PointSourceID", "RGB", "pulseID", "flightlineID", "color", "XYZonly")
  pnames   = names(oldparam)

  if (any(pnames %in% oldnames))
  {
    warning("In 'readLAS', arguments used to select columns are deprecated, please use the new argument 'select' instead.", call. = FALSE)

    if (!is.null(oldparam$Intensity))
      i <- oldparam$Intensity

    if (!is.null(oldparam$ReturnNumber))
      r <- oldparam$ReturnNumber

    if (!is.null(oldparam$NumberOfReturns))
      n <- oldparam$NumberOfReturns

    if (!is.null(oldparam$ScanDirectionFlag))
      d <- oldparam$ScanDirectionFlag

    if (!is.null(oldparam$EdgeOfFlightline))
      e <- oldparam$EdgeOfFlightline

    if (!is.null(oldparam$Classification))
      c <- oldparam$Classification

    if (!is.null(oldparam$ScanAngle))
      a <- oldparam$ScanAngle

    if (!is.null(oldparam$UserData))
      u <- oldparam$UserData

    if (!is.null(oldparam$PointSourceID))
      p <- oldparam$PointSourceID

    if (!is.null(oldparam$RGB))
      RGB <- oldparam$RGB

    if (!is.null(oldparam$color))
      C <- oldparam$color

    if (!is.null(oldparam$pulseID))
      P <- oldparam$pulseID

    if (!is.null(oldparam$flightlineID))
      Fl <- oldparam$flightlineID

    if (!is.null(oldparam$XYZonly))
      i <- r <- n <- d <- e <- c <- a <- u <- p <- RGB <- t <- P <- Fl <- C <- FALSE

    if (!is.null(oldparam$all))
      i <- r <- n <- d <- e <- c <- a <- u <- p <- RGB <- t <- P <- Fl <- C <- TRUE
  }

  # ==================
  # Read the files
  # ==================

  data = lapply(files, function(file)
  {
    header = rlas::readlasheader(file)

    if (rlas_gt_1.1.4)
      data = rlas::readlasdata(file, i, r, n, d, e, c, a, u, p, RGB, t, filter)
    else
      data = rlas::readlasdata(file, i, r, n, d, e, c, a, u, p, RGB, filter)

    # Can happend if filter is badly used
    if (dim(data)[1] == 0)
      return(NULL)

    # If filter is used, header will not be in accordance with the data. Hard check is useless
    if (nchar(filter) > 0)
      lascheck(data, header, hard = F)
    else
      lascheck(data, header, hard = T)

    return(data)
  })

  data = data.table::rbindlist(data)

  if (nrow(data) == 0 | ncol(data) == 0)
    return(invisible())

  header <- rlas::readlasheader(files[1])

  las <- LAS(data, header, check = F)

  if (P)
    laspulse(las)

  if (Fl)
    lasflightline(las, 30)

  if (C)
    suppressWarnings(lascolor(las))

  return(las)
}
