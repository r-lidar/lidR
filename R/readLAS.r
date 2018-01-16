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
#' Reads .las or .laz files in format 1 to 3 according to LAS specification and returns an
#' object of class LAS. If several files are given the returned LAS object is considered
#' as one LAS file. The information retained in the header will be read from the first file
#' in the list. The optional parameters enable the user to save a substantial amount of memory by
#' choosing to load only the fields or points required. These internal options are much more memory
#' efficient than any other R code.
#'
#' The 'select' argument specifies which data will actually be loaded. For example, 'xyzia'
#' means that the x, y, and z coordinates, the intensity and the scan angle will be loaded.
#' The supported entries are t - gpstime, a - scan angle, i - intensity, n - number of returns,
#' r - return number, c - classification, u - user data, p - point source ID, e - edge of
#' flight line flag, d - direction of scan flag, R - red channel of RGB color, G - green
#' channel of RGB color, B - blue channel of RGB color, * - is the wildcard and enables
#' everything from the LAS file. \cr
#' x, y, z are implicit and always loaded. 'xyzia' is equivalent to 'ia' and an empty string is
#' equivalent to 'xyz' but \code{select = "xyz"} is more readable and explicit than
#' \code{select = ""}.\cr
#'
#' Three extra metrics can be computed on the fly with the following flags:
#' P - pulse id, F - flightline id and C - color string (see \link[lidR:LAS-class]{Class LAS}.
#' The symbol + is a shortcut for 'PFC'.\cr\cr
#'
#' The 'filter' argument allows filtering of the point cloud while reading files. This is much
#' more efficient than \link{lasfilter} in many ways. If the desired filters are known before
#' reading the file, the internal filters should always be preferred. The available filters are
#' those from \code{LASlib} and can be found by running the following command:
#' rlas:::lasfilterusage()
#'
#' The selection of specific Extra Byte fields can be done either with select argument for extra bytes 1-9,
#' or with \code{eb} argument for more specific queries, e.g. \code{eb = c(2, 4, 24)} would load
#' Extra Bytes 2, 4 and 24 if they exist. \code{eb = 0} selects all Extra Bytes available.
#' Argument \code{eb} overrides extra byte arguments in \code{select}.
#'
#' @param files array of characters or a \link[lidR:catalog]{LAScatalog} object
#' @param select character. select only columns of interest to save memory (see details)
#' @param filter character. streaming filters - filter data while reading the file (see details)
#'
#' @return A LAS object
#' @export readLAS
#' @seealso
#' \link[lidR:LAS-class]{Class LAS}
#' \link[lidR:catalog]{LAScatalog}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' las = readLAS(LASfile, select = "xyz")
#' las = readLAS(LASfile, select = "xyzi", filter = "-keep_first")
#' las = readLAS(LASfile, select = "xyziar", filter = "-keep_first -drop_z_below 0")
#' las = readLAS(LASfile, select = "*+")
readLAS = function(files, select = "xyztinrcaRGBP", filter = "")
{
  UseMethod("readLAS", files)
}

#' @export
readLAS.LAScatalog = function(files, select = "xyztinrcaRGBP", filter = "")
{
  return(readLAS(files@data$filename, select, filter))
}

#' @export
readLAS.LAScluster = function(files, select = "xyztinrcaRGBP", filter = "")
{
  buffer <- X <- Y <- NULL

  filter = paste(files@filter, filter)
  las = readLAS(files@files, select, filter)

  if (is.null(las))
    return(invisible())

  if (files@buffer > 0)
  {
    ybottom = files@bbox$ymin
    ytop    = files@bbox$ymax
    xleft   = files@bbox$xmin
    xright  = files@bbox$xmax
    xc      = files@center$x
    yc      = files@center$y
    r       = (files@width - 2*files@buffer)/2

    las@data[, buffer := 0]

    if (files@shape == LIDRCIRCLE)
    {
      las@data[(X-xc)^2 + (Y-yc)^2 > r^2, buffer := LIDRBUFFER]
    }
    else
    {
      las@data[Y < ybottom, buffer := LIDRBOTTOMBUFFER]
      las@data[X < xleft,   buffer := LIDRLEFTBUFFER]
      las@data[Y > ytop,    buffer := LIDRTOPBUFFER]
      las@data[X > xright,  buffer := LIDRRIGHTBUFFER]
      las@data[(X > xright) & (Y < ybottom), buffer := LIDRBOTTOMBUFFER]
    }
  }

  return(las)
}


#' @export
readLAS.character = function(files, select = "xyztinrcaRGBP", filter = "")
{
  ofile = ""
  return(streamLAS(files, ofile, select, filter))
}

streamLAS = function(x, ofile, select = "*", filter = "")
{
  UseMethod("streamLAS", x)
}

streamLAS.LAScluster = function(x, ofile, select = "*", filter = "")
{
  filter = paste(x@filter, filter)
  las = streamLAS(x@files, ofile, select,  filter)
  return(invisible(las))
}

streamLAS.character = function(x, ofile, select = "*", filter = "")
{
  # ==================
  # Test the files
  # ==================

  valid <- file.exists(x)
  islas <- tools::file_ext(x) %in% c("las", "laz", "LAS", "LAZ")

  if (sum(valid) == 0 | sum(islas) == 0) {
    stop(paste0("File(s) not supported"), call. = FALSE)
  }

  if (sum(!valid) > 0) {
    warning(paste0("File(s) ", x[!valid], " not found"), call. = FALSE)
    x <- x[valid]
  }

  if (sum(!islas) > 0) {
    warning(paste0("File(s) ", x[!islas], " not supported"), call. = FALSE)
    x <- x[islas]
  }

  ifiles = normalizePath(x)

  # ==================
  # New syntax parsing
  # ==================

  t <- i <- r <- n <- s <- d <- e <- c <- a <- u <- p <- RGB <- P <- Fl <- C <- FALSE

  options = select

  if ("\\*" %is_in% select)
    options = "xyztirndecaupRGB0"

  if ("\\+" %is_in% select)
    options = paste0(options, "PFC")

  if ("i" %is_in% options) i <- TRUE
  if ("t" %is_in% options) t <- TRUE
  if ("r" %is_in% options) r <- TRUE
  if ("n" %is_in% options) n <- TRUE
  if ("d" %is_in% options) d <- TRUE
  if ("e" %is_in% options) e <- TRUE
  if ("c" %is_in% options) c <- TRUE
  if ("a" %is_in% options) a <- TRUE
  if ("u" %is_in% options) u <- TRUE
  if ("p" %is_in% options) p <- TRUE
  if ("R" %is_in% options) RGB <- TRUE
  if ("G" %is_in% options) RGB <- TRUE
  if ("B" %is_in% options) RGB <- TRUE
  eb <- as.numeric(unlist(regmatches(options, gregexpr("[[:digit:]]", options))))
  if ("P" %is_in% options) P <- TRUE
  if ("F" %is_in% options) Fl <- TRUE
  if ("C" %is_in% options) C <- TRUE

  if ((Fl | P) & !t) {
    t <- TRUE
    message("'t' has automatically been added in the selection to match other options")
  }

  # ==================
  # Read the files
  # ==================

  header = rlas::readlasheader(ifiles[1])
  data   = rlas:::streamlasdata(ifiles, ofile, filter, i, r, n, d, e, c, a, u, p, RGB, t, eb)

  if (is.null(data))
    return(invisible())

  if (nrow(data) == 0 | ncol(data) == 0)
    return(invisible())

  # If filter is used, header will not be in accordance with the data. Hard check is useless
  if (nchar(filter) > 0 | length(ifiles) > 1)
    lascheck(data, header, hard = F)
  else
    lascheck(data, header, hard = T)

  las <- LAS(data, header, check = F)

  if (P)
    laspulse(las)

  if (Fl)
    lasflightline(las, 30)

  if (C)
    suppressWarnings(lascolor(las))

  return(las)
}

`%is_in%` <- function(char, str) !is.na(stringr::str_match(str, char)[1,1])
