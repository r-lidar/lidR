# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
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
#' Reads .las or .laz files in format 1 to 3 according to LAS specifications and returns an
#' object of class \link[lidR:LAS-class]{LAS}. If several files are read at once the returned LAS object
#' is considered as one LAS file. The optional parameters enable the user to save a substantial amount
#' of memory by choosing to load only the attributes or points of interest. The LAS format 1.4 is
#' currently only partially supported.
#'
#' \strong{Select:} the 'select' argument specifies which attribute will actually be loaded. For example,
#' 'xyzia' means that the x, y, and z coordinates, the intensity and the scan angle will be loaded.
#' The supported entries are t - gpstime, a - scan angle, i - intensity, n - number of returns,
#' r - return number, c - classification, u - user data, p - point source ID, e - edge of
#' flight line flag, d - direction of scan flag, R - red channel of RGB color, G - green
#' channel of RGB color, B - blue channel of RGB color, N - near infrared channel. Also numbers from
#' 1 to 9 are available for the extra bytes data 1 to 9. 0 enables loading of all extra bytes and '*'
#' is the wildcard and enables everything to be loaded from the LAS file. Note that x, y, z are implicit
#' and always loaded, thus 'xyzia' is equivalent to 'ia'.\cr\cr
#' \strong{Filter:} the 'filter' argument allows filtering of the point cloud while reading files.
#' This is much more efficient than \link{lasfilter} in many ways. If the desired filters are known
#' before reading the file, the internal filters should always be preferred. The available filters are
#' those from \code{LASlib} and can be found by running the following command: rlas:::lasfilterusage().
#' (see also \link[rlas:read.las]{rlas::read.las})
#'
#' @param files characters. Path(s) to one or several a file(s). Can also be a
#' \link[lidR:LAScatalog-class]{LAScatalog} object.
#' @param select character. Read only attributes of interest to save memory (see details).
#' @param filter character. Read only points of interest to save memory (see details).
#'
#'
#' @return A LAS object
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' las = readLAS(LASfile, select = "xyz")
#' las = readLAS(LASfile, select = "xyzi", filter = "-keep_first")
#' las = readLAS(LASfile, select = "xyziar", filter = "-keep_first -drop_z_below 0")
#'
#' # Negation of attribute is also possible (all except intensity and angle)
#' las = readLAS(LASfile, select = "* -i -a")
readLAS = function(files, select = "*", filter = "")
{
  UseMethod("readLAS", files)
}

#' @export
readLAS.LAScatalog = function(files, select = "*", filter = "")
{
  assert_is_a_string(select)
  assert_is_a_string(filter)
  return(readLAS(files@data$filename, select, filter))
}

#' @export
readLAS.LAScluster = function(files, select = "*", filter = "")
{
  assert_is_a_string(select)
  assert_is_a_string(filter)

  buffer <- X <- Y <- NULL

  las <- readLAS(files@files, files@select,files@filter)
  las@proj4string <- files@proj4string

  las@data[, buffer := LIDRNOBUFFER]

  if (files@buffer > 0)
  {
    ext     <- raster::extent(files)
    ybottom <- ext@ymin
    ytop    <- ext@ymax
    xleft   <- ext@xmin
    xright  <- ext@xmax
    xc      <- files@center$x
    yc      <- files@center$y
    r       <- (files@width - 2*files@buffer)/2

    if (files@shape == LIDRCIRCLE)
    {
      las@data[(X-xc)^2 + (Y-yc)^2 > r^2, buffer := LIDRBUFFER]
    }
    else
    {
      las@data[Y < ybottom, buffer := LIDRBOTTOMBUFFER]
      las@data[X < xleft,   buffer := LIDRLEFTBUFFER]
      las@data[Y >= ytop,    buffer := LIDRTOPBUFFER]
      las@data[X >= xright,  buffer := LIDRRIGHTBUFFER]
      las@data[(X >= xright) & (Y < ybottom), buffer := LIDRBOTTOMBUFFER]
    }

    # We found a region with no actual data. The points all belong into the buffer
    # Return empty point cloud
    if (fast_countequal(las@data[["buffer"]], LIDRNOBUFFER) == 0)
      las <- LAS(data.frame(X = numeric(0), Y = numeric(0), Z = numeric(0)))
  }

  return(las)
}

#' @export
readLAS.character = function(files, select = "*", filter = "")
{
  assert_is_a_string(select)
  assert_is_a_string(filter)
  return(streamLAS(files, ofile = "", select, filter))
}

streamLAS = function(x, ofile, select = "*", filter = "", filter_wkt = "")
{
  UseMethod("streamLAS", x)
}

streamLAS.LAScluster = function(x, ofile, select = "*", filter = "", filter_wkt = "")
{
  las = streamLAS(x@files, ofile, x@select, x@filter, filter_wkt)

  # Automatically remove files that are empty
  if (ofile != "")
  {
    header = rlas::read.lasheader(ofile)

    if (header$`Number of point records` == 0)
    {
      file.remove(ofile)
      return(NULL)
    }
  }

  return(las)
}

streamLAS.character = function(x, ofile, select = "*", filter = "", filter_wkt = "")
{
  assert_all_are_existing_files(x)

  islas <- tools::file_ext(x) %in% c("las", "laz", "LAS", "LAZ")

  if (any(!islas)) stop("File(s) are not las or laz")

  ifiles <- normalizePath(x)

  if (ofile != "")
  {
    dir = dirname(ofile)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  }

  header <- rlas::read.lasheader(ifiles[1])
  data   <- rlas:::stream.las(ifiles, ofile, select, filter, filter_wkt)

  if (is.null(data))
    return(invisible())

  if (nrow(data) > 0)
  {
    rlas::check_header(header)
    rlas::check_data(data)

    # If filter is used, header will not be in accordance with the data. Hard check will necessarily return a false positive error
    hard <- if (nchar(filter) > 0 | length(ifiles) > 1) FALSE else TRUE

    # If the number of file read is > 1 header bbox will not be in accordance with the data. Update the header.
    if (length(ifiles) > 1)
    {
      header$`Min X` = min(data$X)
      header$`Max X` = max(data$X)
      header$`Min Y` = min(data$Y)
      header$`Max Y` = max(data$Y)
      header$`Min Z` = min(data$Z)
      header$`Max Z` = max(data$Z)
    }

    rlas::check_data_vs_header(header, data, hard = hard)
  }

  return(LAS(data, header, check = FALSE))
}
