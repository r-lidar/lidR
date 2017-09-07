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

round_any <- function(x, accuracy, f = round)
{
  f(x / accuracy) * accuracy
}

make_grid = function(xmin, xmax, ymin, ymax, res, start = c(0,0))
{
  xo = seq(f_grid(xmin, res, start[1]), f_grid(xmax, res, start[1]), res)
  yo = seq(f_grid(ymin, res, start[2]), f_grid(ymax, res, start[2]), res)

  grid = expand.grid(X = xo, Y = yo)
  data.table::setDT(grid)

  return(grid)
}

group_grid = function(x, y, res, start = c(0,0))
{
  xgrid = f_grid(x, res, start[1])
  ygrid = f_grid(y, res, start[2])

  return(list(Xgrid = xgrid, Ygrid = ygrid))
}

group_grid_3d = function(x, y, z, res, start = c(0,0,0))
{
  xgrid = f_grid(x, res, start[1])
  ygrid = f_grid(y, res, start[2])
  zgrid = f_grid(z, res, start[3])

  return(list(Xgrid = xgrid, Ygrid = ygrid, Zgrid = zgrid))
}

f_grid = function(x, res, start)
{
  round_any(x - 0.5 * res - start, res) + 0.5 * res + start
}

catalog_makecluster = function(ctg, res, buffer, by_file, size = CATALOGOPTIONS("tiling_size"))
{
  if (by_file)
  {
    X = ctg[, c("Min X", "Max X", "Min Y", "Max Y")]
    names(X) = c("xleft", "xright", "ybottom", "ytop")
  }
  else
  {
    # dimension of the clusters (width = height)
    width = ceiling(size/res) * res

    verbose("Computing the bounding box of the catalog...")

    # Bounding box of the catalog
    bbox = with(ctg, c(min(`Min X`), min(`Min Y`), max(`Max X`), max(`Max Y`)))

    # Buffer around the bbox as a multiple of the resolution
    buffered_bbox = bbox + c(-res, -res, +res, +res)
    buffered_bbox = round_any(buffered_bbox, res)
    buffered_bbox = buffered_bbox + c(-res, -res, +res, +res)

    verbose("Creating a set of cluster for the catalog...")

    # Generate coordinates of sub bounding boxes
    xleft   = seq(buffered_bbox[1], buffered_bbox[3], width)
    ybottom = seq(buffered_bbox[2], buffered_bbox[4], width)

    X = expand.grid(xleft = xleft, ybottom = ybottom)
    data.table::setDT(X)

    X$xright = X$xleft + width
    X$ytop   = X$ybottom + width
  }

  X$xleftbuff   = X$xleft - buffer
  X$ybottombuff = X$ybottom - buffer
  X$xrightbuff  = X$xright + buffer
  X$ytopbuff    = X$ytop + buffer
  X$xcenter     = (X$xleft + X$xright)/2
  X$ycenter     = (X$ybottom + X$ytop)/2
  X$name        = 1:nrow(X)

  # Remove cluster outside the catalog
  queries = suppressWarnings(catalog_index(ctg, X$xcenter, X$ycenter, width/2, width/2, buffer, X$name))
  X = X[name %in% names(queries)]

  # Plot the pattern
  xrange = c(min(X$xleft), max(X$xright))
  yrange = c(min(X$ybottom), max(X$ytop))
  title  = "Pattern of clusters"
  graphics::plot(ctg, main = title, xlim = xrange, ylim = yrange)
  with(X, graphics::rect(xleft, ybottom, xright, ytop, border = "red"))

  if (buffer > 0)
    with(X, graphics::rect(xleftbuff, ybottombuff, xrightbuff, ytopbuff, border = "darkgreen", lty = "dotted"))

  return(X)
}

verbose = function(...)
{
  if (LIDROPTIONS("verbose"))
    cat(..., "\n")
}

dummy_las = function(n)
{
  dt = data.table::data.table(
    X = stats::runif(n, 0, 100),
    Y = stats::runif(n, 0, 100),
    Z = c(stats::runif(0.8*n, 0, 25), rep(0, 0.2*n)),
    Classification = c(rep(1, 0.8*n), rep(2, 0.2*n)),
    Intensity = stats::rnorm(n, 50, 10),
    ReturnNumber    = rep(c(1,1,1,2,3,1,2,1,2,1), n/10),
    NumberOfReturns = rep(c(1,1,3,3,3,2,2,2,2,1), n/10 ))

  las = suppressWarnings(LAS(dt))

  return(las)
}

`%+%` <- function(a, b) paste0(a, b)
