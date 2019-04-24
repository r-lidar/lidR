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

round_any <- function(x, accuracy)
{
  roundc(x / accuracy) * accuracy
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
  xgrid = f_grid(x, res[1], start[1])
  ygrid = f_grid(y, res[1], start[2])
  zgrid = f_grid(z, res[2], start[3])

  return(list(Xgrid = xgrid, Ygrid = ygrid, Zgrid = zgrid))
}

f_grid = function(x, res, start)
{
  round_any(x - 0.5 * res - start, res) + 0.5 * res + start
}

verbose = function(...)
{
  if (getOption("lidR.verbose") || getOption("lidR.debug"))
    cat(..., "\n")
}

dummy_las = function(n, seeds = 1)
{
  set.seed(seeds)
  X = stats::runif(n, 0, 100)
  set.seed(seeds + 1)
  Y = stats::runif(n, 0, 100)
  set.seed(seeds + 2)
  Z = c(stats::runif(0.8*n, 0, 25), rep(0, 0.2*n))
  Classification = as.integer(c(rep(1, 0.8*n), rep(2, 0.2*n)))
  Intensity = sample(10:50, n, TRUE)
  ReturnNumber    = as.integer(rep(c(1,1,1,2,3,1,2,1,2,1), n/10))
  NumberOfReturns = as.integer(rep(c(1,1,3,3,3,2,2,2,2,1), n/10 ))

  dt = data.table::data.table(X, Y, Z, Classification, Intensity, ReturnNumber, NumberOfReturns)
  las = suppressWarnings(LAS(dt, check = FALSE))

  return(las)
}

subcircled = function(dt, r, n)
{
  X <- Y <- Z <- NULL

  f = function(x, y, z, px, py)
  {
    x = x + px
    y = y + py
    z = rep(z, length(px))

    list(X = x, Y = y, Z = z)
  }

  n = n + 1

  alpha = seq(0, 2*pi, length.out = n)[-n]
  px = r*cos(alpha)
  py = r*sin(alpha)

  return(dt[, f(X, Y, Z, px, py), by = rownames(dt)][, rownames := NULL][])
}

coordinates = function(las)
{
  DT <- las@data
  X  <- DT[["X"]]
  Y  <- DT[["Y"]]
  DF <- data.frame(X,Y)
  data.table::setDT(DF)
  return(DF)
}

coordinates3D = function(las)
{
  DT <- las@data
  X  <- DT[["X"]]
  Y  <- DT[["Y"]]
  Z  <- DT[["Z"]]
  DF <- data.frame(X,Y,Z)
  data.table::setDT(DF)
  return(DF)
}

