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

dummy_las <- function(n, seeds = 1) {
  set.seed(seeds)
  X <- round_any(stats::runif(n, 0, 100), 0.001)
  set.seed(seeds + 1)
  Y <- round_any(stats::runif(n, 0, 100), 0.001)
  set.seed(seeds + 2)
  Z <- round_any(c(stats::runif(0.8 * n, 0, 25), rep(0, 0.2 * n)), 0.001)
  Classification <- as.integer(c(rep(1, 0.8 * n), rep(2, 0.2 * n)))
  set.seed(seeds + 3)
  Intensity <- as.integer(stats::runif(n, 10, 50))
  ReturnNumber <- as.integer(rep(c(1, 1, 1, 2, 3, 1, 2, 1, 2, 1), n / 10))
  NumberOfReturns <- as.integer(rep(c(1, 1, 3, 3, 3, 2, 2, 2, 2, 1), n / 10))

  data <- data.table::data.table(X, Y, Z, Classification, Intensity, ReturnNumber, NumberOfReturns)
  header <- rlas::header_create(data)
  header[["X scale factor"]] <- 0.001
  header[["Y scale factor"]] <- 0.001
  header[["Z scale factor"]] <- 0.001
  header[["X offset"]] <- 0
  header[["Y offset"]] <- 0
  header[["Z offset"]] <- 0
  las <- suppressMessages(suppressWarnings(LAS(data, header, check = FALSE)))
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

  return(dt[, f(X, Y, Z, px, py), by = 1:nrow(dt)][, nrow := NULL][])
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

