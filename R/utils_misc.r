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

make_overlay_raster = function(las, res, start = c(0,0), subcircle = 0)
{
  if (is(res, "RasterLayer"))
  {
    resolution = raster::res(res)
    if (resolution[1] !=  resolution[2]) stop("Rasters with different x y resolutions are not supported", call. = FALSE)
    return(res)
  }

  bbox      <- raster::extent(las) + 2 * subcircle
  bbox@xmin <- round_any(bbox@xmin - 0.5 * res - start[1], res) + start[1]
  bbox@xmax <- round_any(bbox@xmax - 0.5 * res - start[1], res) + res + start[1]
  bbox@ymin <- round_any(bbox@ymin - 0.5 * res - start[2], res) + start[2]
  bbox@ymax <- round_any(bbox@ymax - 0.5 * res - start[2], res) + res + start[2]
  layout    <- suppressWarnings(raster::raster(bbox, res = res, crs = las@proj4string))
  return(layout)
}

merge_rasters = function(output)
{
  # Outputs have been return in R objects. Merge the outptus in a single object
  names         <- names(output[[1]])
  factor        <- output[[1]]@data@isfactor
  output        <- do.call(raster::merge, output)
  names(output) <- names
  if (is(output, "RasterBrick")) colnames(output@data@values) <- names
  return(output)
}

build_vrt = function(output, vrt)
{
  if (!requireNamespace("gdalUtils", quietly = TRUE))
  {
    message("'gdalUtils' package is needed to build a virtual raster mosaic. Return the list of written files instead.", call. = F)
    return(unlist(output))
  }

  output <- unlist(output)
  folder <- dirname(output[1])
  file   <- paste0("/", vrt, ".vrt")
  vrt    <- paste0(folder, file)
  gdalUtils::gdalbuildvrt(output, vrt)
  return(raster::stack(vrt))
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

verbose = function(...)
{
  if (getOption("lidR.verbose"))
    cat(..., "\n")
}

dummy_las = function(n, seeds = c)
{
  set.seed(1)
  X = stats::runif(n, 0, 100)
  set.seed(2)
  Y = stats::runif(n, 0, 100)
  set.seed(3)
  Z = c(stats::runif(0.8*n, 0, 25), rep(0, 0.2*n))
  Classification = as.integer(c(rep(1, 0.8*n), rep(2, 0.2*n)))
  Intensity = sample(10:50, n, TRUE)
  ReturnNumber    = as.integer(rep(c(1,1,1,2,3,1,2,1,2,1), n/10))
  NumberOfReturns = as.integer(rep(c(1,1,3,3,3,2,2,2,2,1), n/10 ))

  dt = data.table::data.table(X, Y, Z, Classification, Intensity, ReturnNumber, NumberOfReturns)
  las = suppressWarnings(LAS(dt))

  return(las)
}

stopifnotlas = function(x)
{
  if (!inherits(x, "LAS"))
    stop("Argument is not a LAS object", call. = F)
}

stopif_forbidden_name = function(name)
{
  if (name %in% LASFIELDS)
    stop(glue::glue("{name} is a forbidden name."), call. = FALSE)
}

stopif_wrong_context = function(received_context, expected_contexts, func_name)
{
  str = paste0(expected_contexts, collapse  = "' or '")

  if (is.null(received_context))
    stop(glue::glue("The '{func_name}' function has not been called within a correct context. Maybe it has been called alone but it should be used within a lidR function."), call. = FALSE)
  if (!received_context %in% expected_contexts)
    stop(glue::glue("The '{func_name}' function has not been called within a correct context. It is expected to be used in '{str}'"), call. = FALSE)
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

#' Parameters for progressive morphological filter
#'
#' The function \link{lasground} with the progressive morphological filter allows for any
#' sequence of parameters. This function enables computation of the sequences using equations (4),
#'  (5) and (7) from Zhang et al. (see reference and details).
#' @details
#' In the original paper the windows size sequence is given by eq. 4 or 5:\cr\cr
#'
#' \eqn{w_k = 2kb + 1} \cr\cr
#' or\cr\cr
#' \eqn{w_k = 2b^k + 1}\cr\cr
#'
#' In the original paper the threshold sequence is given by eq. 7:\cr\cr
#' \eqn{th_k = s*(w_k - w_{k-1})*c + th_0}\cr\cr
#' Because the function \link{lasground} applies the morphological operation at the point
#' cloud level the parameter \eqn{c} is set to 1 and cannot be modified.
#' @param b numeric. This is the parameter \eqn{b} in Zhang et al. (2003) (eq. 4 and 5).
#' @param max_ws numeric. Maximum window size to be used in filtering ground returns. This limits
#' the number of windows created.
#' @param dh0 numeric. This is \eqn{dh_0} in Zhang et al. (2003) (eq. 7).
#' @param dhmax numeric. This is \eqn{dh_{max}} in Zhang et al. (2003) (eq. 7).
#' @param s numeric. This is \eqn{s} in Zhang et al. (2003) (eq. 7).
#' @param exp logical. The window size can be increased linearly or exponentially (eq. 4 or 5).
#' @return A list with two components: the windows size sequence and the threshold sequence.
#' @references
#' Zhang, K., Chen, S. C., Whitman, D., Shyu, M. L., Yan, J., & Zhang, C. (2003). A progressive
#' morphological filter for removing nonground measurements from airborne LIDAR data. IEEE
#' Transactions on Geoscience and Remote Sensing, 41(4 PART I), 872–882. http:#doi.org/10.1109/TGRS.2003.810682.
#' @export
util_makeZhangParam = function(b = 2, dh0 = 0.5, dhmax = 3.0, s = 1.0,  max_ws = 20, exp = FALSE)
{
  if (exp & b <= 1)
    stop("b cannot be lower than 1 with an exponentially growing windows", call. = FALSE)

  if (dh0 >= dhmax)
    stop("dh0 greater than dhmax", call. = FALSE)

  if (max_ws < 3)
    stop("Minimum windows size is 3. max_ws cannot must be greater than 3", call. = FALSE)

  if (!is.logical(exp))
    stop("exp should be logical", call. = FALSE)

  if (!exp & b < 1)
    warning("Due to an incoherence in the original paper when b < 1 the sequences of windows size cannot be computed for a linear increase. The internal routine uses the fact that the increment is constant to bypass this issue.", call. = FALSE)


  dhtk = c()
  wk = c()
  k = 0
  ws = 0
  th = 0
  c = 1

  while (ws <= max_ws)
  {
    # Determine the initial window size.
    if (exp)
      ws = (2.0*b^k) + 1
    else
      ws = 2.0*(k + 1)*b + 1

    # Calculate the height threshold to be used in the next k.
    if (ws <= 3)
      th = dh0
    else
    {
      if(exp)
        th = s * (ws - wk[k]) * c + dh0
      else
        th = s*2*b*c+dh0
    }

    # Enforce max distance on height threshold
    if (th > dhmax)
      th = dhmax

    if (ws <= max_ws)
    {
      wk = append(wk, ws)
      dhtk = append(dhtk, th)
    }

    k = k + 1
  }

  return(list(ws = wk, th = dhtk))
}

`%+%` <- function(a, b) paste0(a, b)
