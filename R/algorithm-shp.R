# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2019 Jean-Romain Roussel
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

#' Algorithms for shape detection of the local point neighborhood
#'
#' These functions are made to be used in \link{lasdetectshape}. They implement algorithms for local
#' neighborhood shape estimation.
#'
#' In the following, \eqn{a1, a2, a3} denote the eigenvalues of the covariance matrix of the neighbouring
#' points in ascending order. \eqn{th1, th2, th3} denote a set of threshold values. Points are labelled
#' \code{TRUE} if they meet the following criteria. \code{FALSE} otherwise.\cr
#' \describe{
#' \item{shp_plane}{Detection of plans based on criteria defined by Limberger & Oliveira (2015) (see references).
#' A point is labelled TRUE if the neighborhood is approximately planar, that is: \deqn{a2 > (th1*a1) and (th2*a2) > a3}}
#' \item{shp_hplane}{The same as 'plane' but with an extra test on the orientation of the Z vector
#' of the principal components to test the horizontality of the surface.  \deqn{a2 > (th1*a1) and (th2*a2) > a3 and |Z| > th3}
#' In theory |Z| should be exactly equal to 1. In practice 0.98 or 0.99 should be fine}
#' \item{shp_line}{Detection of lines inspired by the Limberger & Oliveira (2015) criterion. A point is
#' labelled TRUE if the neighborhood is approximately linear, that is: \deqn{th1*a2 < a3 and th1*a1 < a3}}
#' }
#'
#' @param th1,th2,th3 numeric. Threshold values (see details)
#' @param k integer. Number of neighbours used to estimate the neighborhood.
#'
#' @references
#' Limberger, F. A., & Oliveira, M. M. (2015). Real-time detection of planar regions in unorganized
#' point clouds. Pattern Recognition, 48(6), 2043–2053. https://doi.org/10.1016/j.patcog.2014.12.020\cr\cr
#'
#' @name shape_detection
#' @rdname shape_detection
NULL

#' @export
#' @rdname shape_detection
shp_plane = function(th1 = 25, th2 = 6, k = 8)
{
  assert_is_a_number(th1)
  assert_is_a_number(th2)
  assert_is_a_number(k)

  th1 <- lazyeval::uq(th1)
  th2 <- lazyeval::uq(th2)
  k   <- lazyeval::uq(k)

  f = function(las, filter)
  {
    assert_is_valid_context(LIDRCONTEXTSHP, "shp_plane")
    return(C_lasdetectshape(las, 1L , c(th1, th2), k, filter, getThread()))
  }

  class(f) <- c(LIDRALGORITHMSHP, LIDRALGORITHMOPENMP)
  return(f)
}

#' @export
#' @rdname shape_detection
shp_hplane = function(th1 = 25, th2 = 6, th3 = 0.98, k = 8)
{
  assert_is_a_number(th1)
  assert_is_a_number(th2)
  assert_is_a_number(th3)
  assert_is_a_number(k)

  th1 <- lazyeval::uq(th1)
  th2 <- lazyeval::uq(th2)
  th3 <- lazyeval::uq(th3)
  k   <- lazyeval::uq(k)

  f = function(las, filter)
  {
    assert_is_valid_context(LIDRCONTEXTSHP, "shp_hplane")
    return(C_lasdetectshape(las, 2L , c(th1, th2, th3), k, filter, getThread()))
  }

  class(f) <- c(LIDRALGORITHMSHP, LIDRALGORITHMOPENMP)
  return(f)
}

#' @export
#' @rdname shape_detection
shp_line = function(th1 = 10, k = 8)
{
  assert_is_a_number(th1)
  assert_is_a_number(k)

  th1 <- lazyeval::uq(th1)
  k   <- lazyeval::uq(k)

  f = function(las, filter)
  {
    assert_is_valid_context(LIDRCONTEXTSHP, "shp_line")
    return(C_lasdetectshape(las, 3L , th1, k, filter, getThread()))
  }

  class(f) <- c(LIDRALGORITHMSHP, LIDRALGORITHMOPENMP)
  return(f)
}
