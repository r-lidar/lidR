# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/rlas
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
#
# This file is part of rlas R package.
#
# rlas is free software: you can redistribute it and/or modify
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


#' Compute the hull of each tree.
#'
#' Compute the hull of each segmented tree. The hull can be a convex hull or a concave hull (see details and
#' reference).
#'
#' The concave hull method under the hood is described in Park & Oh (2012). The function relies on
#' the \link[concaveman:concaveman]{concaveman} function which itself is a wrapper around the
#' \href{https://github.com/mapbox/concaveman}{Vladimir Agafonking's implementation}.
#'
#' @param las An object of class \code{LAS}.
#' @param type character. Hull type. Can be 'convex' or 'concave'.
#' @param concavity numeric. If \code{type = "concave"}, a relative measure of concavity. 1 results
#' in a relatively detailed shape, Infinity results in a convex hull.
#' @param length_threshold numeric. If \code{type = "concave"}, when a segment length is under this
#' threshold, it stops being considered for further detalization. Higher values result in simpler shapes.
#' @param field  character. In which field the ID of each tree is stored. In lidR, default is "treeID".
#'
#' @return A \code{SpatialPolygonsDataFrame}. If a trees have less than 4 points it is not considered.
#' @export
#'
#' @references Park, J. S., & Oh, S. J. (2012). A new concave hull algorithm and concaveness measure
#' for n-dimensional datasets. Journal of Information science and engineering, 28(3), 587-600.
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'
#' lastrees_li2(las, speed_up = 7)
#'
#' convex_hulls = tree_hulls(las)
#' sp::plot(convex_hulls)
#'
#' \dontrun{
#' concave_hulls = tree_hulls(las, "concave")
#' sp::plot(concave_hulls)
#' }
tree_hulls = function(las, type = c("convex", "concave"), concavity = 3, length_threshold = 0, field = "treeID")
{
  X <- Y <- tree <- NULL

  type = match.arg(type)

  if (type == "convex")
    dt = tree_metrics(las, stdtreehullconvex(X,Y, .GRP), field)
  else
    dt = tree_metrics(las, stdtreehullconcave(X,Y, .GRP, concavity, length_threshold), field)

  dt = dt[!is.na(tree)]

  spoly = sp::SpatialPolygons(dt$poly)

  for (i in 1:length(spoly)) spoly@polygons[[i]]@ID = as.character(i)

  data = data.frame(dt[, 1])
  spdf = sp::SpatialPolygonsDataFrame(spoly, data)
  sp::proj4string(spdf)<-las@crs

  return(spdf)
}

stdtreehullconvex = function(x,y, grp)
{
  if (length(x) < 4)
    return(NULL)

  i = grDevices::chull(x,y)
  i = c(i, i[1])
  P = cbind(x[i], y[i])
  poly = sp::Polygon(P)
  poly = sp::Polygons(list(poly), ID = grp)

  list(poly = list(poly))
}

stdtreehullconcave = function(x,y, grp, concavity, length_threshold)
{
  if (length(x) < 4)
    return(NULL)

  P = concaveman::concaveman(cbind(x,y), concavity, length_threshold)
  poly = sp::Polygon(P)
  poly = sp::Polygons(list(poly), ID = grp)

  list(poly = list(poly))
}

