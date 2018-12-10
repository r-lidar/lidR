# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https:#github.com/Jean-Romain/lidR
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
# along with this program.  If not, see <http:#www.gnu.org/licenses/>
#
# ===============================================================================


#' Datum transformation for LAS objects
#'
#' A version of \link[rgdal:spTransform]{spTrasform} for \link[lidR:LAS-class]{LAS} objects.
#' Returns transformed coordinates of a \code{LAS} object from the projection of the object to the
#' the projection given by arguments.
#'
#' @param las An object of class \link[lidR:LAS-class]{LAS}
#'
#' @param epsg integer. Unlike \code{spTransform} the EPSG code of the CRS should be preferred for
#' LAS objects. See also \link{epsg} to understand why. If missing, \code{CRSobj} will be used instead.
#' @param CRSobj logical. Object of class \link[sp:CRS-class]{CRS} or of class character, in which
#' case it is converted to \link[sp:CRS-class]{CRS}.

#'
#' @return An object of class \link[lidR:LAS-class]{LAS} with coordinates XY transformed to the new
#' coordinate reference system.
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzrn")
#'
#' head(las@data)
#'
#' las <- lastransform(las, 26918)
lastransform = function(las, epsg, CRSobj = NULL)
{
  UseMethod("lastransform", las)
}

#' @export
lastransform.LAS = function(las, epsg, CRSobj = NULL)
{
  . <- X <- Y <- NULL

  if (!missing(epsg))
    CRSobj = sp::CRS(paste0("+init=epsg:", epsg))

  if (is.na(sp::proj4string(las)))
    stop("No transformation possible from NA reference system")

  spts = sp::SpatialPoints(coordinates(las))
  sp::proj4string(spts) <- sp::proj4string(las)
  spts = sp::spTransform(spts, CRSobj)

  las@data[["X"]] <- spts@coords[,1]
  las@data[["Y"]] <- spts@coords[,2]

  las <- lasupdateheader(las)

  if (!missing(epsg))
    epsg(las) <- epsg
  else
    sp::proj4string(las) <- sp::proj4string(spts)

  return(las)
}
