# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017 Jean-Romain Roussel
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

get_epsg = function(header)
{
  epsg = NULL
  for (tag in header@VLR$GeoKeyDirectoryTag$tag)
  {
    if (tag$key == 3072)
       epsg = tag$`value offset`
  }

  return(epsg)
}


.epsg2proj = function(code)
{
  if (is.null(code))
    return(sp::CRS())

  crs = tryCatch(sp::CRS(paste0("+init=epsg:", code)), error = function (e) sp::CRS())

  return(crs)
}

.epsg2wkt = function(code)
{
  if (is.null(code))
    return(NA_character_)

  url = paste0("http://spatialreference.org/ref/epsg/", code, "/ogcwkt/")

  proj = tryCatch(readLines(url, warn = F), error = function (e) NA_character_)

  return(proj)
}

epsg2proj = memoise::memoise(.epsg2proj)
epsg2wkt = memoise::memoise(.epsg2wkt)
