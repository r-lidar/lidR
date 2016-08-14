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



#' @importFrom magrittr %>%
#' @importFrom rgl shade3d
cube <- function(x, y, z, col, scale = 1)
{

  mycube <- rgl::cube3d()

  # Reduce size to unit
  mycube$vb[4,] <- mycube$vb[4,]/scale*2

  for (i in 1:length(x))
  {
    # Add cube border
    bcube <- mycube
    bcube$material$lwd <- 2
    bcube$material$front <- 'line'
    bcube$material$back <- 'line'
    bcube %>% rgl::translate3d(x[i], y[i], z[i]) %>% shade3d

    # Add cube fill
    fcube <- mycube
    fcube$vb[4,] <- fcube$vb[4,]*1.01
    fcube$material$col <- col[i]
    fcube %>% rgl::translate3d(x[i], y[i], z[i]) %>% shade3d
  }
}