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



# Test if a function return is valid for the grid_metrics function
#
#
# When a user creates their own function and applies it to the grid_metrics function it can sometimes return
# a conceptual error. This function tests if the return is valid before aggregating the
# data.
.testFuncSignature = function(metrics, func)
{
  funcstring = deparse(func)

  if(is.list(metrics) & !is.data.frame(metrics))
  {
    if(is.null(names(metrics)))
      names(metrics) = paste0("#", 1:length(metrics))

    classes = sapply(metrics, class)
    test = classes %in% c("integer", "numeric", "logical", "character")
    n = names(metrics[!test])
    c = classes[!test]

    if(sum(!test) == 1)
      lidRError("TFS1", expression = funcstring, metric = n, class = c)
    else if(sum(!test) > 1)
      lidRError("TFS2", expression = funcstring, metric = n, class = c)

    size = sapply(metrics, length)
    test = size == 1

    n = names(metrics[!test])
    c = size[!test]

    if(sum(!test) == 1)
      lidRError("TFS3", expression = funcstring, metric = n, number = c)
    else if(sum(!test) > 1)
      lidRError("TFS4", expression = funcstring, metric = n, number = c)
  }
  else if(is.data.frame(metrics))
    lidRError("TFS5", expression = funcstring)
  else if(is.vector(metrics) & length(metrics) > 1)
    lidRError("TFS6", expression = funcstring, number = length(metrics))
  else
    return(0)
}