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

txtProgressBarMulticore = function(min = 0, max = 1, initial = 0, char = "=", width = NA, title, label, style = 1, file = "")
{
  sink(tempfile())
  pbfile = tempfile()
  write(0, pbfile)
  progressBar  = utils::txtProgressBar(min, max, initial, char, width, title, label, style, file)
  attr(progressBar, "pbfile") = pbfile
  sink()
  return(progressBar)
}

addTxtProgressBarMulticore = function(pb, value)
{
  pbfile = attr(pb, "pbfile")
  i = data.table::fread(pbfile)$V1 + value
  utils::setTxtProgressBar(pb, i)
  write(i, pbfile)
}
