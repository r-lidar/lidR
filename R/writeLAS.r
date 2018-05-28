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



#' Write a las or laz file
#'
#' Write a LAS object into a binary file (.las or .laz specified in filename)
#'
#' @param .las an object of class LAS
#' @param file character. A character string naming an output file
#' @return void
#' @export
writeLAS = function(.las, file)
{
  stopifnotlas(.las)
  assertive::assert_is_a_string(file)

  file  = path.expand(file)
  islas = tools::file_ext(file) %in% c("las", "laz")

  if (!islas) stop(glue("File(s) {file} not supported"), call. = FALSE)

  rlas::write.las(file, as.list(.las@header), .las@data)
  return(invisible())
}