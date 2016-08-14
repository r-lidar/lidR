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



#' @importFrom utils capture.output
lidRError = function(code, ..., behaviour = stop)
{
  p <- list(...)

  msg = switch(code,

# testFunctionSignature.r
"TFS1" = list("The expression '", p$expression, "' returned a list in which all elements are not a single numeric or logical value. The field '", p$metric, "' is a '", p$class, "'"),
"TFS2" = list("The expression '", p$expression, "' returned a list in which all elements are not a single numeric or logical value. The field ", capture.output(cat(p$metric, sep=" and ")), " are respectively ", capture.output(cat(p$class, sep= " and "))),
"TFS3" = list("The expression '", p$expression, "' returned a list in which all elements are not a single value. The field '", p$metric, "' has a length of ", p$number),
"TFS4" = list("The expression '", p$expression, "' returned a list in which all elements are not a single value. The fields: ", capture.output(cat(p$metric, sep=" and ")), " have respectively a length of: ", capture.output(cat(p$number, sep=" and "))),
"TFS5" = list("The expression '", p$expression, "' returned a data.frame. A single number or a list of single number is expected."),
"TFS6" = list("The expression '", p$expression, "' returned a vector of lenght ", p$number,  ". A single number or a list of single number is expected."),

# ClassLAS.r
"LDR1" = list("Invalid parameter input in constructor"),
"LDR2" = list("Dataset may be invalid: ", p$number, " points below 0 found."),
"LDR3" = list("Dataset may be invalid: ", p$number, " unclassified points found."),
"LDR4" = list("No gpstime field found. 'pulseID', 'flightlines' and 'pulse density' cannot be computed from this file."),
"LDR5" = list("Parameter n of function getNth incorrect."),
"LDR6" = list("This algorithm does not exist."),
"LDR7" = list("The option '", p$option, "' does not exist."),

# ClassGridMetric.r
"GDM1" = list("More than 3 columns in the variable: please input the metric name to plot it."),
"GDM2" = list(p$number, "duplicated rasters have been found. X,Y variables do not identify a single observation for each output cell. Automatic aggregation have been done using mean function"),
"GDM3" = list("More than 3 columns: please input the column's name tranform it into a matrix."),

# ClassCatalog.r
"CTG1" = list("Invalid parameter input in constructor. Expected string."),
"CTG2" = list("This folder does not exist"),

# readLASheader.r and readLAS.r
"LAS1" = list("File(s) ", p$files, " not found"),
"LAS2" = list("File(s) ", p$files, " not supported"),
"LAS3" = list("Unable to read any input file(s)"),
"LAS4" = list("The field parameter is invalid. It must to be a string: 'minimal' or 'standard' or 'all'"),
"LAS5" = list("Please write only one file at a time"),

# ClassVoxel.r
"VOX1" = list("This diplay method does not exist.")
  )

  msg = do.call(paste, list(msg, sep=""))

  behaviour(msg, call.=F)
}
