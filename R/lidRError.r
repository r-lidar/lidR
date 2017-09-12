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



lidRError = function(code, ..., behaviour = stop)
{
  p <- list(...)

  msg = switch(code,

# testFunctionSignature.r
"TFS1" = list("The expression '", p$expression, "' returned a list in which all elements are not a single numeric or logical value. The field '", p$metric, "' is a '", p$class, "'"),
"TFS2" = list("The expression '", p$expression, "' returned a list in which all elements are not a single numeric or logical value. The field ", utils::capture.output(cat(p$metric, sep = " and ")), " are respectively ", utils::capture.output(cat(p$class, sep= " and "))),
"TFS3" = list("The expression '", p$expression, "' returned a list in which all elements are not a single value. The field '", p$metric, "' has a length of ", p$number),
"TFS4" = list("The expression '", p$expression, "' returned a list in which all elements are not a single value. The fields: ", utils::capture.output(cat(p$metric, sep = " and ")), " have respectively a length of: ", utils::capture.output(cat(p$number, sep=" and "))),
"TFS5" = list("The expression '", p$expression, "' returned a data.frame. A single number or a list of single number is expected."),
"TFS6" = list("The expression '", p$expression, "' returned a vector of length ", p$number,  ". A single number or a list of single number is expected."),

# ClassLAS.r set*()
"LDR1" = list("Invalid parameter data in constructor"),
"LDR2" = list("Dataset may be invalid: ", p$number, " points below 0 found."),
"LDR3" = list("Dataset may be invalid: ", p$number, " unclassified points found."),
"LDR4" = list("No '", p$infield, "' field found. '", p$outfield, "', cannot be computed from this file."),
"LDR5" = list("Parameter n of function lasfilternth incorrect."),
"LDR6" = list("This algorithm does not exist."),
"LDR7" = list("'flightlineID': no such field in the dataset. Check function 'lasflightline'"),
"LDR8" = list("ScanDirectionFlag field is not valid according to LAS specifications. Cannot compute 'scanlineID'"),
"LDR9" = list("'data' is empty. No point found"),
"LDR10" = list("Dataset is invalid: ", p$number, " points with a return number of 0 found."),
"LDR11" = list("'", p$what, "' was missing in the header. The value ", p$num, " was automatically attributed."),
"LDR12" = list(p$what, " were missing in the header. The values ", utils::capture.output(cat(p$num, sep = " ")), " were automatically attributed."),

# ClassGridMetric.r
"GDM1" = list("More than 3 columns in the variable: please input the metric name to plot it."),
"GDM2" = list(p$number, " duplicated rasters have been found. X,Y variables do not identify a single observation for each output cell. Automatic aggregation has been done using the mean function"),
"GDM3" = list("More than 3 columns: please input the third column's name to transform the data.frame into a matrix."),

# ClassLAScatalog.r
"CTG1" = list("Invalid parameter input in constructor. Expected a string."),
"CTG2" = list("This folder does not exist"),

# readLASheader.r and readLAS.r
"LAS1" = list("File(s) ", p$files, " not found"),
"LAS2" = list("File(s) ", p$files, " not supported"),
"LAS3" = list("Unable to read any input file(s)"),
"LAS4" = list("The field parameter is invalid. It must to be a string: 'minimal' or 'standard' or 'all'"),
"LAS5" = list("Please write only one file at a time"),

# ClassVoxel.r
"VOX1" = list("This display method does not exist."),

# Getters
"GET1" = list("No point found with the condition: ", p$expression),

# Thin
"THI1" = list("No 'pulseID' field found.  Function 'lasdecimate' cannot be run.")
  )

  msg = do.call(paste, list(msg, sep = ""))

  behaviour(msg, call. = F)
}

stopifnotlas = function(x)
{
  if (!is(x, "LAS"))
    stop("First argument is not a LAS object", call. = F)
}
