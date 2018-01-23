# ===============================================================================
#
# PROGRAMMERS:
#
# florian.deboissieu@irstea.fr  -  https://github.com/Jean-Romain/lidR
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

#' Creates or updates extra byte field description
#'
#' Creates or updates extra byte field description in \code{header@VLR}
#' Avalaible parameters for \code{...} are \code{data_type}, \code{min}, \code{max}, \code{scale},
#' \code{offset}.
#'
#' \code{data_type}: integer describing following code of LAS v1.4. Default is 10, i.e. \code{double}.
#' Codes are the following 1=U8, 2=I8, 3=U16, 4=I16, 5=U32, 6=I32, 7=U64, 8=I64, 9=F32, 10=F34.
#'
#' \code{min, max}: minimum and maximum value (after scaling).
#'
#' \code{scale, offset}: factor with which data will be scaled before writing to file.
#'
#' @param .las an object of class LAS
#' @param name character. A character string with field name to update.
#' @param ... see details.
#' @return LASheader object.
#' @export
extra_byte_desc <- function(header, name, ...){

  argsin = list(...)

  eb_desc=list()

  if(is.null(header@VLR$Extra_Bytes$`Extra Bytes Description`)){
    if(is.null(header@VLR$Extra_Bytes)){
      header@VLR$Extra_Bytes$`user ID`= "LASF_Spec"
      header@VLR$Extra_Bytes$`record ID`= 4
      header@VLR$Extra_Bytes$`length after header`= 192
    }
    header@VLR$Extra_Bytes$`Extra Bytes Description`=list()
    header@VLR$Extra_Bytes$`Extra Bytes Description`[name]=list(list())

  }else if(!(name %in% names(header@VLR$Extra_Bytes$`Extra Bytes Description`))){
    header@VLR$Extra_Bytes$`Extra Bytes Description`[name]=list(list())
    header@VLR$Extra_Bytes$`length after header`= header@VLR$Extra_Bytes$`length after header`+192
  }else{
    warning(list("Description of '", name, "' updated with new values."))
  }


  eb_desc = header@VLR$Extra_Bytes$`Extra Bytes Description`[[name]]


  eb_desc$name = name

  if("data_type" %in% names(argsin))
    eb_desc$data_type = argsin$data_type
  else if(!("data_type" %in% names(eb_desc)))
    eb_desc$data_type=10 # double

  if("no_data" %in% names(argsin))
    eb_desc$no_data = argsin$no_data

  if("min" %in% names(argsin))
    eb_desc$min = argsin$min

  if("max" %in% names(argsin))
    eb_desc$max = argsin$max

  if("scale" %in% names(argsin))
    eb_desc$scale = argsin$scale

  if("offset" %in% names(argsin))
    eb_desc$offset = argsin$offset

  if("description" %in% names(argsin))
    eb_desc$description = argsin$description
  else if(!("description" %in% names(eb_desc)))
    eb_desc$description = name

  # Recompute options code
  options = 0
  if(!is.null(eb_desc$no_data))
    options = options + 2^0
  if(!is.null(eb_desc$min))
    options = options + 2^1
  if(!is.null(eb_desc$max))
    options = options + 2^2
  if(!is.null(eb_desc$scale))
    options = options + 2^3
  if(!is.null(eb_desc$offset))
    options = options + 2^4

  eb_desc$options = options

  header@VLR$Extra_Bytes$`Extra Bytes Description`[name] <- list(eb_desc[!sapply(eb_desc, is.null)])
  return(header)
}

#' Add extra byte field
#'
#' Add extra byte field and specify description in \code{header@VLR}
#' Avalaible parameters for \code{...} are \code{data_type}, \code{min}, \code{max}, \code{scale},
#' \code{offset}. These should be vectors of length the number of columns of \code{data} argument.
#'
#' \code{data_type}: integer describing following code of LAS v1.4. Default is 10, i.e. \code{double}.
#' Codes are the following 1=U8, 2=I8, 3=U16, 4=I16, 5=U32, 6=I32, 7=U64, 8=I64, 9=F32, 10=F34.
#'
#' \code{min, max}: minimum and maximum value (in data_type format).
#'
#' \code{scale, offset}: factor with which data will be scaled before writing to file.
#'
#' @param .las an object of class LAS
#' @param data data.table. The Extra Byte data to be added.
#' @param datanames character. A character string with field name to update.
#' @param ... see details.
#' @return an object of class LAS.
#' @export
add.extra_byte <- function(.las, data, datanames = colnames(data), ...){
  argsin=list(...)

  .las@data <- cbind(.las@data, data)


  for(i in 1:length(datanames)){
    eb_desc <- list(header=.las@header, name=datanames[i])

    if("data_type" %in% names(argsin))
      eb_desc$data_type = argsin$data_type[i]

    if("no_data" %in% names(argsin))
      eb_desc$no_data = argsin$no_data[i]

    if("min" %in% names(argsin))
      eb_desc$min = argsin$min[i]

    if("max" %in% names(argsin))
      eb_desc$max = argsin$max[i]

    if("scale" %in% names(argsin))
      eb_desc$scale = argsin$scale[i]

    if("offset" %in% names(argsin))
      eb_desc$offset = argsin$offset[i]

    if("description" %in% names(argsin))
      eb_desc$description = argsin$description[i]

    .las@header <-  do.call(extra_byte_desc, eb_desc)
  }
  return(.las)
}


