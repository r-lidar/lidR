# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2019 Jean-Romain Roussel
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

#' Summary and Print for \code{LAS*} objects
#'
#' @param object,x A \code{LAS*} object or other lidR related objects.
#' @param ... Unused
setGeneric("print", function(x, ...)
  standardGeneric("print"))

setGeneric("summary", function(object, ...)
  standardGeneric("summary"))

#' @rdname print
#' @aliases summary
#' @export
setMethod("summary", "LAS", function(object, ...)
{
  print(object)
  print(object@header)
  return(invisible(object))
})

#' @rdname print
#' @export
setMethod("print", "LAS", function(x)
{
  show(x)
  return(invisible(x))
})

setMethod("show", "LAS", function(object)
{
  size      <- format(utils::object.size(object), units = "auto")
  area      <- area(object)
  area.h    <- area
  npoints   <- nrow(object@data)
  npoints.h <- npoints
  dpts      <- if (area > 0) npoints/area else 0
  #attr      <- names(object@data)
  ext       <- sp::bbox(object)
  phb       <- object@header@PHB
  major     <- phb[["Version Major"]]
  minor     <- phb[["Version Minor"]]
  version   <- paste(major, minor, sep = ".")
  format    <- phb[["Point Data Format ID"]]

  coord.ref <- sf::st_crs(object@proj4string)
  coord.ref <- if (is.null(coord.ref$wkt)) coord.ref$proj4string else coord.ref$input

  units <- regmatches(object@proj4string@projargs, regexpr("(?<=units=).*?(?=\\s)", object@proj4string@projargs, perl = TRUE))
  units <- if (length(units) == 0) "units" else units

  areaprefix  <- ""
  pointprefix <- ""

  if (area > 1000*1000/2)
  {
    areaprefix <- if (length(units) == 0) "thousand " else "k"
    area.h     <- round(area/(1000*1000), 2)
  }

  if (npoints > 1000 & npoints < 1000^2)
  {
    pointprefix <- "thousand"
    npoints.h   <- round(npoints/1000, 1)
  }
  else if (npoints >= 1000^2 & npoints < 1000^3)
  {
    pointprefix <- "million"
    npoints.h   <- round(npoints/(1000^2), 2)
  }
  else if (npoints >= 1000^3)
  {
    pointprefix <- "billion"
    npoints.h   <- round(npoints/(1000^3), 2)
  }

  cat("class        : ", class(object), " (v", version, " format ", format, ")\n", sep = "")
  cat("memory       :", size, "\n")
  cat("extent       : ", ext[1,1], ", ", ext[1,2], ", ", ext[2,1], ", ", ext[2,2], " (xmin, xmax, ymin, ymax)\n", sep = "")
  cat("coord. ref.  :", coord.ref, "\n")
  cat("area         : ", area.h, " ", areaprefix, units, "\u00B2\n", sep = "")
  cat("points       :", npoints.h, pointprefix, "points\n")
  cat("density      : ", round(dpts, 2), " points/", units, "\u00B2\n", sep = "")
  #cat("names        :", attr, "\n")

  return(invisible(object))
})

setMethod("show", "LAScatalog", function(object)
{
  area        <- area(object)
  area.h      <- area
  npoints     <- sum(object@data$Number.of.point.records)
  npoints.h   <- npoints
  ext         <- object@bbox
  units      <- regmatches(object@proj4string@projargs, regexpr("(?<=units=).*?(?=\\s)", object@proj4string@projargs, perl = TRUE))
  units       <- if (length(units) == 0) "units" else units
  areaprefix  <- ""
  pointprefix <- ""
  major       <- sort(unique(object[["Version.Major"]]))
  minor       <- sort(unique(object[["Version.Minor"]]))
  version     <- paste(major, minor, sep = ".", collapse = " and ")
  format      <- paste(sort(unique(object[["Point.Data.Format.ID"]])), collapse = " and ")
  density     <- round(npoints/area, 1)

  if (is.nan(density)) density <- 0

  coord.ref <- sf::st_crs(object@proj4string)
  coord.ref <- coord.ref$input

  if (area > 1000*1000/2)
  {
    areaprefix <- if (length(units) == 0) "thousand " else "k"
    area.h     <- round(area/(1000*1000), 2)
  }

  if (npoints <= 1000)
  {
    pointprefix <- ""
    npoints.h   <- npoints
  }
  else if (npoints > 1000 & npoints < 1000^2)
  {
    pointprefix <- "thousand"
    npoints.h   <- round(npoints/1000, 1)
  }
  else if (npoints >= 1000^2 & npoints < 1000^3)
  {
    pointprefix <- "million"
    npoints.h   <- round(npoints/(1000^2), 2)
  }
  else if (npoints >= 1000^3)
  {
    pointprefix <- "billion"
    npoints.h   <- round(npoints/(1000^3), 2)
  }

  cat("class       : ", class(object), " (v", version, " format ", format, ")\n", sep = "")
  cat("extent      : ", ext[1,1], ", ", ext[1,2], ", ", ext[2,1], ", ", ext[2,2], " (xmin, xmax, ymin, ymax)\n", sep = "")
  cat("coord. ref. :", coord.ref, "\n")
  cat("area        : ", area.h, " ", areaprefix, units, "\u00B2\n", sep = "")
  cat("points      : ", npoints.h, pointprefix, " points\n", sep = "")
  cat("density     : ", density, " points/", units, "\u00B2\n", sep = "")
  cat("num. files  :", dim(object@data)[1], "\n")
  return(invisible(object))
})

#' @rdname print
setMethod("summary", "LAScatalog", function(object, ...)
{
  inmemory <- if (opt_output_files(object) == "") "in memory" else "on disk"
  w2w  <- if (opt_wall_to_wall(object)) "guaranteed" else "not guaranteed"
  merging <- if (opt_merge(object)) "enabled" else "disable"

  show(object)
  cat("proc. opt.  : buffer: ", opt_chunk_buffer(object), " | chunk: ", opt_chunk_size(object), "\n", sep = "")
  cat("input opt.  : select: ", opt_select(object), " | filter: ", opt_filter(object), "\n", sep = "")
  cat("output opt. : ", inmemory, " | w2w ", w2w, " | merging ", merging, "\n", sep = "")
  cat("drivers     :\n")

  drivers <- object@output_options$drivers
  dnames <- names(drivers)
  for (i in 1:length(drivers))
  {
    driver <- drivers[[i]]
    params <- driver$param
    pnames <- names(params)
    cat(" -", dnames[i], ": ")
    if (length(params) > 0)
    {
      for (j in 1:length(params))
        cat(pnames[j], "=", params[[j]], " ")
    }
    else
    {
      cat("no parameter")
    }
    cat("\n")

  }
  return(invisible(object))
})

setMethod("show", "LASheader",  function(object)
{
  x = object@PHB

  gpstype   = if (x[["Global Encoding"]][["GPS Time Type"]]) "Standard GPS Time" else "GPS Week Time"
  synthetic = if (x[["Global Encoding"]][["Synthetic Return Numbers"]]) "true" else "no"
  WKT       = if (x[["Global Encoding"]][["WKT"]]) "CRS is WKT" else "CRS is GeoTIFF"
  aggregate = if (x[["Global Encoding"]][["Aggregate Model"]]) "true" else "false"

  cat("File signature:          ", x$`File Signature`, "\n")
  cat("File source ID:          ", x$`File Source ID`, "\n")
  cat("Global encoding:\n")
  cat(" - GPS Time Type:" , gpstype, "\n")
  cat(" - Synthetic Return Numbers:" , synthetic, "\n")
  cat(" - Well Know Text:" , WKT, "\n")
  cat(" - Aggregate Model:" , aggregate, "\n")
  cat("Project ID - GUID:       ", x$`Project ID - GUID`, "\n")
  cat("Version:                  ", x$`Version Major`, ".", x$`Version Minor`, "\n", sep = "")
  cat("System identifier:       ", x$`System Identifier`, "\n")
  cat("Generating software:     ", x$`Generating Software`, "\n")
  cat("File creation d/y:        ", x$`File Creation Day of Year`, "/", x$`File Creation Year`, "\n", sep = "")
  cat("header size:             ", x$`Header Size`, "\n")
  cat("Offset to point data:    ", x$`Offset to point data`, "\n")
  cat("Num. var. length record: ", x$`Number of variable length records`, "\n")
  cat("Point data format:       ", x$`Point Data Format ID`, "\n")
  cat("Point data record length:", x$`Point Data Record Length`, "\n")
  cat("Num. of point records:   ", x$`Number of point records`, "\n")
  cat("Num. of points by return:", x$`Number of points by return`, "\n")
  cat("Scale factor X Y Z:      ", x$`X scale factor`, x$`Y scale factor`, x$`Z scale factor`, "\n")
  cat("Offset X Y Z:            ", x$`X offset`, x$`Y offset`, x$`Z offset`, "\n")
  cat("min X Y Z:               ", x$`Min X`, x$`Min Y`, x$`Min Z`, "\n")
  cat("max X Y Z:               ", x$`Max X`, x$`Max Y`, x$`Max Z`, "\n")

  n = length(object@VLR)

  if (n == 0)
  {
    cat("Variable length records:  void\n")
    return(invisible())
  }

  cat("Variable length records: \n")

  for (i in 1:n)
  {
    vlr = object@VLR[[i]]

    cat("   Variable length record", i, "of", n, "\n")
    #cat("       Reserve:            ",  vlr$reserved, "\n")
    #cat("       User ID:             ", vlr$`user ID`, "\n")
    #cat("       record ID:           ", vlr$`record ID`, "\n")
    #cat("       Length after header: ", vlr$`length after header`, "\n")
    cat("       Description:", vlr$description, "\n")

    if (vlr$`record ID` == 34735)
    {
      cat("       Tags:\n")
      lapply(vlr[[6]], function(xx)
      {
        cat("          Key", xx$key, "value", xx$`value offset`, "\n")
      })
    }
    else if (vlr$`record ID` == 34736)
    {
      cat("       data:                ", vlr[[6]], "\n")
    }
    else if (vlr$`record ID` == 34737)
    {
      cat("       data:                ", vlr[[6]], "\n")
    }
    else if (vlr$`record ID` == 4)
    {
      cat("       Extra Bytes Description:\n")
      lapply(vlr$`Extra Bytes Description`, function(xx)
      {
        cat("          ", xx$name, ": ", xx$description, "\n", sep = "")
      })
    }
    else if (vlr$`record ID` == 4)
    {
      cat("       Extra Bytes Description:\n")
      lapply(vlr$`Extra Bytes Description`, function(xx)
      {
        cat("          ", xx$name, ": ", xx$description, "\n", sep = "")
      })
    }
    else if (vlr$`record ID` == 2112)
    {
      cat("       WKT OGC COORDINATE SYSTEM: ", strtrim(vlr$`WKT OGC COORDINATE SYSTEM`, 70), " [...] (truncated)\n", sep = "")
    }
  }

  return(invisible(object))
})

setMethod("show", "LAScluster", function(object)
{
  cat("class   : LAScluster\n")
  cat("name    :", object@name, "\n")
  cat("center  :", object@center$x, ",", object@center$y, "\n")
  cat("extent  :", object@bbox[1], ",", object@bbox[3], ",", object@bbox[2], ",", object@bbox[4], "(xmin, xmax, ymin, ymax)\n")
  cat("extent+ :", object@bbbox[1], ",", object@bbbox[3], ",", object@bbbox[2], ",", object@bbbox[4], "(xmin, xmax, ymin, ymax)\n")
  cat("size    :", object@width, "x", object@height, "\n")
  cat("files   :", basename(object@files), "\n")
  cat("filter  :", object@filter, "\n")
  return(invisible(object))
})


#' @export
#' @method print lidRAlgorithm
#' @rdname print
print.lidRAlgorithm = function(x, ...)
{
  e <- environment(x)
  params <- ls(e)
  params <- Filter(function(i) !is.algorithm(get(i,e)), params)
  omp <- if (is.parallelised(x)) "yes" else "no"

  if (is(x, LIDRDSM))
  { use = "digital surface model" ; with = LIDRCONTEXTDSM }
  else if (is (x, LIDRDEC))
  { use = "point cloud thinning" ; with = LIDRCONTEXTDEC }
  else if (is (x, LIDRGND))
  { use = "ground classification" ; with = LIDRCONTEXTGND }
  else if (is (x, LIDRITD))
  { use = "individual tree detection" ; with = LIDRCONTEXTITD }
  else if (is (x, LIDRITS))
  { use = "individual tree segmentation" ; with = LIDRCONTEXTITS }
  else if (is (x, LIDRNIT))
  { use = "intensity normalisation" ; with = LIDRCONTEXTNIT }
  else if (is (x, LIDRSNG))
  { use = "snag segmentation" ; with = LIDRCONTEXTSNG }
  else if (is (x, LIDRTRK))
  { use = "sensor tracking" ; with = LIDRCONTEXTTRK }
  else if (is (x, LIDRSHP))
  { use = "shape segmentation" ; with = LIDRCONTEXTSHP }
  else if (is (x, LIDRSPI))
  { use = "spatial interpolation" ; with = LIDRCONTEXTSPI }
  else if (is (x, LIDROUT))
  { use = "noise classification" ; with = LIDRCONTEXTOUT }
  else
  { use = "unknown" ; with = "unknown" } # nocov

  with = paste(with, collapse = " or ")

  cat("Object of class lidR algorithm\n")
  cat("Algorithm for:", use, "\n")
  cat("Designed to be used with:", with, "\n")
  cat("Native C++ parallelization:", omp, "\n")
  cat("Parameters: ")

  if (length(params) == 0L)
  {
    cat("none\n")
    return(invisible(x))
  }
  else
    cat("\n")

  for (param in params)
  {
    v = get(param, e)
    if (is.numeric(v) | is.complex(v) | is.logical(v) | is.character(v))
      cat(" - ", param, " = " , v, " <", class(v), ">\n", sep = "")
    else if (is.null(v))
      cat(" - ", param, " = NULL <NULL>\n", sep = "")
    else
      cat(" - ", param, " <", class(v), ">\n", sep = "")
  }

  return(invisible(x))
}
