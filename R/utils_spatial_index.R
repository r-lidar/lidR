#' Spatial index
#'
#' This document explains how to process point-clouds taking advantage of different spatial
#' indexes available in the lidR package. lidR can use several types of spatial indexes to
#' apply algorithms that need a spatial indexing as fast as possible. The choice of the spatial
#' index depends on the type of point-cloud that is processed and the algorithm that is performed.
#' lidR can use a grid partition, a voxel partition, a quadtree or an octree. See details.
#'
#' In lidR (>= 3.1.0), a \link[=LAS-class]{LAS} object records the sensor used to sample
#' the point-cloud (ALS, TLS, UAV, DAP) as well as the spatial index that must be used
#' for processing the point cloud. This can be set manually by the user but the simplest is
#' to use one of the \link[=readLAS]{read*LAS()} function. By default a point-cloud is associated
#' to a sensor and the best spatial index is chosen on-the-fly depending on the algorithm
#' applied. It is possible to force the use of a specific spatial index.
#'
#' Information relative to the spatial indexing are stored in slot `@index` that contains
#' a `list` with two elements:
#'
#' - `sensor`: an integer that records the sensor type
#' - `index`: an integer that records the spatial index to used
#'
#' Some global variable defined in the package can be used to attribute manually the adequate
#' codes if needed. By default the spatial index code is 0 meaning that each function is free
#' to choose a spatial index depending on the recorded sensor. If the code is not 0 then each
#' function will be forced to used the spatial index that is imposed.
#'
#' \link[=LAScatalog-class]{LAScatalog} objects also record such information that is automatically
#' propagated to the LAS objects when processing.
#'
#' Notice that sensor code 0 (not registered) is equivalent to ALS for legacy and backward
#' compatibility reasons. Consequently `readLAS()` and `readALSLAS()` are equivalent.
#'
#' @template param-las
#' @param h boolean. Human readable
#' @param value integer. A code for refereeing to a sensor type or a spatial index type. Use
#' one of `UKNLAS`, `ALSLAS`, `TLSLAS`, `UAVLAS`, `DAPLAS`, `MLSLAS` for sensor and one of
#' `LIDRAUTOINDEX`, `LIDRGRIDPARTITION`, `LIDRVOXELPARTITION`, `LIDRQUADTREE` `LIDROCTREE`
#' for spatial index.
#'
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las <- readLAS(LASfile)
#'
#' # By default the sensor and spatial index codes are 0
#' sensor(las)
#' index(las)
#'
#' # Codes are used internally and not intended to be known by users
#' # Use h option for human readable output
#' sensor(las, h = TRUE)
#' index(las, h = TRUE)
#'
#' # Modification of the sensor enables to select a better spatial index
#' # when processing the point-cloud.
#' sensor(las) <- TLSLAS
#' sensor(las, h = TRUE)
#' index(las, h = TRUE)
#'
#' # Modification of the spatial index forces to use one of the avaialble
#' # spatial index.
#' index(las) <- LIDRQUATREE
#' sensor(las, h = TRUE)
#' index(las, h = TRUE)
#'
#' # The simplest way to take advantage of appropriated spatial indexing is
#' # to use one of read*LAS().
#' las <- readTLSLAS(LASfile)
#' sensor(las, h = TRUE)
#' index(las, h = TRUE)
#'
#' # But for some specific point-cloud / algorithm it might be advised to force
#' # the use of a specific spatial index to perform the computation faster
#' index(las) <- LIDRVOXELPARTITION
#' index(las, h = TRUE)
#'
#' # With a LAScatalog, spatial indexing informations are propagated to the
#' # different chunks
#' ctg = readTLSLAScatalog(LASfile)
#' index(ctg) <- LIDRVOXELPARTITION
#' sensor(ctg, h = TRUE)
#' index(ctg, h = TRUE)
#'
#' @name lidR-spatial-index
#' @md
NULL

#' @rdname lidR-spatial-index
#' @export
LIDRAUTOINDEX      <- 0L

#' @rdname lidR-spatial-index
#' @export
LIDRGRIDPARTITION  <- 1L

#' @rdname lidR-spatial-index
#' @export
LIDRVOXELPARTITION <- 2L

#' @rdname lidR-spatial-index
#' @export
LIDRQUADTREE       <- 3L

#' @rdname lidR-spatial-index
#' @export
LIDROCTREE         <- 4L

#' @rdname lidR-spatial-index
#' @export
UKNLAS <- 0L

#' @rdname lidR-spatial-index
#' @export
ALSLAS <- 1L

#' @rdname lidR-spatial-index
#' @export
TLSLAS <- 2L

#' @rdname lidR-spatial-index
#' @export
UAVLAS <- 3L

#' @rdname lidR-spatial-index
#' @export
DAPLAS <- 4L

#' @rdname lidR-spatial-index
#' @export
MLSLAS <- 5L

NLAS   <- 10L

LIDRDEFAULTINDEX <- list(sensor = UKNLAS, index = LIDRAUTOINDEX)
LIDRALSINDEX     <- list(sensor = ALSLAS, index = LIDRAUTOINDEX)
LIDRTLSINDEX     <- list(sensor = TLSLAS, index = LIDRAUTOINDEX)
LIDRUAVINDEX     <- list(sensor = UAVLAS, index = LIDRAUTOINDEX)
LIDRDAPINDEX     <- list(sensor = DAPLAS, index = LIDRAUTOINDEX)
LIDRMLSINDEX     <- list(sensor = MLSLAS, index = LIDRAUTOINDEX)

#' @rdname lidR-spatial-index
#' @export
sensor = function(las, h = FALSE)
{
  if (isTRUE(h))
    index = format_index(las)
  else
    index = las@index

  return(index[["sensor"]])
}

#' @rdname lidR-spatial-index
#' @export
`sensor<-` = function(las, value)
{
  las@index[["sensor"]] <- value
  return(las)
}

#' @rdname lidR-spatial-index
#' @export
index = function(las, h = FALSE)
{
  if (isTRUE(h))
    index = format_index(las)
  else
    index = las@index

  return(index[["index"]])
}

#' @rdname lidR-spatial-index
#' @export
`index<-` = function(las, value)
{
  las@index[["index"]] <- value
  return(las)
}

format_index = function(las)
{
  index = las@index
  sensor = index$sensor
  index = index$index

  if (sensor == UKNLAS)
    sensor = "not registered"
  else if (sensor == ALSLAS)
    sensor = "ALS"
  else if (sensor == TLSLAS)
    sensor = "TLS"
  else if (sensor == UAVLAS)
    sensor = "UAV"
  else if (sensor == DAPLAS)
    sensor = "DAP"
  else if (sensor == MLSLAS)
    sensor = "mutispectral ALS"
  else if (sensor == UKNLAS + NLAS)
    sensor = "not registered (normalized)"
  else if (sensor == ALSLAS + NLAS)
    sensor = "ALS (normalized)"
  else if (sensor == TLSLAS + NLAS)
    sensor = "TLS (normalized)"
  else if (sensor == UAVLAS + NLAS)
    sensor = "UAV (normalized)"
  else if (sensor == DAPLAS + NLAS)
    sensor = "DAP (normalized)"
  else if (sensor == MLSLAS + NLAS)
    sensor = "mutispectral ALS (normalized)"
  else
    stop("invalid sensor registred", call. = FALSE)

  if (index == LIDRAUTOINDEX)
    index = "automatic selection"
  else if (index == LIDRGRIDPARTITION)
    index = "grid partition"
  else if (index == LIDRVOXELPARTITION)
    index = "voxel partition"
  else if (index == LIDRQUADTREE)
    index = "quadtree"
  else if (index == LIDROCTREE)
    index = "octree"
  else
    stop("invalid spatial index registred", call. = FALSE)

  return(return(list(sensor = sensor, index = index)))
}

force_autoindex = function(las, value)
{
  if (index(las) == LIDRAUTOINDEX)
    index(las) <- value

  return(las)
}
