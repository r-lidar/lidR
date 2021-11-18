#' Spatial index
#'
#' This document explains how to process point-clouds taking advantage of different spatial
#' indexes available in the lidR package. lidR can use several types of spatial indexes to
#' apply algorithms (that need a spatial indexing) as fast as possible. The choice of the spatial
#' index depends on the type of point-cloud that is processed and the algorithm that is performed.
#' lidR can use a grid partition, a voxel partition, a quadtree or an octree. See details.
#'
#' From lidR (>= 3.1.0), a \link[=LAS-class]{LAS} object records the sensor used to sample
#' the point-cloud (ALS, TLS, UAV, DAP) as well as the spatial index that must be used
#' for processing the point cloud. This can be set manually by the user but the simplest is
#' to use one of the \link[=readLAS]{read*LAS()} functions. By default a point-cloud is associated
#' to a sensor and the best spatial index is chosen on-the-fly depending on the algorithm
#' applied. It is possible to force the use of a specific spatial index.
#'
#' Information relative to the spatial indexing is stored in slot `@index` that contains
#' a `list` with two elements:
#'
#' - `sensor`: an integer that records the sensor type
#' - `index`: an integer that records the spatial index to be used
#'
#' By default the spatial index code is 0 ("automatic") meaning that each function is free
#' to choose a different spatial index depending on the recorded sensor. If the code is not
#' 0 then each function will be forced to used the spatial index that is imposed. This,
#' obviously, applies only to functions that use spatial indexing.
#'
#' \link[=LAScatalog-class]{LAScatalog} objects also record such information that is automatically
#' propagated to the LAS objects when processing.
#'
#' Note: before version 3.1.0, point-clouds were all considered as ALS because lidR was originally
#' designed for ALS. Consequently, for legacy and backwards-compatibility reasons, `readLAS()`
#' and `readALSLAS()` are actually equivalent. `readLAS()` tags the point cloud with "unknown"
#' sensor while `readALSLAS()` tags it with 'ALS'. Both behave the same and this is
#' especially true compared with versions < 3.1. As a consequence, using `readLAS()` provides
#' the same performance (no degradation) than in previous versions, while using one of the `read*LAS()`
#' functions may improve the performance.
#'
#' @template param-las
#' @param h boolean. Human readable. Everything is stored as integers that are understood
#' internally. Use `h = TRUE` for user readable output.
#' @param value integer or character. A code for referring to a sensor type or a spatial
#' index type. Use one of `"unknown"`, `"als"`, `"tls"`, `"uav"`, `"dap"`, `"multispectral"`
#' for sensor type and one of `"auto"`, `"gridpartition"`, `"voxelpartition"`, `"quadtree"`, `"octree"`
#' for spatial index type.
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
#' # Modification of the sensor enables users to select a better spatial index
#' # when processing the point-cloud.
#' sensor(las) <- "tls"
#' sensor(las, h = TRUE)
#' index(las, h = TRUE)
#'
#' # Modification of the spatial index forces users to choose one of the available
#' # spatial indexes.
#' index(las) <- "quadtree"
#' sensor(las, h = TRUE)
#' index(las, h = TRUE)
#'
#' # The simplest way to take advantage of appropriate spatial indexing is
#' # to use one of the read*LAS() functions.
#' las <- readTLSLAS(LASfile)
#' sensor(las, h = TRUE)
#' index(las, h = TRUE)
#'
#' # But for some specific point-clouds / algorithms it might be advisable to force
#' # the use of a specific spatial index to perform the computation faster
#' index(las) <- "voxelpartition"
#' index(las, h = TRUE)
#'
#' # With a LAScatalog, spatial indexing information is propagated to the
#' # different chunks
#' ctg = readTLSLAScatalog(LASfile)
#' index(ctg) <- "voxelpartition"
#' sensor(ctg, h = TRUE)
#' index(ctg, h = TRUE)
#'
#' # ==================
#' # PERFORMANCE TESTS
#' # ==================
#'
#' \dontrun{
#' # Performance tests on TLS
#' # ------------------------
#'
#' # The package does not include TLS data
#' # so we can generate something that looks TLS-ish
#' # >>>>>>>>>>>
#' X <- runif(50, -25, 25)
#' Y <- runif(50, -25, 25)
#' X <- as.numeric(sapply(X, function(x) rnorm(2000, x, 2)))
#' Y <- as.numeric(sapply(Y, function(x) rnorm(2000, x, 2)))
#' Z <- abs(rnorm(length(Y), 10, 5))
#' veg <- data.frame(X,Y,Z)
#' X <- runif(5000, -30, 30)
#' Y <- runif(5000, -30, 30)
#' Z <- runif(5000, 0, 1)
#' ground <- data.frame(X,Y,Z)
#' X <- runif(30, -30, 30)
#' Y <- runif(30, -30, 30)
#' Z <- runif(30, 0, 30)
#' noise <- data.frame(X,Y,Z)
#' las <- LAS(rbind(ground, veg, noise))
#' # <<<<<<<<<<<<<
#'
#' plot(las)
#'
#' # If read with readALSLAS()
#' sensor(las) <- "als"
#' system.time(classify_noise(las, sor(20, 8)))
#' #> 1.5 sec
#'
#' # If read with readTLSLAS()
#' sensor(las) <- "tls"
#' system.time(classify_noise(las, sor(20, 8)))
#' #> 0.6 sec
#'
#' # Performance tests on ALS
#' # ------------------------
#'
#' # The package does not include large ALS data
#' # so we can generate something that looks ALS-ish
#' # >>>>>>>>>>>
#' X <- runif(4e5, 0, 1000)
#' Y <- runif(4e5, 0, 1000)
#' Z <- 40*sin(0.01*X) + 50*cos(0.005*Y) + abs(rnorm(length(Y), 10, 5))
#' veg <- data.frame(X,Y,Z)
#' X <- runif(100, 0, 1000)
#' Y <- runif(100, 0, 1000)
#' Z <- 40*sin(0.01*X) + 50*cos(0.005*Y) + abs(rnorm(length(Y), 10, 5)) + runif(100, 30, 70)
#' noise <- data.frame(X,Y,Z)
#' las <- LAS(rbind(veg, noise))
#' # <<<<<<<<<<<<<
#'
#' plot(las)
#'
#' # If read with readALSLAS()
#' sensor(las) <- "als"
#' system.time(classify_noise(las, sor(15, 8)))
#' #> 3 sec
#'
#' # If read with readTLSLAS()
#' sensor(las) <- "tls"
#' system.time(classify_noise(las, sor(15, 8)))
#' #> 4.3 sec
#' }
#' @name lidR-spatial-index
#' @md
NULL
