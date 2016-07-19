#' Voxelize the space and compute metrics for each voxel
#'
#' Voxelize the cloud of points and compute a series of descriptive statistics for
#' each voxel.
#'
#' Voxelize creates a 3D matrix of voxels with a given resolution. It creates a voxel
#' from the cloud of point if there is at least one point in the voxel. For each voxel
#' the function allows computation of one or several derived metrics in the same way as
#' the gridMetrics functions.
#' Basically there are no predifined metrics. Users must write their own function to create metrics.
#' Voxelize will dispach the LiDAR data for each voxel in the user's function. The user writes their
#' function without considering grid cells, only a cloud of points (see example).
#'
#' @aliases  voxelize
#' @param obj An object of class \code{Lidar}
#' @param res numeric. The size of the cells
#' @param func the function to be apply to each cells
#' @return It returns a \code{data.table} containing the metrics for each voxel. The table has the class "voxels" enabling to easier plotting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
#'
#' # Cloud of points is voxelized with a 1 meter resolution and in each voxel
#' # the number of points is computed.
#' voxelize(lidar, 1, length(Z))
#'
#' # Cloud of points is voxelized with a 1 meter resolution and in each voxel
#' # the mean scan angle of points is computed.
#' voxelize(lidar, 1, mean(ScanAngle))
#'
#' # Define your own metric function
#' myMetrics = function(i, angle, pulseID)
#' {
#'   ret = list(
#'         npulse  = length(unique(pulseID)),
#'         angle   = mean(angle),
#'         imean   = mean(i)
#'         )
#'
#'    return(ret)
#'  }
#'
#' voxels = voxelize(lidar, 20, myMetrics(Intensity, ScanAngle, pulseID))
#'
#' plot(voxels, "angle")
#' plot(voxels, "imean")
#' #etc.
#' @export voxelize
#' @importFrom plyr round_any
setGeneric("voxelize", function(obj, res, func){standardGeneric("voxelize")})

#' @rdname voxelize
setMethod("voxelize", "Lidar",
    function(obj, res, func)
    {
        func_call = substitute(func)

	      obj@data %$% eval(func_call) %>% .testFuncSignature(func_call)

        x_raster = plyr::round_any(obj@data$X, res)
        y_raster = plyr::round_any(obj@data$Y, res)
        z_raster = plyr::round_any(obj@data$Z-0.5*res, res)+0.5*res

        by = list(Xc = x_raster, Yc = y_raster, Zc = z_raster)

        stat <- obj@data[, c(eval(func_call)), by=by]

        n = names(stat)
        n[1:3] = c("X", "Y", "Z")
        setnames(stat, n)

        attr(stat, "class") = c("voxels", attr(stat, "class"))
        attr(stat, "res") = res

        return(stat)
    }
)