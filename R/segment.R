#' Segment a point cloud
#'
#' Segment a point cloud using different methods. `segment_*` functions add a new attribute to the
#' point cloud to label each point. They segment either individual trees, snags, or
#' geometrical features.
#'
#' \describe{
#' \item{`segment_trees`}{Individual tree segmentation with several possible algorithms. The returned
#' point cloud has a new extra byte attribute named after the parameter `attribute` independently
#' of the algorithm used.}
#' \item{`segment_shapes`}{Computes, for each point, the eigenvalues of the covariance matrix of the
#' neighbouring points. The eigenvalues are later used either to segment linear/planar points or to
#' compute derived metrics. The points that meet a given criterion based on the eigenvalue are labelled
#' as approximately coplanar/colinear or any other shape supported.}
#' \item{`segment_snags`}{Snag segmentation using several possible algorithms. The function attributes
#' a number identifying a snag class (`snagCls` attribute) to each point of the point cloud.
#' The classification/segmentation is done at the point cloud level and currently only one algorithm is
#' implemented, which uses LiDAR intensity thresholds and specified neighbourhoods to differentiate
#' bole and branch from foliage points.}
#' }
#'
#' @section Non-supported LAScatalog options:
#' The option `select` is not supported and not respected because it always preserves the file format
#' and all the attributes. `select = "*"` is imposed internally.
#'
#' @template param-las
#' @param algorithm function. An algorithm for segmentation. For individual tree segmentation, lidR
#' has \link{dalponte2016}, \link{watershed}, \link{li2012}, and \link{silva2016}. More experimental
#' algorithms may be found in the package [lidRplugins](https://github.com/Jean-Romain/lidRplugins).
#' For snag segmentation, \code{lidR} has \link{wing2015}. For geometry segmentation, lidR has
#' \link{shp_plane}, \link{shp_hplane}, and \link{shp_line}.
#' @param attribute character. The returned LAS object as a new attribute (in a new column).
#' This parameter controls the name of the new attribute.
#' @param uniqueness character. A method to compute a unique ID. Can be 'incremental', 'gpstime' or
#' 'bitmerge'. See section 'Uniqueness'. This feature must be considered as 'experimental'.
#' @param filter formula of logical predicates. Enables the function to run only on points of interest
#' in an optimized way. See the examples.
#'
#' @template section-uniqueness
#'
#' @name segment
#' @rdname segment
#' @md
#' @examples
#' # ==============
#' # Segment trees
#' # ==============
#'
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'
#' # Using Li et al. (2012)
#' las <- segment_trees(las, li2012(R = 3, speed_up = 5))
#' #plot(las, color = "treeID")
#'
#' # ==============
#' # Segment shapes
#' # ==============
#'
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, filter = "-keep_random_fraction 0.5")
#'
#' # Use the eigenvalues to estimate if points are part of a local plan
#' las <- segment_shapes(las, shp_plane(k = 15), "Coplanar")
#' #plot(las, color = "Coplanar")
#'
#' \dontrun{
#' # Drop ground point at runtime
#' las <- segment_shapes(las, shp_plane(k = 15), "Coplanar", filter = ~Classification != 2L)
#' #plot(las, color = "Coplanar")
#'
#' # ==============
#' # Segment snags
#' # ==============
#'
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzi", filter="-keep_first") # Wing also included -keep_single
#'
#' # For the Wing2015 method, supply a matrix of snag BranchBolePtRatio conditional
#' # assessment thresholds (see Wing et al. 2015, Table 2, pg. 172)
#' bbpr_thresholds <- matrix(c(0.80, 0.80, 0.70,
#'                             0.85, 0.85, 0.60,
#'                             0.80, 0.80, 0.60,
#'                             0.90, 0.90, 0.55),
#'                             nrow =3, ncol = 4)
#'
#' # Run snag classification and assign classes to each point
#' las <- segment_snags(las, wing2015(neigh_radii = c(1.5, 1, 2), BBPRthrsh_mat = bbpr_thresholds))
#'
#' # Plot it all, tree and snag points...
#' plot(las, color="snagCls", colorPalette = rainbow(5))
#'
#' # Filter and plot snag points only
#' snags <- filter_poi(las, snagCls > 0)
#' plot(snags, color="snagCls", colorPalette = rainbow(5)[-1])
#'
#' # Wing et al's (2015) methods ended with performing tree segmentation on the
#' # classified and filtered point cloud using the watershed method
#' }
NULL
