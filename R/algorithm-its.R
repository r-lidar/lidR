#' Individual Tree Segmentation Algorithm
#'
#' This function is made to be used in \link{segment_trees}. It implements an algorithm for tree
#' segmentation based on Dalponte and Coomes (2016) algorithm (see reference).
#' This is a seeds + growing region algorithm. This algorithm exists in the package \code{itcSegment}.
#' This version has been written from the paper in C++. Consequently it is hundreds to millions times
#' faster than the original version. Note that this algorithm strictly performs a segmentation, while the
#' original method as implemented in \code{itcSegment} and described in the manuscript also performs
#' pre- and post-processing tasks. Here these tasks are expected to be done by the user in separate functions.
#'
#' Because this algorithm works on a CHM only there is no actual need for a point cloud. Sometimes the
#' user does not even have the point cloud that generated the CHM. \code{lidR} is a point cloud-oriented
#' library, which is why this algorithm must be used in \link{segment_trees} to merge the result with the point
#' cloud. However the user can use this as a stand-alone function like this:
#' \preformatted{
#'  chm <- raster("chm.tif")
#'  ttops <- locate_trees(chm, lmf(3))
#'  crowns <- dalponte2016(chm, ttops)()
#' }
#'
#' @template param-chm-lastrees
#' @template param-treetops
#' @param th_tree numeric. Threshold below which a pixel cannot be a tree. Default is 2.
#' @param th_seed numeric. Growing threshold 1. See reference in Dalponte et al. 2016. A pixel
#' is added to a region if its height is greater than the tree height multiplied by this value.
#' It should be between 0 and 1. Default is 0.45.
#' @param th_cr numeric. Growing threshold 2. See reference in Dalponte et al. 2016. A pixel
#' is added to a region if its height is greater than the current mean height of the region
#' multiplied by this value. It should be between 0 and 1. Default is 0.55.
#' @param max_cr numeric. Maximum value of the crown diameter of a detected tree (in pixels).
#' Default is 10.
#' @param ID character. If `treetops` contains an attribute with the ID for
#' each tree, the name of this attribute. This way, original IDs will be preserved.
#'
#' @references
#' Dalponte, M. and Coomes, D. A. (2016), Tree-centric mapping of forest carbon density from
#' airborne laser scanning and hyperspectral data. Methods Ecol Evol, 7: 1236–1245. doi:10.1111/2041-210X.12575.
#'
#' @export
#'
#' @family individual tree segmentation algorithms
#' @family raster based tree segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' poi <- "-drop_z_below 0 -inside 481280 3812940 481320 3812980"
#' las <- readLAS(LASfile, select = "xyz", filter = poi)
#' col <- pastel.colors(200)
#'
#' chm <- rasterize_canopy(las, 0.5, p2r(0.3))
#' ker <- matrix(1,3,3)
#' chm <- terra::focal(chm, w = ker, fun = mean, na.rm = TRUE)
#'
#' ttops <- locate_trees(chm, lmf(4, 2))
#' las   <- segment_trees(las, dalponte2016(chm, ttops))
#' #plot(las, color = "treeID", colorPalette = col)
#' @name its_dalponte2016
#' @md
dalponte2016 = function(chm, treetops, th_tree = 2, th_seed = 0.45, th_cr = 0.55, max_cr = 10, ID = "treeID")
{
  assert_is_a_number(th_tree)
  assert_is_a_number(th_seed)
  assert_is_a_number(th_cr)
  assert_is_a_number(max_cr)
  assert_all_are_in_closed_range(th_seed, 0, 1)
  assert_all_are_in_closed_range(th_cr, 0, 1)
  stopifnot(raster_is_supported(chm))

  # Workaround for #622 If segment_tree is ran in parallel it will fail with
  # SpatRaster because they are not serializable. SpatRaster are converted to RasterLayer
  # for multicore strategies
  chm <- convert_ondisk_spatraster_into_serializable_raster_if_necessary(chm)

  treetops <- check_tree_tops(treetops, ID)
  chm      <- lazyeval::uq(chm)
  treetops <- lazyeval::uq(treetops)
  th_tree  <- lazyeval::uq(th_tree)
  th_seed  <- lazyeval::uq(th_seed)
  th_cr    <- lazyeval::uq(th_cr)
  max_cr   <- lazyeval::uq(max_cr)
  ID       <- lazyeval::uq(ID)

  f = function(bbox)
  {
    assert_is_valid_context(LIDRCONTEXTITS, "dalponte2016", null_allowed = TRUE)

    # If no seed it means we received an sf with 0 data
    if (nrow(treetops) == 0L)
    {
      crown <- chm
      crown <- raster_set_values(crown, NA_integer_)
      return(crown)
    }

    if (raster_is_proxy(chm) & missing(bbox))
      stop("Cannot segment the trees from a raster stored on disk. Use segment_trees() or load the raster in memory", call. = FALSE)

    # If a bbox is given we crop the CHM and the seed to this extent to reduce processing time
    # Otherwise we could get a chm much bigger than the LAS (e.g. LAScatalog processing) and
    # process would never ends
    res <- crop_special_its(treetops, chm, bbox)
    treetops <- res$treetops
    chm <- res$chm

    st_chm <- chm
    if (!is(chm, "stars"))
      st_chm <- stars::st_as_stars(chm)

    # If no seed, exit with with warning
    if (nrow(treetops) == 0L)
    {
      warning("No tree can be used as seed", call. = FALSE)
      crown <- chm
      crown <- raster_set_values(crown, NA_integer_)
      return(crown)
    }

    template <- stars::st_as_stars(st_chm)
    template[[1]][] <- 0L
    storage.mode(template[[1]]) <- "integer"

    treetops[["SEQUENTIALIDTREE"]] <- 1:nrow(treetops)
    rtreetops <- stars::st_rasterize(treetops["SEQUENTIALIDTREE"], template)
    storage.mode(rtreetops[[1]]) <- "integer"

    Maxima <- raster_as_matrix(rtreetops)$z
    Canopy <- raster_as_matrix(chm)$z

    Canopy[is.na(Canopy)] <- -Inf
    Crowns <- C_dalponte2016(Canopy, Maxima, th_seed, th_cr, th_tree, max_cr)

    Maxima[Maxima == 0L] <- NA_integer_
    Crowns[Crowns == 0L] <- NA_integer_
    Crowns[] <- treetops[[ID]][Crowns]
    val <- as.numeric(t(apply(Crowns, 1, rev)))
    storage.mode(val) <- storage.mode(treetops[[ID]])

    output <- chm
    output <- raster_set_values(output, NA_integer_)
    output <- raster_set_values(output, val)
    return(output)
  }

  class(f) <- c(LIDRALGORITHMITS, LIDRALGORITHMRASTERBASED)
  return(f)
}

#' Individual Tree Segmentation Algorithm
#'
#' This functions is made to be used in \link{segment_trees}. It implements an algorithm for tree
#' segmentation based on Silva et al. (2016) (see reference). This is a simple method
#' based on seed + voronoi tesselation (equivalent to nearest neighbour). This algorithm is implemented
#' in the package `rLiDAR`. This version is not the version from `rLiDAR`. It is
#' code written from the original article by the lidR authors and is considerably (between 250
#' and 1000 times) faster.
#'
#' Because this algorithm works on a CHM only there is no actual need for a point cloud. Sometimes the
#' user does not even have the point cloud that generated the CHM. `lidR` is a point cloud-oriented
#' library, which is why this algorithm must be used in \link{segment_trees} to merge the result into the point
#' cloud. However, the user can use this as a stand-alone function like this:
#' \preformatted{
#'  chm <- raster("chm.tif")
#'  ttops <- locate_trees(chm, lmf(3))
#'  crowns <- silva2016(chm, ttops)()
#' }
#'
#' @template param-chm-lastrees
#' @template param-treetops
#'
#' @param max_cr_factor numeric. Maximum value of a crown diameter given as a proportion of the
#' tree height. Default is 0.6, meaning 60% of the tree height.
#' @param exclusion numeric. For each tree, pixels with an elevation lower than `exclusion`
#' multiplied by the tree height will be removed. Thus, this number belongs between 0 and 1.
#' @param ID character. If `treetops` contains an attribute with the ID for
#' each tree, the name of this attribute. This way, original IDs will be preserved.
#'
#' @references
#' Silva, C. A., Hudak, A. T., Vierling, L. A., Loudermilk, E. L., O’Brien, J. J., Hiers,
#' J. K., Khosravipour, A. (2016). Imputation of Individual Longleaf Pine (Pinus palustris Mill.)
#' Tree Attributes from Field and LiDAR Data. Canadian Journal of Remote Sensing, 42(5), 554–573.
#' https://doi.org/10.1080/07038992.2016.1196582.
#'
#' @export
#'
#' @family individual tree segmentation algorithms
#' @family raster based tree segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' poi <- "-drop_z_below 0 -inside 481280 3812940 481320 3812980"
#' las <- readLAS(LASfile, select = "xyz", filter = poi)
#' col <- pastel.colors(200)
#'
#' # Using raster because focal does not exist in stars
#' chm <- rasterize_canopy(las, res = 0.5, p2r(0.3), pkg = "raster")
#' ker <- matrix(1,3,3)
#' chm <- raster::focal(chm, w = ker, fun = mean, na.rm = TRUE)
#'
#' ttops <- locate_trees(chm, lmf(4, 2))
#' las   <- segment_trees(las, silva2016(chm, ttops))
#' #plot(las, color = "treeID", colorPalette = col)
#' @name its_silva2016
#' @md
silva2016 = function(chm, treetops, max_cr_factor = 0.6, exclusion = 0.3, ID = "treeID")
{
  assert_is_a_number(max_cr_factor)
  assert_is_a_number(exclusion)
  assert_all_are_positive(max_cr_factor)
  assert_all_are_in_open_range(exclusion, 0, 1)

  # Workaround for #622 If segment_tree is ran in parallel it will fail with
  # SpatRaster because they are not serializable. SpatRaster are converted to RasterLayer
  # for multicore strategies
  chm <- convert_ondisk_spatraster_into_serializable_raster_if_necessary(chm)

  treetops       <- check_tree_tops(treetops, ID)
  chm            <- lazyeval::uq(chm)
  treetops       <- lazyeval::uq(treetops)
  max_cr_factor  <- lazyeval::uq(max_cr_factor)
  exclusion      <- lazyeval::uq(exclusion)
  ID             <- lazyeval::uq(ID)

  f = function(bbox)
  {
    assert_is_valid_context(LIDRCONTEXTITS, "silva2016", null_allowed = TRUE)

    # If no seed it means we received an sf with 0 data
    if (nrow(treetops) == 0L)
    {
      crown <- chm
      crown <- raster_set_values(crown, NA_integer_)
      return(crown)
    }

    if (raster_is_proxy(chm) & missing(bbox))
      stop("Cannot segment the trees from a raster stored on disk. Use segment_trees() or load the raster in memory", call. = FALSE)

    # If a bbox is given we crop the CHM and the seed to this extent to reduce processing time
    # Otherwise we could get a chm much bigger than the LAS (e.g. LAScatalog processing) and
    # process would never ends
    res <- crop_special_its(treetops, chm, bbox)
    treetops <- res$treetops
    chm <- res$chm

    st_chm <- chm
    if (!is(chm, "stars"))
      st_chm <- stars::st_as_stars(chm)

    # If no seed, exit with with warning
    if (nrow(treetops) == 0L)
    {
      warning("No tree can be used as seed", call. = FALSE)
      crown <- chm
      crown <- raster_set_values(crown, NA_integer_)
      return(crown)
    }

    chmdt <- raster_as_dataframe(chm, xy = FALSE, na.rm = TRUE)
    data.table::setDT(chmdt)

    ids <- treetops[[ID]]

    # Voronoi tesselation is nothing else but the nearest neigbour
    coords <- sf::st_coordinates(treetops)
    u <- C_knn(coords[,1], coords[,2], chmdt$X, chmdt$Y, 1L, getThread())

    id <- d <- hmax <- Z <- . <- X <- Y <- NULL
    chmdt[, id := u$nn.idx[,1]]
    chmdt[, id := ids[id]]
    chmdt[, d := u$nn.dist[,1]]
    chmdt[, hmax := max(Z), by = id]

    chmdt <- chmdt[Z >= exclusion*hmax & d <= max_cr_factor*hmax, .(X,Y, id)]

    crown <- chm
    crown <- raster_set_values(crown, NA_integer_)
    cells <- raster_cell_from_xy(crown, chmdt$X, chmdt$Y)
    crown <- raster_set_values(crown, chmdt[["id"]], cells)
    return(crown)
  }

  f <- plugin_its(f, omp = TRUE, raster_based = TRUE)
  return(f)
}

#' Individual Tree Segmentation Algorithm
#'
#' This function is made to be used in \link{segment_trees}. It implements an algorithm for tree
#' segmentation based on a watershed. It is based on the bioconductor package \code{EBIimage}. You
#' need to install this package to run this method (see its \href{https://github.com/aoles/EBImage}{github page}).
#' Internally, the function EBImage::watershed is called.
#'
#' Because this algorithm works on a CHM only there is no actual need for a point cloud. Sometimes the
#' user does not even have the point cloud that generated the CHM. \code{lidR} is a point cloud-oriented
#' library, which is why this algorithm must be used in \link{segment_trees} to merge the result into the point
#' cloud. However, the user can use this as a stand-alone function like this:
#' \preformatted{
#'  chm <- raster("chm.tif")
#'  crowns <- watershed(chm)()
#' }
#'
#' @template param-chm-lastrees
#' @param th_tree numeric. Threshold below which a pixel cannot be a tree. Default is 2.
#' @param tol numeric. Tolerance see ?EBImage::watershed.
#' @param ext numeric. see ?EBImage::watershed.
#'
#' @export
#'
#' @family individual tree segmentation algorithms
#' @family raster based tree segmentation algorithms
#'
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' poi <- "-drop_z_below 0 -inside 481280 3812940 481320 3812980"
#' las <- readLAS(LASfile, select = "xyz", filter = poi)
#' col <- pastel.colors(250)
#'
#' # Using raster because focal does not exist in stars
#' chm <- rasterize_canopy(las, res = 0.5, p2r(0.3), pkg = "raster")
#' ker <- matrix(1,3,3)
#' chm <- raster::focal(chm, w = ker, fun = mean, na.rm = TRUE)
#' las <- segment_trees(las, watershed(chm))
#'
#' plot(las, color = "treeID", colorPalette = col)
#' }
#' @md
#' @name its_watershed
watershed = function(chm, th_tree = 2, tol = 1, ext = 1)
{
  assert_is_a_number(th_tree)
  assert_is_a_number(tol)
  assert_is_a_number(ext)
  assert_package_is_installed("EBImage")

  # Workaround for #622 If segment_tree is ran in parallel it will fail with
  # SpatRaster because they are not serializable. SpatRaster are converted to RasterLayer
  # for multicore strategies
  chm <- convert_ondisk_spatraster_into_serializable_raster_if_necessary(chm)

  chm     <- lazyeval::uq(chm)
  th_tree <- lazyeval::uq(th_tree)
  tol     <- lazyeval::uq(tol)
  ext     <- lazyeval::uq(ext)

  f = function(bbox)
  {
    assert_is_valid_context(LIDRCONTEXTITS, "watershed", null_allowed = TRUE)

    if (raster_is_proxy(chm) & missing(bbox))
      stop("Cannot segment the trees from a raster stored on disk. Use segment_trees() or load the raster in memory", call. = FALSE)


    # If a bbox is given we crop the CHM and the seed to this extent to reduce processing time
    # Otherwise we could get a chm much bigger than the LAS (e.g. LAScatalog processing) and
    # process would never ends
    if (!missing(bbox)) chm <- raster_crop(chm, bbox)

    # Convert the CHM to a matrix
    Canopy <- raster_as_matrix(chm)$z
    mask   <- Canopy < th_tree | is.na(Canopy)
    Canopy[mask] <- 0
    Crowns <- EBImage::watershed(Canopy, tol, ext)

    Crowns[mask] <- NA_integer_

    val <- as.numeric(t(apply(Crowns, 1, rev)))
    storage.mode(val) <- "integer"

    output <- chm
    output <- raster_set_values(output, NA_integer_)
    output <- raster_set_values(output, val)
    return(output)
  }

  f <- plugin_its(f, omp = TRUE, raster_based = TRUE)
  return(f)
}
#' Individual Tree Segmentation Algorithm
#'
#' This functions is made to be used in \link{segment_trees}. It implements an algorithm for tree
#' segmentation based on Li et al. (2012) (see reference). This method is a growing region
#' method working at the point cloud level. It is an implementation by lidR authors, from the original
#' paper, as close as possible from the original description. However we added a parameter \code{hmin}
#' to prevent over-segmentation for objects that are too low. This algorithm is known to be slow because
#' it has an algorithmic complexity worst that O(n^2).
#'
#' @param dt1 numeric. Threshold number 1. See reference page 79 in Li et al. (2012). Default is 1.5.
#' @param dt2 numeric. Threshold number 2. See reference page 79 in Li et al. (2012). Default is 2.
#' @param R numeric. Search radius. See page 79 in Li et al. (2012). Default is 2. If \code{R = 0}
#' all the points are automatically considered as local maxima and the search step is skipped (much
#' faster).
#' @param hmin numeric. Minimum height of a detected tree. Default is 2.
#' @param Zu numeric. If point elevation is greater than Zu, \code{dt2} is used, otherwise \code{dt1} is
#' used. See page 79 in Li et al. (2012). Default is 15.
#' @param speed_up numeric. Maximum radius of a crown. Any value greater than a crown is
#' good because this parameter does not affect the result. However, it greatly affects the
#' computation speed by restricting the number of comparisons to perform.
#' The lower the value, the faster the method. Default is 10.
#'
#' @export
#'
#' @family individual tree segmentation algorithms
#' @family point-cloud based tree segmentation algorithms
#'
#' @references
#' Li, W., Guo, Q., Jakubowski, M. K., & Kelly, M. (2012). A new method for segmenting individual
#' trees from the lidar point cloud. Photogrammetric Engineering & Remote Sensing, 78(1), 75-84.
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' poi <- "-drop_z_below 0 -inside 481280 3812940 481320 3812980"
#' las <- readLAS(LASfile, select = "xyz", filter = poi)
#' col <- pastel.colors(200)
#'
#' las <- segment_trees(las, li2012(dt1 = 1.4))
#' #plot(las, color = "treeID", colorPalette = col)
#' @name its_li2012
#' @md
li2012 = function(dt1 = 1.5, dt2 = 2, R = 2, Zu = 15, hmin = 2, speed_up = 10)
{
  assert_is_a_number(dt1)
  assert_is_a_number(dt2)
  assert_is_a_number(R)
  assert_is_a_number(Zu)
  assert_is_a_number(hmin)
  assert_is_a_number(speed_up)
  assert_all_are_positive(dt1)
  assert_all_are_positive(dt2)
  assert_all_are_non_negative(R)
  assert_all_are_positive(Zu)
  assert_all_are_positive(hmin)
  assert_all_are_positive(speed_up)

  dt1      <- lazyeval::uq(dt1)
  dt2      <- lazyeval::uq(dt2)
  R        <- lazyeval::uq(R)
  Zu       <- lazyeval::uq(Zu)
  hmin     <- lazyeval::uq(hmin)
  speed_up <- lazyeval::uq(speed_up)

  f = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTITS, "li2012")

    if (las[["Max Z"]] < hmin)
    {
      warning("'hmin' is higher than the highest point. No tree segmented.", call. = FALSE)
      return(rep(NA_integer_, nrow(las@data)))
    }
    else
    {
      return(C_li2012(las, dt1, dt2, Zu, R, hmin, speed_up))
    }
  }

  f <- plugin_its(f, omp = FALSE, raster_based = FALSE)
  return(f)
}

check_tree_tops <- function(treetops, ID)
{
  if (inherits(treetops, "Spatial"))
    treetops <- sf::st_as_sf(treetops)

  if (!is(treetops, "sf") && !is(treetops, "sfc"))
    stop("treetops must be a spatial object", call. = FALSE)

  # Special case if 0 apex is given it must work and never fail
  if (is(sf::st_geometry(treetops), "sfc_GEOMETRY") && nrow(treetops) == 0L)
    return(treetops)

  if (!is(sf::st_geometry(treetops), "sfc_POINT"))
    stop("treetops must be a spatial object of points", call. = FALSE)

  if (is(treetops, 'sfc'))
  {
    n <- length(treetops)
    treetops <- sf::st_sf(treetops)
    treetops[[ID]] <- 1:n
  }

  if (is(treetops, "sf"))
  {
    ids <- treetops[[ID]]

    if (!is.numeric(ids) )
      stop("Tree IDs much be of a numeric type",  call. = FALSE)

    if (length(unique(ids)) < length(ids))
      stop("Duplicated tree IDs found.", call. = FALSE)
  }

  return(treetops)
}

crop_special_its <- function (treetops, chm, bbox)
{
  if (!missing(bbox))
  {
    assert_is_all_of(bbox, "bbox")
    chm <- raster_crop(chm, bbox)
    if (is(chm, "stars_proxy"))
      chm <- stars::st_as_stars(chm)
    sf::st_agr(treetops) <- "constant"
    treetops <- sf::st_crop(treetops, bbox)
  }
  else
  {
    sf::st_agr(treetops) <- "constant"
    bbox = sf::st_bbox(chm)
    sf::st_crs(bbox) <- sf::st_crs(chm)
    treetops <- sf::st_crop(treetops, bbox)
  }

  return(list(treetops = treetops, chm = chm))
}

