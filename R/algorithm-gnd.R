#' Ground Segmentation Algorithm
#'
#' This function is made to be used in \link{classify_ground}. It implements an
#' algorithm for segmentation of ground points base on a Multiscale Curvature
#' Classification. This method is a strict implementation of the MCC algorithm
#' made by Evans and Hudak. (2007) (see references) that relies on the authors'
#' original source code written and exposed to R via the the \code{RMCC} package.
#'
#' There are two parameters that the user must define, the scale (s) parameter and
#' the curvature threshold (t). The optimal scale parameter is a function of
#' 1) the scale of the objects (e.g., trees) on the ground, and 2) the sampling
#' interval (post spacing) of the lidar data. Current lidar sensors are capable
#' of collecting high density data (e.g., 8 pulses/m2) that translate to a spatial
#' sampling frequency (post spacing) of 0.35 m/pulse (1/sqrt(8 pulses/m2) = 0.35 m/pulse),
#' which is small relative to the size of mature trees and will oversample larger
#' trees (i.e., nominally multiple returns/tree). Sparser lidar data (e.g., 0.25 pulses/m2)
#' translate to a post spacing of 2.0 m/pulse (1/sqrt(0.25 pulses/m2) = 2.0 m/pulse),
#' which would undersample trees and fail to sample some smaller trees (i.e.,
#' nominally <1 return/tree).\cr\cr
#' Therefore, a bit of trial and error is warranted to determine the best scale
#' and curvature parameters to use. Select a las tile containing a good variety
#' of canopy and terrain conditions, as it's likely the parameters that work best
#' there will be applicable to the rest of your project area tiles. As a starting
#' point: if the scale (post spacing) of the lidar survey is 1.5 m, then try 1.5.
#' Try varying it up or down by 0.5 m increments to see if it produces a more desirable
#' digital terrain model (DTM) interpolated from the classified ground returns in
#' the output file. Use units that match the units of the lidar data. For example,
#' if your lidar data were delivered in units of feet with a post spacing of 3 ft,
#' set the scale parameter to 3, then try varying it up or down by 1 ft increments
#' and compare the resulting interpolated DTMs. If the trees are large, then
#' it's likely that a scale parameter of 1 m (3 ft) will produce a cleaner DTM
#' than a scale parameter of 0.3 m (1 ft), even if the pulse density is 0.3 m
#' (1 ft). As for the curvature threshold, a good starting value to try might be
#' 0.3 (if data are in meters; 1 if data are in feet), and then try varying this
#' up or down by 0.1 m increments (if data are in meters; 0.3 if data are in feet).
#'
#' @param s numeric. Scale parameter. The optimal scale parameter is a function of
#' 1) the scale of the objects (e.g., trees) on the ground, and 2) the sampling
#' interval (post spacing) of the lidar data.
#' @param t numeric. Curvature threshold
#'
#' @references Evans, Jeffrey S.; Hudak, Andrew T. 2007. A multiscale curvature
#' algorithm for classifying discrete return LiDAR in forested environments.
#' IEEE Transactions on Geoscience and Remote Sensing. 45(4): 1029-1038.
#'
#' @family ground segmentation algorithms
#'
#' @examples
#' if (require(RMCC, quietly = TRUE))
#' {
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzrn", filter = "-inside 273450 5274350 273550 5274450")
#'
#' las <- classify_ground(las, mcc(1.5,0.3))
#' #plot(las, color = "Classification")
#' }
#' @export
#' @name gnd_mcc
mcc <- function(s = 1.5, t = 0.3)
{
  s <- lazyeval::uq(s)
  t <- lazyeval::uq(t)

  assert_is_a_number(s)
  assert_is_a_number(t)
  assert_package_is_installed("RMCC")

  f = function(las, filter)
  {
    . <- X <- Y <- Z <- idx <- NULL
    assert_is_valid_context(LIDRCONTEXTGND, "mcc")
    las@data[["idx"]] <- 1:npoints(las)
    cloud <- las@data
    if (length(filter) > 1) cloud <- cloud[filter, .(X,Y,Z, idx)]
    gnd <- RMCC::MCC(cloud, s, t)
    ids <- cloud$idx[gnd]
    return(ids)
  }

  f <- plugin_gnd(f)
  return(f)
}

#' Ground Segmentation Algorithm
#'
#' This function is made to be used in \link{classify_ground}. It implements an algorithm for segmentation
#' of ground points based on a progressive morphological filter. This method is an implementation of
#' the Zhang et al. (2003) algorithm (see reference). Note that this is not a strict implementation
#' of Zhang et al. This algorithm works at the point cloud level without any rasterization process.
#' The morphological operator is applied on the point cloud, not on a raster. Also, Zhang et al.
#' proposed some formulas (eq. 4, 5 and 7) to compute the sequence of windows sizes and thresholds.
#' Here, these parameters are free and specified by the user. The function \link{util_makeZhangParam}
#' enables computation of the parameters according to the original paper.
#'
#' The progressive morphological filter allows for any sequence of parameters. The `util_makeZhangParam`
#' function enables computation of the sequences using equations (4),
#'  (5) and (7) from Zhang et al. (see reference and details).
#' @details
#' In the original paper the windows size sequence is given by eq. 4 or 5:\cr\cr
#' \eqn{w_k = 2kb + 1} \cr\cr
#' or\cr\cr
#' \eqn{w_k = 2b^k + 1}\cr\cr
#' In the original paper the threshold sequence is given by eq. 7:\cr\cr
#' \eqn{th_k = s*(w_k - w_{k-1})*c + th_0}\cr\cr
#' Because the function \link{classify_ground} applies the morphological operation at the point
#' cloud level the parameter \eqn{c} is set to 1 and cannot be modified.
#'
#' @param b numeric. This is the parameter \eqn{b} in Zhang et al. (2003) (eq. 4 and 5).
#' @param max_ws numeric. Maximum window size to be used in filtering ground returns. This limits
#' the number of windows created.
#' @param dh0 numeric. This is \eqn{dh_0} in Zhang et al. (2003) (eq. 7).
#' @param dhmax numeric. This is \eqn{dh_{max}} in Zhang et al. (2003) (eq. 7).
#' @param s numeric. This is \eqn{s} in Zhang et al. (2003) (eq. 7).
#' @param exp logical. The window size can be increased linearly or exponentially (eq. 4 or 5).
#' @param ws numeric. Sequence of windows sizes to be used in filtering ground returns.
#' The values must be positive and in the same units as the point cloud (usually meters, occasionally
#' feet).
#' @param th numeric. Sequence of threshold heights above the parameterized ground surface to be
#' considered a ground return. The values must be positive and in the same units as the point cloud.
#'
#' @references
#' Zhang, K., Chen, S. C., Whitman, D., Shyu, M. L., Yan, J., & Zhang, C. (2003). A progressive
#' morphological filter for removing nonground measurements from airborne LIDAR data. IEEE
#' Transactions on Geoscience and Remote Sensing, 41(4 PART I), 872–882. http:#doi.org/10.1109/TGRS.2003.810682.
#'
#' @export
#'
#' @family ground segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzrn", filter = "-inside 273450 5274350 273550 5274450")
#'
#' ws <- seq(3,12, 3)
#' th <- seq(0.1, 1.5, length.out = length(ws))
#'
#' las <- classify_ground(las, pmf(ws, th))
#' #plot(las, color = "Classification")
#' @name gnd_pmf
pmf = function(ws, th)
{
  ws <- lazyeval::uq(ws)
  th <- lazyeval::uq(th)

  f = function(las, filter)
  {
    assert_is_valid_context(LIDRCONTEXTGND, "pmf")
    force_autoindex(las) <- LIDRGRIDPARTITION
    return(C_pmf(las, ws, th, filter))
  }

  f <- plugin_gnd(f)
  return(f)
}

#' Ground Segmentation Algorithm
#'
#' This function is made to be used in \link{classify_ground}. It implements an algorithm for segmentation
#' of ground points base on a Cloth Simulation Filter. This method is a strict implementation of
#' the CSF algorithm made by Zhang et al. (2016) (see references) that relies on the authors' original
#' source code written and exposed to R via the the \code{RCSF} package.
#'
#' @param sloop_smooth logical. When steep slopes exist, set this parameter to TRUE to reduce
#' errors during post-processing.
#' @param class_threshold scalar. The distance to the simulated cloth to classify a point cloud into ground
#' and non-ground. The default is 0.5.
#' @param cloth_resolution scalar. The distance between particles in the cloth. This is usually set to the
#' average distance of the points in the point cloud. The default value is 0.5.
#' @param rigidness integer. The rigidness of the cloth. 1 stands for very soft (to fit rugged
#' terrain), 2 stands for medium, and 3 stands for hard cloth (for flat terrain). The default is 1.
#' @param iterations integer. Maximum iterations for simulating cloth. The default value is 500. Usually,
#' there is no need to change this value.
#' @param time_step scalar. Time step when simulating the cloth under gravity. The default value
#' is 0.65. Usually, there is no need to change this value. It is suitable for most cases.
#'
#' @references
#' W. Zhang, J. Qi*, P. Wan, H. Wang, D. Xie, X. Wang, and G. Yan, “An Easy-to-Use Airborne LiDAR Data
#' Filtering Method Based on Cloth Simulation,” Remote Sens., vol. 8, no. 6, p. 501, 2016.
#' (http://www.mdpi.com/2072-4292/8/6/501/htm)
#'
#' @export
#'
#' @family ground segmentation algorithms
#'
#' @examples
#' if (require(RCSF, quietly = TRUE))
#' {
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzrn")
#'
#' mycsf <- csf(TRUE, 1, 1, time_step = 1)
#' las <- classify_ground(las, mycsf)
#' #plot(las, color = "Classification")
#' }
#' @name gnd_csf
csf = function(sloop_smooth = FALSE, class_threshold = 0.5, cloth_resolution = 0.5, rigidness = 1L, iterations = 500L, time_step = 0.65)
{
  sloop_smooth     <- lazyeval::uq(sloop_smooth)
  class_threshold  <- lazyeval::uq(class_threshold)
  cloth_resolution <- lazyeval::uq(cloth_resolution)
  rigidness        <- lazyeval::uq(rigidness)
  iterations       <- lazyeval::uq(iterations)
  time_step        <- lazyeval::uq(time_step)

  assert_is_a_bool(sloop_smooth)
  assert_is_a_number(class_threshold)
  assert_is_a_number(cloth_resolution)
  assert_is_a_number(rigidness)
  assert_is_a_number(iterations)
  assert_is_a_number(time_step)
  assert_package_is_installed("RCSF")

  f = function(las, filter)
  {
    . <- X <- Y <- Z <- NULL
    assert_is_valid_context(LIDRCONTEXTGND, "csf")

    las@data[["idx"]] <- 1:npoints(las)
    cloud <- las@data[filter, .(X,Y,Z, idx)]
    gnd <- RCSF::CSF(cloud, sloop_smooth, class_threshold, cloth_resolution, rigidness, iterations, time_step)
    idx <- cloud$idx[gnd]
    return(idx)
  }

  f <- plugin_gnd(f)
  return(f)
}

#' @export
#' @rdname gnd_pmf
util_makeZhangParam = function(b = 2, dh0 = 0.5, dhmax = 3.0, s = 1.0,  max_ws = 20, exp = FALSE)
{
  if (exp & b <= 1)
    stop("b cannot be less than 1 with an exponentially growing window")

  if (dh0 >= dhmax)
    stop("dh0 greater than dhmax")

  if (max_ws < 3)
    stop("Minimum windows size is 3. max_ws must be greater than 3")

  if (!is.logical(exp))
    stop("exp should be logical")

  if (!exp & b < 1)
    warning("Due to an incoherence in the original paper when b < 1, the sequences of windows size cannot be computed for a linear increase. The internal routine uses the fact that the increment is constant to bypass this issue.")


  dhtk = c()
  wk = c()
  k = 0
  ws = 0
  th = 0
  c = 1

  while (ws <= max_ws)
  {
    # Determine the initial window size.
    if (exp)
      ws = (2.0*b^k) + 1
    else
      ws = 2.0*(k + 1)*b + 1

    # Calculate the height threshold to be used in the next k.
    if (ws <= 3)
      th = dh0
    else
    {
      if (exp)
        th = s * (ws - wk[k]) * c + dh0
      else
        th = s*2*b*c + dh0
    }

    # Enforce max distance on height threshold
    if (th > dhmax)
      th = dhmax

    if (ws <= max_ws)
    {
      wk = append(wk, ws)
      dhtk = append(dhtk, th)
    }

    k = k + 1
  }

  return(list(ws = wk, th = dhtk))
}
