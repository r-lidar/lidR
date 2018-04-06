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

#' Predefined standard metrics functions
#'
#' Predefined functions usable in \link{grid_metrics}, \link{grid_hexametrics}, \link{lasmetrics}, \link{tree_metrics},
#' and their convenient shortcuts. The philosophy of the \code{lidR} package is to provide an easy way
#' to compute user-defined metrics rather than to provide them. However, for efficiency and to save time, a set of
#' standard metrics has been predefined. To use these functions please read the Details and Examples sections.
#'
#' The function names, their parameters and the output names of the metrics rely on a nomenclature chosen for brevity:
#' \itemize{
#' \item{\code{z}: refers to the elevation}
#' \item{\code{i}: refers to the intensity}
#' \item{\code{rn}: refers to the return number}
#' \item{\code{q}: refers to quantile}
#' \item{\code{a}: refers to the ScanAngle}
#' \item{\code{n}: refers to a number (a count)}
#' \item{\code{p}: refers to a percentage}
#' }
#' For example the metric named \code{zq60} refers to the elevation, quantile, 60 i.e. the 60th percentile of elevations.
#' The metric \code{pground} refers to a percentage. It is the percentage of points classified as ground.
#' The function \code{stdmetric_i} refers to metrics of intensity. A description of each existing metric can be found
#' on the \href{https://github.com/Jean-Romain/lidR/wiki/stdmetrics}{lidR wiki page}.\cr\cr
#' Some functions have optional parameters. If these parameters are not provided the function
#' computes only a subset of existing metrics. For example \code{stdmetrics_i} requires the intensity
#' values, but if the elevation values are provided it can compute additional metrics such as cumulative
#' intensity at a given percentile of height.\cr\cr
#' Each function has a convenient associated variable. It is the name of the function, with a
#' dot before the name. This enables the function to be used without writing parameters. The cost
#' of such a feature is inflexibility. It corresponds to a predefined behaviour (see examples)\cr\cr
#' \code{stdtreemetrics} is a special function which works with \link{tree_metrics}. Actually
#' it won't fail with other functions but the output make sense more sense if computed at the
#' individual tree level.
#'
#' @param x,y,z,i,a Coordinates of the points, Intensity and ScanAngle
#' @param rn,class ReturnNumber, Classification
#' @param pulseID The number referencing each pulse
#' @param dz Layer thickness for metrics requiring this data, such as \link[lidR:entropy]{entropy}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile, select = "*")
#'
#' # All the predefined functions
#' lidar %>% grid_metrics(stdmetrics(X,Y,Z,Intensity, ScanAngle,
#'                                   ReturnNumber, Classification,
#'                                   dz = 1))
#'
#' # Convenient shortcut
#' lidar %>% grid_metrics(.stdmetrics)
#'
#' # Basic metrics from intensities
#' lidar %>% grid_metrics(stdmetrics_i(Intensity))
#'
#' # All the metrics from intensities
#' lidar %>% grid_metrics(stdmetrics_i(Intensity, Z, Classification, ReturnNumber))
#'
#' # Convenient shortcut for the previous example
#' lidar %>% grid_metrics(.stdmetrics_i)
#'
#' # Compute the metrics only on first return
#' lidar %>% lasfilterfirst %>% grid_metrics(.stdmetrics_z)
#'
#' # Compute the metrics with a threshold at 2 meters
#' lidar %>% lasfilter(Z > 2) %>% grid_metrics(.stdmetrics_z)
#'
#' # Works also with lasmetrics and grid_hexametrics
#' lidar %>% lasmetrics(.stdmetrics)
#' lidar %>% grid_hexametrics(.stdmetrics)
#'
#' # Combine some predefined function with your own new metrics
#' # Here convenient shortcuts are no longer usable.
#' myMetrics = function(z, i)
#' {
#'   metrics = list(
#'      zwimean = sum(z*i)/sum(i), # Mean elevation weighted by intensities
#'      zimean  = mean(z*i),       # Mean products of z by intensity
#'      zsqmean = sqrt(mean(z^2))  # Quadratic mean
#'    )
#'
#'   return( c(metrics, stdmetrics_z(z)) )
#' }
#'
#' lidar %>% grid_metrics(myMetrics(Z, Intensity))
#'
#' # You can write your own convenient shorcuts like this:
#' .myMetrics = expression(myMetrics(Z,Intensity))
#'
#' lidar %>% grid_metrics(.myMetrics)
#' @seealso
#' \link{grid_metrics}
#' \link{lasmetrics}
#' \link{grid_hexametrics}
#' \link{grid_metrics3d}
#' \link{tree_metrics}
#' @rdname stdmetrics
#' @export
stdmetrics = function(x, y, z, i, a, rn, class, dz = 1)
{
  C  = stdmetrics_ctrl(x, y, z, a)
  Z  = stdmetrics_z(z, dz)
  I  = stdmetrics_i(i, z, class, rn)
  RN = stdmetrics_rn(rn, class)
  #PU = stdmetrics_pulse(pulseID, rn)

  metrics = c(C, Z, I, RN)
  return(metrics)
}

#' @rdname stdmetrics
#' @export
.stdmetrics = expression(stdmetrics(X,Y,Z,Intensity, ScanAngle, ReturnNumber, Classification, dz = 1))

#' Gap fraction profile
#'
#' Computes the gap fraction profile using the method of Bouvier et al. (see reference)
#'
#' The function assesses the number of laser points that actually reached the layer
#' z+dz and those that passed through the layer [z, z+dz]. By definition the layer 0
#' will always return 0 because no returns pass through the ground. Therefore, the layer 0 is removed
#' from the returned results.
#'
#' @param z vector of positive z coordinates
#' @param dz numeric. The thickness of the layers used (height bin)
#' @param z0 numeric. The bottom limit of the profile
#' @return A data.frame containing the bin elevations (z) and the gap fraction for each bin (gf)
#' @examples
#' z = c(rnorm(1e4, 25, 6), rgamma(1e3, 1, 8)*6, rgamma(5e2, 5,5)*10)
#' z = z[z<45 & z>0]
#'
#' hist(z, n=50)
#'
#' gapFraction = gap_fraction_profile(z)
#'
#' plot(gapFraction, type="l", xlab="Elevation", ylab="Gap fraction")
#' @references Bouvier, M., Durrieu, S., Fournier, R. a, & Renaud, J. (2015).  Generalizing predictive models of forest inventory attributes using an area-based approach with airborne LiDAR data. Remote Sensing of Environment, 156, 322-334. http://doi.org/10.1016/j.rse.2014.10.004
#' @seealso \link[lidR:LAD]{LAD}
#' @export gap_fraction_profile
gap_fraction_profile = function(z, dz = 1, z0 = 2)
{

  bk = seq(floor((min(z)-z0)/dz)*dz+z0, ceiling((max(z)-z0)/dz)*dz+z0, dz)

  histogram = graphics::hist(z, breaks = bk, plot = F)
  h = histogram$mids
  p = histogram$counts/sum(histogram$counts)

  p = c(p, 0)

  cs = cumsum(p)
  i = data.table::shift(cs) /cs
  i[is.na(i)] = 0

  i[is.nan(i)] = NA

  z = h #[-1]
  i = i[-length(i)] #[-c(1, length(i))]

  return(data.frame(z = z[z>z0], gf = i[z>z0]))
}

#' Leaf area density
#'
#' Computes a leaf area density profile based on the method of Bouvier et al. (see reference)
#'
#' The function assesses the number of laser points that actually reached the layer
#' z+dz and those that passed through the layer [z, z+dz] (see \link[lidR:gap_fraction_profile]{gap_fraction_profile}).
#' Then it computes the log of this quantity and divides it by the extinction coefficient k as described in Bouvier
#' et al. By definition the layer 0 will always return infinity because no returns pass through
#' the ground. Therefore, the layer 0 is removed from the returned results.
#'
#' @param z vector of positive z coordinates
#' @param dz numeric. The thickness of the layers used (height bin)
#' @param k numeric. is the extinction coefficient
#' @param z0 numeric. The bottom limit of the profile
#' @return A data.frame containing the bin elevations (z) and leaf area density for each bin (lad)
#' @examples
#' z = c(rnorm(1e4, 25, 6), rgamma(1e3, 1, 8)*6, rgamma(5e2, 5,5)*10)
#' z = z[z<45 & z>0]
#'
#' lad = LAD(z)
#'
#' plot(lad, type="l", xlab="Elevation", ylab="Leaf area density")
#' @references Bouvier, M., Durrieu, S., Fournier, R. a, & Renaud, J. (2015).  Generalizing predictive models of forest inventory attributes using an area-based approach with airborne LiDAR data. Remote Sensing of Environment, 156, 322-334. http://doi.org/10.1016/j.rse.2014.10.004
#' @seealso \link[lidR:gap_fraction_profile]{gap_fraction_profile}
#' @export LAD
LAD = function(z, dz = 1, k = 0.5, z0 = 2) # (Bouvier et al. 2015)
{
	ld = gap_fraction_profile(z, dz, z0)

	if(anyNA(ld))
	  return(NA_real_)

	lad = ld$gf
	lad = -log(lad)/(k*dz)

	lad[is.infinite(lad)] = NA

	lad = lad

	return(data.frame(z = ld$z, lad))
}

#' Normalized Shannon diversity index
#'
#' A normalized Shannon vertical complexity index
#'
#' The Shannon diversity index is a measure for quantifying diversity and is
#' based on the number and frequency of species present. This index, developed by
#' Shannon and Weaver for use in information theory, was successfully transferred
#' to the description of species diversity in biological systems (Shannon 1948).
#' Here it is applied to quantify the diversity and the evenness of an elevational distribution
#' of LiDAR points. It makes bins between 0 and the maximum elevation.
#'
#' @param z vector of positive z coordinates
#' @param by numeric. The thickness of the layers used (height bin)
#' @param zmax numeric. Used to turn the function entropy to the function \link[lidR:VCI]{VCI}.
#' @seealso
#' \link[lidR:VCI]{VCI}
#' @examples
#' z = runif(10000, 0, 10)
#'
#' # expected to be close to 1. The highest diversity is given for a uniform distribution
#' entropy(z, by = 1)
#'
#'  z = runif(10000, 9, 10)
#'
#' # Must be 0. The lowest diversity is given for a unique possibility
#' entropy(z, by = 1)
#'
#' z = abs(rnorm(10000, 10, 1))
#'
#' # expected to be between 0 and 1.
#' entropy(z, by = 1)
#' @references
#' Pretzsch, H. (2008). Description and Analysis of Stand Structures. Springer Berlin Heidelberg. http://doi.org/10.1007/978-3-540-88307-4 (pages 279-280)
#' Shannon, Claude E. (1948), "A mathematical theory of communication," Bell System Tech. Journal 27, 379-423, 623-656.
#' @return A number between 0 and 1
#' @export entropy
entropy = function(z, by = 1, zmax = NULL)
{
  # Fixed entropy (van Ewijk et al. (2011)) or flexible entropy
  if(is.null(zmax))
	  zmax = max(z)

	# If zmax < 3 it is meaningless to compute entropy
	if(zmax < 2 * by)
	{
	  warning("entropy() was computed with 3 or less bins. The function returned NA.")
		return(NA_real_)
  }

  if(min(z) < 0)
  {
    warning("entropy() found negative values. The function returned NA.")
    return(NA_real_)
  }

	# Define the x meters bins from 0 to zmax (rounded to the next integer)
	bk = seq(0, ceiling(zmax/by)*by, by)

	# Compute the p for each bin
	hist = findInterval(z, bk)
	hist = fast_table(hist, length(bk)-1)
	hist = hist/sum(hist)

	# Remove bin where there are no points because of log(0)
	p    = hist[hist > 0]
	pref = rep(1/length(hist), length(hist))

	# normalized entropy
	S = - sum(p*log(p)) / -sum(pref*log(pref))

	return(S)
}

#' Vertical Complexity Index
#'
#' A fixed normalization of the entropy function (see references)
#' @param z vector of z coordinates
#' @param by numeric. The thickness of the layers used (height bin)
#' @param zmax numeric. Used to turn the function entropy to the function vci.
#' @return A number between 0 and 1
#' @seealso
#' \link[lidR:entropy]{entropy}
#' @examples
#' z = runif(10000, 0, 10)
#'

#' VCI(z, by = 1, zmax = 20)
#'
#' z = abs(rnorm(10000, 10, 1))
#'
#' # expected to be closer to 0.
#' VCI(z, by = 1, zmax = 20)
#' @references van Ewijk, K. Y., Treitz, P. M., & Scott, N. A. (2011). Characterizing Forest Succession in Central Ontario using LAS-derived Indices. Photogrammetric Engineering and Remote Sensing, 77(3), 261-269. Retrieved from <Go to ISI>://WOS:000288052100009
#' @export VCI
VCI = function(z, zmax, by = 1)
{
  z = z[z < zmax]

  return(entropy(z, by, zmax))
}

#' @rdname stdmetrics
#' @export
stdmetrics_z = function(z, dz = 1)
{
  zmax <- zmean <- zsd <- zcv <- zskew <- zkurt <- zentropy <- NULL

  n = length(z)
  zmax  = max(z)
  zmean = mean(z)

  probs = seq(0.05, 0.95, 0.05)
  zq 	  = as.list(stats::quantile(z, probs))
  names(zq) = paste("zq", probs*100, sep="")

  if(zmax <= 0)
  {
    d = rep(0, 9)
  }
  else
  {
    breaks = seq(0, zmax, zmax/10)
    d = findInterval(z, breaks)
    d = fast_table(d, 10)
    d = d / sum(d)*100
    d = cumsum(d)[1:9]
    d = as.list(d)
  }
  names(d) = paste0("zpcum", 1:9)

  metrics = list(
    zmax  = zmax,
    zmean = zmean,
    zsd   = stats::sd(z),
    zskew = (sum((z - zmean)^3)/n)/(sum((z - zmean)^2)/n)^(3/2),
    zkurt = n * sum((z - zmean)^4)/(sum((z - zmean)^2)^2),
    zentropy  = entropy(z, dz)
  )

  metrics = c(metrics, zq, d)

  return(metrics)
}

#' @rdname stdmetrics
#' @export
.stdmetrics_z = expression(stdmetrics_z(Z, dz =1))

#' @rdname stdmetrics
#' @export
stdmetrics_i = function(i, z = NULL, class = NULL, rn = NULL)
{
  itot <- imax <- imean <- isd <- icv <- iskew <- ikurt <- NULL
  icumzq10 <- icumzq30 <- icumzq50 <- icumzq70 <- icumzq90 <- NULL
  itot1st <- itot2sd <- itot3rd <- itot4th  <- itot5th <- NULL

  n = length(i)
  itot = sum(i)
  imean = mean(i)

  probs = seq(0.05, 0.95, 0.05)
  iq 	  = as.list(stats::quantile(i, probs))
  names(iq) = paste("iq", probs*100, sep="")

  metrics = list(
    itot = itot,
    imax  = max(i),
    imean = imean,
    isd   = stats::sd(i),
    iskew = (sum((i - imean)^3)/n)/(sum((i - imean)^2)/n)^(3/2),
    ikurt = n * sum((i - imean)^4)/(sum((i - imean)^2)^2)
  )

  if(!is.null(class))
  {
    metrics = c(metrics, list(ipground = sum(i[class == 2])/itot*100))
  }

  if(!is.null(z))
  {
    zq = stats::quantile(z, probs = seq(0.1, 0.9, 0.2))

    ipcum = list(
      ipcumzq10 = sum(i[z <= zq[1]])/itot*100,
      ipcumzq30 = sum(i[z <= zq[2]])/itot*100,
      ipcumzq50 = sum(i[z <= zq[3]])/itot*100,
      ipcumzq70 = sum(i[z <= zq[4]])/itot*100,
      ipcumzq90 = sum(i[z <= zq[5]])/itot*100
    )

    metrics = c(metrics, ipcum)
  }
}

#' @rdname stdmetrics
#' @export
.stdmetrics_i = expression(stdmetrics_i(Intensity, Z, Classification, ReturnNumber))

#' @rdname stdmetrics
#' @export
stdmetrics_rn = function(rn, class = NULL)
{
  nground <- NULL

  n = length(rn)

  prn = table(factor(rn, levels=c(1:5)))/n*100
  prn = as.list(prn)
  names(prn) = paste0("p", names(prn), "th")

  metrics = prn

  if(!is.null(class))
    metrics = c(metrics, list(pground = sum(class == 2)/n*100))

  return(metrics)
}

#' @rdname stdmetrics
#' @export
.stdmetrics_rn = expression(stdmetrics_rn(ReturnNumber, Classification))


#' @rdname stdmetrics
#' @export
stdmetrics_pulse = function(pulseID, rn)
{
  . <- nr <- NULL

  n = length(rn)

  dt = data.table::data.table(pulseID, rn)

  np_with_x_return = dt[, .(nr = max(rn)), by = pulseID][, .N, by = nr]
  data.table::setorder(np_with_x_return, nr)

  np = numeric(9)
  names(np) = paste0("ppulse", 1:9, "return")
  np[np_with_x_return$nr] = np_with_x_return$N/n*100
  np = as.list(np)

  return(np)
}

#' @rdname stdmetrics
#' @export
.stdmetrics_pulse = expression(stdmetrics_pulse(pulseID, ReturnNumber))

#' @rdname stdmetrics
#' @export
stdmetrics_ctrl = function(x, y, z, a)
{
  xmax = max(x)
  ymax = max(y)
  xmin = min(x)
  ymin = min(y)
  n    = length(z)
  angle = abs(a)
  area = (xmax - xmin)*(ymax - ymin)

  metrics = list(
    n    = n,
    area = area,
    angle = mean(angle)
  )

  return(metrics)
}

#' @rdname stdmetrics
#' @export
.stdmetrics_ctrl = expression(stdmetrics_ctrl(X, Y, Z, ScanAngle))

#' @rdname stdmetrics
#' @export
stdtreemetrics = function(x, y, z)
{
  npoints = length(x)

  j = which.max(z)

  zmax = z[j]
  xmax = x[j]
  ymax = y[j]

  convhull.area = area(x,y)

  metrics = list(
    npoints = npoints,
    convhull_area = convhull.area,
    zmax.z = zmax,
    zmax.x = xmax,
    zmax.y = ymax
  )

  return(metrics)
}

#' @rdname stdmetrics
#' @export
.stdtreemetrics = expression(stdtreemetrics(X, Y, Z))

# canopy = canopyMatrix(x,y,z, canopyResolution)

# canopyq    = as.numeric(quantile(canopy, seq(0.1, 0.9, 0.2), na.rm=T))
#
# canopymean = mean(canopy, na.rm=T)
# canopysd   = sd(canopy, na.rm=T)
# canopycv   = canopysd/canopymean
#
# canopyq10  = canopyq[1]
# canopyq30  = canopyq[2]
# canopyq50  = canopyq[3]
# canopyq70  = canopyq[4]
# canopyq90  = canopyq[5]
#
# canopyfd   = fractal.dimension(canopy)
#
# perc.canopy.na = sum(is.na(canopy))/length(canopy) * 100
#
# CC2  = sum(canopy > 2, na.rm=T)/length(canopy)*100
# CC10 = sum(canopy > 10, na.rm=T)/length(canopy)*100
# CC20 = sum(canopy > 20, na.rm=T)/length(canopy)*100
# CC30 = sum(canopy > 30, na.rm=T)/length(canopy)*100
# CC40 = sum(canopy > 40, na.rm=T)/length(canopy)*100
