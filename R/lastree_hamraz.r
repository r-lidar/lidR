#' Individual tree segmentation
#'
#' It is an strict implementation of the Hamraz et al. 2012 (see references) algorithm made by the \code{lidR}
#' author from the original paper. The classification is done at the point cloud level and the function
#' returns nothing (NULL). The original point cloud is updated in place with an ID for each point in
#' a new column \code{treeID}. The user is free to post-process this output the way he want.
#'
#' @param las A LAS object
#' @param nps numeric. Nominal point spacing (see reference page 533  section 2)
#' @param th numeric. Minimal height. Point below this threshold are removed for the segmentation (see reference page 534  section 2)
#' @param MDCW numeric. Minimum detectable crown width (page 534 section 2)
#' @param epsilon numeric. Small deviation from vertical (page 535 section 2.2.2)
#' @param CLc numeric. Crown ratio of a narrow cone-shaped crown (page 535 equation 3)
#' @param Oc numeric. Crown radius reduction due to the overlap assuming the narrow cone-shaped tree is situated in a dense stand(page 535 equation 3)
#' @param CLs numeric. Crown ratio of a sphere-shaped crown (page 535 equation 4)
#' @param Os numeric. Crown radius reduction due to the overlap within a dense stand for the sphere-shaped tree (page 535 equation 4)
#' @param R numeric Maximum horizontal distance of vertical profiles (page 535). Any value greater
#' than a crown is good because this parameter does not affect the result. However, it greatly affects the
#' computation speed. The lower the value, the faster the method.
#'
#' @return
#' Nothing (NULL)
#' @export
#' @examples
#'LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#'las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'col = pastel.colors(200)
#'
#'tree = lastrees_hamraz(las, 0.25)
#'
#'plot(tree, color = "treeID", colorPalette = pastel.colors(200))
lastrees_hamraz = function(las, nps = 0.25, th = 5, MDCW = 1.5, epsilon = 5, CLc = 0.8, Oc = 2/3, CLs = 0.7, Os = 1/3, gap_sensitivity = 6, R = 15.24)
{
  # Preprocess : LSP + remove low point + smooth
  LSP = LAS(las@data[, .(X,Y,Z)])
  LSP = lasfiltersurfacepoints(LSP, nps)
  LSP = lasfilter(LSP, Z > 5)
  LSP@data[, Z := C_lassmooth(LSP, 0.6)]

  idTree = 0L
  treeID = rep(NA_integer_, nrow(las@data))

  npts = nrow(LSP@data)
  npoints = nrow(LSP@data)
  pbar =  utils::txtProgressBar(0, npoints)

  while (npts != 0)
  {
    utils::setTxtProgressBar(pbar, npoints - npts)      # Setting for progressBar (2/2)

    Pmax   = find_global_maxima(LSP@data)
    disc   = get_points_in_disc(LSP@data, Pmax, R)
    center = c(Pmax$X, Pmax$Y, Pmax$Z)

    l = find_tree_polygon_vec2(disc, nps, gap_sensitivity, MDCW, epsilon, CLc, CLs, Oc, Os, center, R)
    p = l$polygon
    p = data.frame(l$polygon)
    p = p[p$X4 > 1.5,]
    q = quantile(p$X4, probs = c(0.1, 0.9))
    p = p[p$X4 < q[2] & p$X4 > q[1],]
    p = as.matrix(p)
    p = rbind(p, center)
    p = convex_hull(p[,1], p[,2])

    plot(disc@data$X, disc@data$Y, col = lidR:::set.colors(disc@data$Z, height.colors(50)), asp = 1)

    x = numeric(64)
    y = numeric(64)
    a = numeric(64)

    for(i in 1:64)
    {
      x[i] = Pmax$X + l$profile[[i]]$extremityPoint[5] * cos(l$profile[[i]]$angle*pi/180)
      y[i] = Pmax$Y + l$profile[[i]]$extremityPoint[5] * sin(l$profile[[i]]$angle*pi/180)
      a[i] = l$profile[[i]]$angle
      points(l$profile[[i]]$extremityPoint[1], l$profile[[i]]$extremityPoint[2], col = "red", pch = 19)
    }

    lines(l$polygon[,1 ], l$polygon[,2], col = "red")
    lines(p$x, p$y)

    if(nrow(p) <= 2)
    {
      in_p = logical(nrow(LSP@data))
      in_p[Pmax$i] = TRUE
    } else {
      in_p = C_points_in_polygon(p$x, p$y, LSP@data$X, LSP@data$Y)

      if (sum(in_p) == 0)
      {
        in_p[Pmax$i] = TRUE
      }
    }


    tree = LSP@data[in_p]
    LSP = lasfilter(LSP, !in_p)

    #plot(LSP@data$X, LSP@data$Y, col = lidR:::set.colors(LSP@data$Z, height.colors(50)), asp = 1)

    if (!is.null(LSP))
    {
      npts = nrow(LSP@data)
    } else {
      npts = 0
    }

    p2 = convex_hull(tree$X, tree$Y)

    if (nrow(p2) > 4)
    {
      sp_p = sp::Polygon(p2)

      if (sp_p@area > pi*MDCW^2 | sum (in_p) < 25)
      {
        idTree <- idTree + 1

        las_in_p = C_points_in_polygon(p$x, p$y, las@data$X, las@data$Y)
        update = las_in_p & is.na(treeID)
        treeID[update] = idTree
      }
    }

    plot(las@data$X, las@data$Y, col = treeID, asp = 1)
  }

  lasadddata(las, treeID, "treeID")

  return(invisible())
}

# Search for maximal Z-value
#
# This function returns the maximal point of a given point cloud regarding its Z-value.
#
# @param cloud Data from LAS object
#
# @return A line of this data corresponding to a point
find_global_maxima = function( cloud )
{
  i = which.max(cloud$Z)
  center = cloud[i]
  center$i = i
  return(center)
}

# Extraction around specific point
#
# This function extracts all points of a given point cloud located at a specific distance from a considered point
#
# @param points Data from LAS object
# @param center Point of this data
# @param radius Distance for extraction limit
#
# @return A LAS object
get_points_in_disc = function(points, center, radius)
{
  points[, distToMax := abs( sqrt( (points$X-center$X)^2 + (points$Y-center$Y)^2))]
  surroundingPoints = points[ points$distToMax<=radius ]
  points[, distToMax := NULL]
  return(LAS(surroundingPoints))
}


