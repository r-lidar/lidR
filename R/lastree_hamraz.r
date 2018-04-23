#' Individual tree segmentation
#'
#'
#' @param las A LAS object
#' @param nps nominal point spacing (page 533 in "2.Tree segmentation approach")
#' @param th minimal tree height to take into account (page 534)
#' @param R maximum horizontal distance of vertical profiles (page 535)
#' @param SENSITIVITY for inter tree gap identification ( multiplied by interquartile range from the third quartile) (p535)
#' @param MDCW minimum detectable crown width (page 534)
#' @param epsilon small deviation from vertical (page 535)
#' @param CLc crown ratio of a narrow cone-shaped crown (page 535)
#' @param Oc crown radius reduction due to the overlap assuming the narrow cone-shaped tree is situated in a dense stand(page 535)
#' @param CLs crown ratio of a sphere-shaped crown (page 535)
#' @param Os crown radius reduction due to the overlap within a dense stand for the sphere- shaped tree (page 535)
#' @param angleRefCone (page 536 - first sentence)
#' @param angleRefSphere (page 536 - first sentence)
#'
#' @return
#' A LAS object
#' @export
#'
#' @examples
#'LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#'las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'col = pastel.colors(200)
#'
#'tree = lastrees_hamraz(las)
#'
#'plot(tree, color = "treeID", colorPalette = pastel.colors(200))
lastrees_hamraz = function(las, nps = 0.25, th = 5, R = 15.24, SENSITIVITY = 6, MDCW = 1.5,
                            epsilon = 5, CLc = 0.8, Oc = 2/3, CLs = 0.7, Os = 1/3,
                            angleRefCone = 90, angleRefSphere = 32.7 )
  {
  #TODO: find limit value + addition into argument list
  minimalNumberOfPointsForTree = 100

  las@data$pointID = 1:nrow(las@data)

  #1 - Extraction of points with maximum Z for each grid cell
  LSP = lasfiltersurfacepoints(las, nps)

  #2 - Suppression of points under th
  cloud = LSP@data[Z > th, .(X, Y, Z, pointID)]
  cloudRef = LSP@data[Z > th, .(X, Y, Z, pointID)]     #data copy for final return output

  #3 - Application of a gaussian filter to remove low variations in vegetation
  #TODO

  idTree = 0L               # current tree ID
  npts = nrow(cloud)
  treeID = rep(NA, npts)    # tree ID of each point
  cloud[,number:=(1:npts)]  # Addition of point numbering for idTree attribution

  #Settings for progressBar (1/2)
  npoints = nrow(cloud)
  pbar =  utils::txtProgressBar(0, npoints)

  while ( npts != 0 )
  {
    utils::setTxtProgressBar(pbar, npoints - npts)      #Setting for progressBar (2/2)

    idTree <- idTree + 1

    #Treatment - segmentation of LSP into trees aeras
    #1 - Research of Z max - highest apex of tree in data
    Pmax = find_global_maxima(cloud)

    #2 - Extraction of circular aera around
    disc = get_points_in_disc(cloud, Pmax, R)

    #3 - Definition of profiles and delimitation of gaps/boundaries
    #centerRef = c(Pmax$X, Pmax$Y, Pmax$Z, Pmax$pointID)
    center = c(Pmax$X, Pmax$Y, Pmax$Z, Pmax$pointID)
    id_tree_points = find_tree_polygon_vec ( disc, disc@header@PHB$`Number of point records`, nps, SENSITIVITY, MDCW, epsilon, CLc, CLs, Oc, Os, angleRefCone, angleRefSphere, center, R )

    #Storage of extracted subset from disc
    extractPoints <- disc@data[disc@data$pointID %in% id_tree_points]

    #Subset verification: sufficient number of points? diameter above MDCW?
    limMDCW <-  max(sqrt( (extractPoints$X - Pmax$X)^2 + (extractPoints$Y - Pmax$Y)^2))

    if ( length(id_tree_points) > minimalNumberOfPointsForTree & limMDCW > MDCW )
    {
      treeID[cloud$number[cloud$pointID %in% id_tree_points]] = idTree
    } else {
      treeID[cloud$number[cloud$pointID %in% id_tree_points]] = NA
    }

    #Delete polygon points of intial data
    cloud = cloud[ cloud$pointID %in% id_tree_points == FALSE ]
    npts = nrow(cloud)
  }

  return(LAS(cloudRef[,idTree := treeID], las@header))
}




#----------------------------------------------------------------------------------------#
# Search for maximal Z-value
# This function returns the maximal point of a given point cloud regarding its Z-value.
# @param cloud Data from LAS object
#
# @return A line of this data corresponding to a point
# @export
#
# @examples
find_global_maxima = function( cloud )
{
  i = which.max(cloud$Z)
  center = cloud[i]
  return ( center )
}




#----------------------------------------------------------------------------------------#
# Extraction around specific point
# This function extracts all points of a given point cloud located at a specific distance from a considered point
# @param points Data from LAS object
# @param center Point of this data
# @param radius Distance for extraction limit
#
# @return
# @export
#
# @examples
get_points_in_disc = function( points, center, radius )
{
  points[, distToMax := abs( sqrt( (points$X-center$X)^2 + (points$Y-center$Y)^2))]
  surroundingPoints = points[ points$distToMax<=radius ]
  points[, distToMax := NULL]
  return(LAS(surroundingPoints, las@header))
}


