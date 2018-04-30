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
#'tree = lastrees_hamraz(las, 0.25)
#'
#'plot(tree, color = "treeID", colorPalette = pastel.colors(200))
lastrees_hamraz = function(las, nps = 0.25, th = 5, R = 15.24, SENSITIVITY = 6, MDCW = 1.5, epsilon = 5, CLc = 0.8, Oc = 2/3, CLs = 0.7, Os = 1/3, angleRefCone = 90, angleRefSphere = 32.7 )
{
  # Todo: find limit value + addition into argument list
  minimalNumberOfPointsForTree = 100

  las@data$pointID = 1:nrow(las@data)

  # 1 - Extraction of points with maximum Z for each grid cell
  LSP = lasfiltersurfacepoints(las, nps)
  LSP@data[, Z := C_lassmooth(LSP, 0.6)]

  # 2 - Suppression of points under th
  cloud = LSP@data[Z > th, .(X, Y, Z, pointID)]
  cloudRef = LSP@data[Z > th, .(X, Y, Z, pointID)]     #data copy for final return output

  # 3 - Application of a gaussian filter to remove low variations in vegetation
  # Todo

  idTree = 0L                   # current tree ID
  npts = nrow(cloud)
  treeID = rep(NA, npts)        # tree ID of each point
  cloud[, number := (1:npts)]   # Addition of point numbering for idTree attribution

  #Settings for progressBar (1/2)
  npoints = nrow(cloud)
  pbar =  utils::txtProgressBar(0, npoints)

  while (npts != 0)
  {
    utils::setTxtProgressBar(pbar, npoints - npts)      # Setting for progressBar (2/2)

    idTree <- idTree + 1

    # Treatment - segmentation of LSP into trees aeras
    # 1 - Research of Z max - highest apex of tree in data

    # JR: Si le nuage de point avait été trié sur Z dès le départ on aurait pas eu besoin de ca. Le
    # point 1 aurait été le maximum comme dans Li et al. On aurait sauté une étable O(n) de recherche
    # sequentielle.
    Pmax = find_global_maxima(cloud)

    # 2 - Extraction of circular aera around

    # JR: cette fonction prend un tableau en entrée et sort un objet LAS. Pourquoi?
    # Un LAS en entrée un LAS en sortie ou un tableau en entrée un tableau en sortie.
    # Ici comme la fonction n'est pas logique il y a une erreur: la variable 'las' n'est pas
    # définie dans cette fonction (voir fonction)

    disc = get_points_in_disc(cloud, Pmax, R)

    # 3 - Definition of profiles and delimitation of gaps/boundaries

    # JR: Je n'aime pas ca. pointID est un int. Les autres des double. En mettant tout dans un uniqe vecteur
    # on transtype l'entier en decimal. Si ca n'impacte pas forcément le résultat ca reste pas élégant
    # et est symptomatique d'une design à améliorer
    center = c(Pmax$X, Pmax$Y, Pmax$Z, Pmax$pointID)

    # JR: On a pas besoin de passer le nombre de points en paramètre. Ce nombre de points est nécéssairement
    # connu puisqu'on passe les données à la fonction
    n = disc@header@PHB$`Number of point records`
    list_id_tree_points = find_tree_polygon_vec2(disc, nps, SENSITIVITY, MDCW, epsilon, CLc, CLs, Oc, Os, angleRefCone, angleRefSphere, center, R)


    # Storage of extracted subset from disc
    extractPoints <- disc@data[pointID %in% id_tree_points]

    # JR: Avec ce plot je visualise le résulat, visiblement améliorable.
    # plot(disc@data$X, disc@data$Y, asp = 1)
    # points(extractPoints$X, extractPoints$Y, col = "red")

    # Subset verification: sufficient number of points? diameter above MDCW?
    limMDCW <-max(sqrt( (extractPoints$X - Pmax$X)^2 + (extractPoints$Y - Pmax$Y)^2))

    # JR: L'approche par %in% qui est une serie de recherche sequentielle est très très inneficace.
    # On a des nombres on doit trouver un moyen de les utiliser tel quel.
    # Ok j'ai compris, en supprimant le pointID on doit pouvoir faire ca plus simple

    if ( length(id_tree_points) > minimalNumberOfPointsForTree & limMDCW > MDCW )
      treeID[cloud$number[cloud$pointID %in% id_tree_points]] = idTree

    # Delete polygon points of intial data
    cloud = cloud[ cloud$pointID %in% id_tree_points == FALSE ]
    npts = nrow(cloud)
  }

  # JR: J'aurais voulu que 'find_tree_polygon_vec' nous sorte un polygon idéalement. Une fois la
  # boucle while terminée on aurait eu une liste de polygon et on aurait non pas updaté les LSP (cloud ref)
  # mais les données d'origine (las) et on aurait sorti en plus un shapefile des couronnes.

  return(LAS(cloudRef[, treeID := treeID], las@header))
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
  return(LAS(surroundingPoints, las@header))
}


