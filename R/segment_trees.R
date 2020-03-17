#' Individual tree segmentation
#'
#' Individual tree segmentation with several possible algorithms. The returned point cloud has a new
#' extra byte attribute named after the parameter `attribute` independently of the algorithm used.
#'
#' @template param-las
#' @param algorithm function. An algorithm of individual tree segmentation. `lidR` has:
#' \link{dalponte2016}, \link{watershed}, \link{li2012} and \link{silva2016}.
#' More experimental algorithms may be found in the package [lidRplugins](https://github.com/Jean-Romain/lidRplugins).
#' @param attribute character. The returned LAS object as a new extra byte attribute (in a new column).
#' This parameter controls the name of the new attribute. Default is `"treeID"`.
#' @param uniqueness character. A method to compute a unique ID. Can be 'incremental', 'gpstime' or
#' 'bitmerge'. See section 'Uniqueness'. This feature must be considered as 'experimental'.
#'
#' @section Uniqueness:
#'
#' By default the trees IDs are numbered from 1 to n, n being the number of tree found. The problem
#' with such incremental numbering is that, while it ensure to get a unique ID for each tree in
#' a given point-cloud it also gurantees to have duplicated tree IDs in different tiles or chunks when
#' processing a `LAScatalog` because each file is processed independently of the others and potentially
#' in parallel on different computers. Thus the index always restart to 1 on each file or chunk. Worst,
#' a tree that belongs exactly between 2 files will have two different IDs for its two halfs.
#'
#' This is why `segment_trees()` was not able to process a `LAScatalog` until v3.0.0. In v3.0.0 we
#' introduced some uniqueness strategies that are all imperfect and that should be seen as experimental.
#' Please report any troubleshooting. Using a uniqueness-safe strategy it ensures that trees from
#' different files will not share the same IDs. Moreover it ensure that two halfs of a trees on the
#' edge of a processing chunk will be attributed with the same ID.
#'
#' \describe{
#' \item{incremental}{Number from 0 to n. This methods **does not** ensure unicity of the IDs. This
#' is the legacy method.}
#' \item{gpstime}{This method uses the gpstime of the highest point of a tree (apex) to create a
#' unique ID. This ID is not a integer but a 64 bits decimal number which is suboptimal but at
#' least it is exepected to be unique  **if the gpstime attribute is consistant accross files**.}
#' If inconsistancies with gpstime are reported (for example gpstime records the week time and was
#' reset to 0 in a coverage that takes more than a week to do) there is a (low) probability to get
#' IDs attribution errors.
#' \item{bitmerge}{This method uses the XY coordinates of the highest point of a tree (apex) to
#' create a single number with a bitwise operation. First XY are converted back to integers using the
#' scales and offsets of point-cloud. Then the ID is computed with X * 2^32 + Y
#' to combine twice 32 bits of information into a 64 bits number. For example if the apex is at (10.32, 25.64)
#' with a scale factor of 0.01 and an offset of 0 the integer coordinates are X = 1032 and Y = 2564
#' and the ID is 4432406252036. Such methods returns a 64 bits integer but because 64 bits integer do
#' not exist in R it is converted to 64 bits decimal number that is guaranteed to be unique
#' **if all files have the same offsets and scale factors**.}
#' }
#'
#' All the proposed options are suboptimal because they either do not guarantee uniqueness in all cases
#' (inconsitancies in the collection of files) and they implies that IDs are based on non integers
#' meaningless numbers. But at least it should work in simple cases.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater
#'
#' @template return-lasupdater-las-lascatalog
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'
#' # Using Li et al. (2012)
#' las <- segment_trees(las, li2012(R = 3, speed_up = 5))
#' plot(las, color = "treeID")
#' @md
segment_trees = function(las, algorithm, attribute = "treeID", uniqueness = 'incremental')
{
  UseMethod("segment_trees", las)
}

#'@export
segment_trees.LAS = function(las, algorithm, attribute = "treeID", uniqueness = 'incremental')
{
  stopif_forbidden_name(attribute)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_its(algorithm)
  match.arg(uniqueness, c('incremental', 'gpstime', 'bitmerge'))
  lidR.context <- "segment_trees"

  if (uniqueness == 'gpstime' && !"gpstime" %in% names(las@data))
    stop("Impossible to compute unique IDs using gpstime: no gpstime found.", call. = FALSE)

  if (uniqueness == 'gpstime' &&  fast_countequal(las@data[["gpstime"]], 0L) == npoints(las))
    stop("Impossible to compute unique IDs using gpstime: gpstime is not populated.", call. = FALSE)

  if (is(algorithm, "RasterBased"))
    output <- algorithm()
  else if (is(algorithm, "PointCloudBased"))
    output <- algorithm(las)
  else
    stop("Invalid algorithm provided in segment_trees. The algorithm must have a class 'RasterBased' or 'PointCloudBased'")

  if (is(output, "RasterLayer"))
    las <- merge_spatial(las, output, attribute)
  else if (is.integer(output))
    las <- add_attribute(las, output, attribute)
  else
    stop(glue::glue("Wrong output type for the algorithm used. Expected 'RasterLayer' or 'integer', received {class(output)}"))

  if (uniqueness == 'incremental')
  {
    las <- add_lasattribute(las, name = attribute, desc = "An ID for each segmented tree")
    return(las)
  }

  tapex <- function(z,t) {
    j <- which.max(z)
    return(list(t.pos.t = t[j]))
  }

  xyapex <- function(x,y,z) {
    j <- which.max(z)
    return(list(x.pos.t = x[j], y.pos.t = y[j]))
  }

  X <- Y <- Z <- gpstime <- NULL
  ids <- las@data[[attribute]]

  if (uniqueness == 'gpstime')
    identifyers <- las@data[, if (!is.na(.BY)) tapex(Z, gpstime), by = ids]
  else if (uniqueness == 'bitmerge')
    identifyers <- las@data[, if (!is.na(.BY)) xyapex(X, Y, Z), by = ids]


  if (uniqueness == 'gpstime')
  {
    ids <- data.frame(ids)
    data.table::setDT(ids)
    matching <- identifyers[ids, on = 'ids']

    las@data[[attribute]] <- matching[["t.pos.t"]]

    las <- lasaddextrabytes_manual(las, name = attribute, desc = "An ID for each segmented tree", type = "double", NA_value = .Machine$double.xmin)
    return(las)
  }
  else if (uniqueness == 'bitmerge')
  {
    xoffset <- las@header@PHB[["X offset"]]
    yoffset <- las@header@PHB[["Y offset"]]
    zoffset <- las@header@PHB[["Z offset"]]

    xscale  <- las@header@PHB[["X scale factor"]]
    yscale  <- las@header@PHB[["Y scale factor"]]
    zscale  <- las@header@PHB[["Z scale factor"]]

    xscaled <- as.integer((identifyers[["x.pos.t"]] - xoffset)/xscale)
    yscaled <- as.integer((identifyers[["y.pos.t"]] - yoffset)/yscale)

    identifyers[["IDs"]] <- xscaled * 2^32 + yscaled

    identifyers <- identifyers[, c(1,4)]
    ids <- data.frame(ids)
    data.table::setDT(ids)
    matching <- identifyers[ids, on = 'ids']

    las@data[[attribute]] <- matching[["IDs"]]

    las <- add_lasattribute_manual(las, name = attribute, desc = "An ID for each segmented tree", type = "double", NA_value = .Machine$double.xmin)
    return(las)
  }
  else
    stop('Internal error: invalid uniqueness method', call. = FALSE) # nocov
}

#' @export
segment_trees.LAScluster = function(las, algorithm, attribute = "treeID", uniqueness = 'incremental')
{
  buffer <- NULL
  x <- suppressMessages(suppressWarnings(readLAS(las)))
  if (is.empty(x)) return(NULL)
  x <- segment_trees(x, algorithm, attribute, uniqueness)
  x <- filter_poi(x, buffer == 0)
  return(x)
}

#' @export
segment_trees.LAScatalog = function(las, algorithm, attribute = "treeID", uniqueness = 'incremental')
{
  # Defensive programming
  assert_is_algorithm(algorithm)
  assert_is_algorithm_its(algorithm)

  if (!uniqueness %in% c('gpstime', 'bitmerge'))
    stop("When processing a LAScatalog 'uniqueness' must be a method that garantees unicity of the IDs.", call. = FALSE)

  # Enforce some options
  opt_select(las) <- "*"

  # Processing
  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE, automerge = TRUE)
  output  <- catalog_apply(las, segment_trees, algorithm = algorithm, attribute = attribute, uniqueness = uniqueness, .options = options)
  return(output)
}
