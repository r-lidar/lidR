#' Classify points of interest
#'
#' Classify points that meet some logical criterion and/or that belong in a region of interest. The
#' function updates the attribute \code{Classification} of the LAS object.
#'
#' @template param-las
#'
#' @param class The ASPRS class to attribute to the points that meet the criterion.
#' @param poi a formula of logical predicates. The points that are `TRUE` will be classified `class`.
#' @param roi A `SpatialPolygons`, `SpatialPolygonDataFrame` from `sp` or a `POLYGON` from `sf`.
#' The points that are in the region of interest delimited by the polygon(s) are classified
#' `class`.
#' @param inverse_roi bool. Inverses the `roi`. The points that are outside the polygon(s)
#' are classified `class`
#' @param by_reference bool. Updates the classification in place (LAS only).
#' @param ... Unused
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater-nobuffer
#'
#' @template return-lasupdater-las-lascatalog
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' shp     <- system.file("extdata", "lake_polygons_UTM17.shp", package = "lidR")
#'
#' las  <- readLAS(LASfile, filter = "-keep_random_fraction 0.1")
#' lake <- sf::st_read(shp, quiet = TRUE)
#'
#' # Classifies the points that are NOT in the lake and that are NOT ground points as class 5
#' poi <- ~Classification != LASGROUND
#' las <- classify_poi(las, LASHIGHVEGETATION, poi = poi, roi = lake, inverse = TRUE)
#'
#' # Classifies the points that are in the lake as class 9
#' las <- classify_poi(las, LASWATER, roi = lake, inverse = FALSE)
#'
#' #plot(las, color = "Classification")
#' @md
classify_poi = function(las, class, poi = NULL, roi = NULL, inverse_roi = FALSE, by_reference = FALSE, ...)
{
  UseMethod("classify_poi", las)
}

#' @export
classify_poi.LAS = function(las, class, poi = NULL, roi = NULL, inverse_roi = FALSE, by_reference = FALSE, ...)
{
  assert_is_a_number(class)
  assert_all_are_non_negative(class)
  assert_is_a_bool(inverse_roi)
  stopifnot(class == as.integer(class))

  Classification <- NULL

  bool1 <- rep(TRUE, npoints(las))
  if (!is.null(poi)) bool1 <- parse_filter(las, poi)

  bool2 <- rep(TRUE, npoints(las))
  if (!is.null(roi))
  {
    las <- merge_spatial(las, roi, "..inpoly..")
    bool2 <- las[["..inpoly.."]]
    las[["..inpoly.."]] <- NULL
    if (inverse_roi) bool2 <- !bool2
  }

  bool <- bool1 & bool2

  if (! "Classification" %in% names(las@data))
  {
    if (by_reference)
      las@data[, Classification := 0L]
    else
      las@data[["Classification"]] <- 0L
  }

  if (by_reference)
  {
    las@data[bool, Classification := class]
    return(invisible(las))
  }
  else
  {
    las$Classification[bool] <- class
    return(las)
  }
}

#' @export
classify_poi.LAScluster = function(las, class, poi = NULL, roi = NULL, inverse_roi = FALSE, by_reference = FALSE, ...)
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  x <- classify_poi(x, class, poi, roi, inverse_roi, by_reference = TRUE, ...)
  return(x)
}

#' @export
classify_poi.LAScatalog = function(las, class, poi = NULL, roi = NULL, inverse_roi = FALSE, by_reference = FALSE, ...)
{
  opt_select(las) <- "*"
  opt_chunk_buffer(las) <- 0

  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE, automerge = TRUE)
  output  <- catalog_apply(las, classify_poi, class = class, poi = poi, roi = roi, inverse_roi = inverse_roi, .options = options)
  return(output)
}
