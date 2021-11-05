#' @param geom character. Geometry type of the output. Can be 'point', 'convex', 'concave' or 'bbox'.
#' @param concaveman numeric. Only if \code{type = "concave"}. Vector with the two parameters of the
#' function \link{concaveman}.
#' @param attribute character. The column name of the attribute containing tree IDs. Default is
#' \code{"treeID"}
#'
#' @rdname aggregate
#' @export
crown_metrics = function(las, func, geom = "point", concaveman = c(3, 0), attribute = "treeID", ...)
{
  UseMethod("crown_metrics", las)
}

#' @export
crown_metrics.LAS = function(las, func, geom = "point", concaveman = c(3, 0), attribute = "treeID", ...)
{
  assert_is_a_string(attribute)
  if (!attribute %in% names(las)) stop("The trees are not segmented yet. See function 'segment_trees'.", call. = FALSE)
  geom <- match.arg(geom, c("point", "convex", "concave", "bbox"))

  . <- .BY <- X <- Y <- Z <- NULL

  template <- las[[attribute]]

  if (all(is.na(template)))
  {
    warning("No tree found. NULL returned.", call. = FALSE)
    return(NULL)
  }

  if      (geom == "point")   fgeom <- stdtreeapex
  else if (geom == "convex")  fgeom <- stdtreehullconvex
  else if (geom == "concave") fgeom <- stdtreehullconcave
  else if (geom == "bbox")    fgeom <- stdtreehullbbox

  concavity <- concaveman[1]
  length_threshold <- concaveman[2]

  M1 <- las@data[, if (!anyNA(.BY)) fgeom(X,Y,Z, concavity, length_threshold), by = .(GRPID = template)]
  geom <- sf::st_as_sfc(M1[["geom"]])

  if (!is.null(func))
  {
    M2 <- template_metrics(las, func, template, ...)
    M2 <- M2[M2$GRPID %in% M1$GRPID]
    data.table::setnames(M2, "GRPID", attribute)
  }
  else
  {
    M2 <- M1[, "GRPID"]
    data.table::setnames(M2, attribute)
  }

  # This is only used for backward compatibility
  # with delineate_crowns() that call crown_metrics()
  xyz <- isTRUE(list(...)$xyz)
  if (xyz)
  {
    f <- function(x,y,z) {
      if (length(x) < 4) { return(NULL) }
      j <- which.max(z)
      return(list(XTOP= x[j], YTOP = y[j], ZTOP = z[j]))
    }

    M3 <- las@data[, if (!anyNA(.BY)) f(X,Y,Z), by = .(GRPID = template)]
    M3[["GRPID"]] <- NULL
    M2 <- cbind(M2, M3)
  }

  output <- sf::st_set_geometry(M2, geom)
  sf::st_crs(output) <- st_crs(las)
  return(output)
}

#' @export
crown_metrics.LAScluster = function(las, func, geom = "point", concaveman = c(3, 0), attribute = "treeID", ...)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  metrics <- crown_metrics(x, func, geom, concaveman, attribute, ...)

  bbox <- st_bbox(las)
  sf::st_agr(metrics) <- "constant"
  centroid <- sf::st_centroid(metrics)
  centroid$ID <- 1:nrow(centroid)
  sf::st_agr(centroid) <- "constant"
  centroid <- sf::st_crop(centroid, bbox)
  return(metrics[centroid$ID,])
}

#' @export
crown_metrics.LAScatalog = function(las, func, geom = "point", concaveman = c(3, 0), attribute = "treeID", ...)
{
  assert_is_a_string(attribute)

  is_formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!is_formula) func <- lazyeval::f_capture(func)
  globals <- future::getGlobalsAndPackages(func)

  options <- list(need_buffer = FALSE, drop_null = TRUE, globals = names(globals$globals), automerge = TRUE)
  output  <- catalog_apply(las, crown_metrics, func = func, geom = geom, concaveman = concaveman, attribute = attribute, ..., .options = options)
  return(output)
}
