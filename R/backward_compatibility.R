#' Older R Spatial Packages
#'
#' `rgdal` and `rgeos` will be retired on Jan 1st 2024. The `raster` and `sp` packages are based on
#' `rgdal` and `rgeos`. `lidR` was based on `raster` and `sp` because it was created before `sf`, `terra`
#' and `stars` packages. This means that sooner or later users and packages that are still based on
#' old R spatial packages will run into trouble. According to Edzer Pebesma, Roger Bivand: \cr\cr
#' *R users who have been around a bit longer, in particular before packages like `sf` and `stars` were
#' developed, may be more familiar with older packages like `maptools`, `sp`, `rgeos`, and `rgdal`. A fair
#' question is whether they should migrate existing code and/or existing R packages depending on these
#' packages. The answer is: yes (see reference).*\cr\cr
#' lidR 4.0.0 no longer uses `sp` and `raster`. New functions are based on `sf` and `stars`.
#' However to maintain backward compatibility the old functions from v<4.0.0 were preserved. The old
#' functions are using the new functions and automatically cast the output to the old format. The old
#' functions are not formally deprecated but users should definitively move their workflow to modern
#' spatial package. lidR will maintain the old functions as long as it does not generate troubles
#' on CRAN. So, it might be until Jan 1st 2024 or later who knows...
#'
#' @param x,las an object of class LAS*
#' @param res,start see \link{pixel_metrics}
#' @param func see \link{template_metrics}
#' @param attribute,type see \link{crown_metrics}
#' @param ... ignored
#' @param algorithm see \link{rasterize_canopy}, \link{rasterize_terrain}
#' @param full_raster,use_class,Wdegenerated,is_concave,keep_lowest see \link{rasterize_density}
#' @param filter,by_echo see \link{template_metrics}
#' @param uniqueness see \link{crown_metrics}
#' @param concavity,length_threshold see \link{concaveman}
#' @references Edzer Pebesma, Roger Bivand Spatial Data Science with applications in R
#' https://keen-swartz-3146c4.netlify.app/older.html
#' @md
#' @name old_spatial_packages
#' @rdname old_spatial_packages
NULL

#' @export
#' @rdname old_spatial_packages
as.spatial = function(x) UseMethod("as.spatial", x)

#' @export
#' @rdname old_spatial_packages
as.spatial.LAS = function(x) { sp::SpatialPointsDataFrame(x@data[, 1:2], x@data[, 3:ncol(x@data)], proj4string = as(st_crs(x), "CRS")) }

#' @export
#' @rdname old_spatial_packages
as.spatial.LAScatalog = function(x)
{
  # Workaround to repair LAScatalog v3 and minimize backward incompatibilities with v4
  if (is_lascatalog_v3(x))
  {
    res <- new("SpatialPolygonsDataFrame")
    res@bbox <- x@bbox
    res@proj4string <- x@proj4string
    res@plotOrder <- x@plotOrder
    res@data <- x@data
    res@polygons <- x@polygons
    return(res)
  }

  return(sf::as_Spatial(x@data))
}

#' @export
#' @rdname old_spatial_packages
tree_metrics <- function(las, func = ~list(Z = max(Z)), attribute = "treeID", ...)
{
  res <- crown_metrics(las, func, geom = "point", attribute, ...)
  if (is(res, "sf")) res <- sf::as_Spatial(res)
  return(res)
}

#' @export
#' @rdname old_spatial_packages
grid_canopy = function(las, res, algorithm)
{
  rasterize_canopy(las, res, algorithm, pkg = "raster")
}

#' @export
#' @rdname old_spatial_packages
grid_density = function(las, res = 4)
{
  rasterize_density(las, res, pkg = "raster")
}

#' @export
#' @rdname old_spatial_packages
grid_terrain = function(las, res = 1, algorithm, ..., keep_lowest = FALSE, full_raster = FALSE, use_class = c(2L,9L), Wdegenerated = TRUE, is_concave = FALSE)
{
  shape <- "convex"
  if (full_raster) shape <- "bbox"
  if (is_concave) shape <- "concave"

  rasterize_terrain(las, res, algorithm, shape = shape, use_class = use_class, keep_lowest = keep_lowest, Wdegenerated = Wdegenerated, pkg = "raster")
}

#' @export
#' @rdname old_spatial_packages
grid_metrics = function(las, func, res = 20, start = c(0,0), filter = NULL, by_echo = "all")
{
  res <- raster_layout(las, res, start, format = "raster")
  res <- pixel_metrics(las, func, res, start, filter = filter, by_echo = by_echo)
  return(res)
}

#' @export
#' @rdname old_spatial_packages
find_trees = function(las, algorithm, uniqueness = 'incremental')
{
  res <- locate_trees(las, algorithm, uniqueness)
  if (is(res, "sf")) {
    res <- sf::st_zm(res)
    res <- sf::as_Spatial(res)
  }
  return(res)
}

#' @export
#' @rdname old_spatial_packages
delineate_crowns = function(las, type = c("convex", "concave", "bbox"), concavity = 3, length_threshold = 0, func = NULL, attribute = "treeID")
{
  type <- match.arg(type)
  res <- crown_metrics(las, func = func, geom = type, concaveman = c(concavity, length_threshold), attribute = attribute, xyz = TRUE)
  if (is(res, "sf")) res <- sf::as_Spatial(res)
  res
}

is_las_v3 <- function(x) { !methods::.hasSlot(x, "crs") }
las_v3_repair <- function(x) { if (is_las_v3(x)) return(LAS(x)) else return(x) }

is_lascatalog_v3 <- function(x) { !is(x@data, "sf") }
lascatalog_v3_repair <- function(x)
{
  if (is_lascatalog_v3(x))
  {
    y <- new("LAScatalog")
    y@data <- sf::st_as_sf(as.spatial(x))
    opt_copy(y) <- x
    return(y)
  }

  return(x)
}
