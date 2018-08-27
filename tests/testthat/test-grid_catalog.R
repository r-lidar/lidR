context("grid_catalog")

#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' shapefile_dir <- system.file("extdata", package = "lidR")
#'
#' ctg = catalog(LASfile)
#' set_tiling_size(ctg) <- 160
#'
#' lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")
#'
#' my_grid_metrics = function(x, res, spdf)
#' {
#'   lasclassify(x, spdf, "inpoly")
#'   x = lasfilter(x, !inpoly)
#'   grid_metrics(x, mean(Z), res)
#' }
#'
#' mean = grid_catalog(ctg, my_grid_metrics, 20,
#'                     select = "xyz", filter = "-drop_z_below 5",
#'                     spdf = lakes)


