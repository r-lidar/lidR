#' Pits and spikes filling
#'
#' Pits and spikes filling for raster. Typically used for post-processing CHM. This algorithm
#' is from St-Onge 2008 (see reference).
#'
#' @param x raster. SpatRaster, RasterLayer, stars.
#' @param lap_size integer. Size of the Laplacian filter kernel (integer value, in pixels).
#' @param thr_lap numeric. Threshold Laplacian value for detecting a cavity (all values above this
#' value will be considered a cavity). A positive value.
#' @param thr_spk numeric. Threshold Laplacian value for detecting a spike (all values below this
#' value will be considered a spike). A negative value.
#' @param med_size integer. Size of the median filter kernel (integer value, in pixels).
#' @param dil_radius integer. Dilation radius (integer value, in pixels).
#'
#' @references
#' St-Onge, B., 2008. Methods for improving the quality of a true orthomosaic of Vexcel UltraCam
#' images created using alidar digital surface model, Proceedings of the Silvilaser 2008, Edinburgh,
#' 555-562. https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=81365288221f3ac34b51a82e2cfed8d58defb10e
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile)
#' chm <- rasterize_canopy(las, 0.5, dsmtin())
#' sto <- pitfill_stonge2008(chm)
#'
#' #terra::plot(c(chm, sto), col = lidR::height.colors(25))
#' @export
pitfill_stonge2008 = function(x, lap_size = 3L, thr_lap = 0.1, thr_spk = -0.1, med_size = 3L, dil_radius = 0L)
{
  res <- raster_res(x)
  z   <- raster_as_matrix(x)$z
  dim = dim(z)
  z = as.numeric(t(apply(z,1,rev)))
  z[is.na(z)] = -99999
  y = C_chm_prep(z, dim[2], dim[1], lap_size, thr_lap, thr_spk, med_size, dil_radius, -99999)
  y[y == -99999] = NA
  y = matrix(y, nrow = dim[1], ncol = dim[2])
  x[] = t(y)
  return(x)
}
