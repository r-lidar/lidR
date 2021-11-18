fasterize = function(las, layout, buffer = 0, method = c("max", "min", "count"))
{
  methods <- c("max", "min", "count")
  method <- match.arg(method, methods)
  id <- which(method == methods)
  layout <- raster_template(layout)
  C_rasterize(las, layout, buffer, id)
}

rasterize_fast = function(las, res, buffer = 0, method = c("max", "min", "count"), pkg = getOption("lidR.raster.default"))
{
  layout <- raster_layout(las, res, buffer = buffer)
  val    <- fasterize(las, layout, buffer, method)
  layout <- raster_materialize(layout, pkg)
  layout <- raster_set_values(layout, val)
  return(layout)
}
