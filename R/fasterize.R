fasterize = function(las, layout, buffer = 0, method = c("max", "min", "count"))
{
  methods <- c("max", "min", "count")
  method <- match.arg(method, methods)
  id <- which(method == methods)
  layout <- raster_template(layout)
  C_rasterize(las, layout, buffer, id)
}
