st_make_bboxes = function(xmin, xmax, ymin, ymax, crs = sf::NA_crs_)
{
  bboxes <- mapply(c,  xmin, xmax, ymin, ymax, SIMPLIFY = FALSE)
  bboxes <- lapply(bboxes, function(x) { names(x) = c("xmin", "xmax", "ymin", "ymax") ; x})
  bboxes <- lapply(bboxes, sf::st_bbox)
  return(bboxes)
}

st_expand_bbox <- function(bbox, size)
{
   return(bbox + c(-size, -size, size, size))
}

st_adjust_bbox <- function(x, res, start = c(0,0), buffer = 0)
{
  bbox    <- sf::st_bbox(x)
  if (length(res) == 1) res <- c(res, res)
  bbox[1] <- round_any(bbox$xmin - buffer - 0.5 * res[1] - start[1], res[1]) + start[1]
  bbox[3] <- round_any(bbox$xmax + buffer - 0.5 * res[1] - start[1], res[1]) + res[1] + start[1]
  bbox[2] <- round_any(bbox$ymin - buffer - 0.5 * res[2] - start[2], res[2]) + start[2]
  bbox[4] <- round_any(bbox$ymax + buffer - 0.5 * res[2] - start[2], res[2]) + res[2] + start[2]
  return(bbox)
}

st_crop_if_not_similar_bbox = function(source, las)
{

  bbox1  <- st_bbox(las)
  bbox2  <- sf::st_bbox(source)
  width  <- (bbox1[3] - bbox1[1])
  height <- (bbox1[4] - bbox1[2])
  bbox   <- bbox1 + c(-width, -height, width, height)

  # If source bbox is within bbox las x 2 do not crop
  if (bbox2$xmin > bbox$xmin && bbox2$ymin > bbox$ymin && bbox2$xmax < bbox$xmax && bbox2$xmin < bbox$ymax)
  {
    return(source)
  }

  # crop a tiny bit  (1%) larger than the point cloud including lon/lat case
  bbox   <- st_bbox(las)
  width  <- (bbox[3] - bbox[1])*0.01 + las[["X scale factor"]]
  height <- (bbox[4] - bbox[2])*0.01 + las[["Y scale factor"]]
  bbox   <- bbox + c(-width, -height, width, height)
  sf::st_crs(bbox) <- sf::st_crs(source)
  if (is(source, "sf")) sf::st_agr(source) <- "constant"
  source <- sf::st_crop(source, bbox)
  return(source)
}

st_proj_is_meters <- function(obj)
{
  !is.na(sf::st_crs(obj)) & !sf::st_is_longlat(obj) & is.null(sf::st_crs(obj)$to_meter)
}
