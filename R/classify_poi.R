#' @export
#' @rdname classify
classify_poi = function(las, class, poi = NULL, roi = NULL, inverse_roi = FALSE, by_reference = FALSE)
{
  UseMethod("classify_poi", las)
}

#' @export
classify_poi.LAS = function(las, class, poi = NULL, roi = NULL, inverse_roi = FALSE, by_reference = FALSE)
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

  if (! "Classification" %in% names(las))
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
classify_poi.LAScatalog = function(las, class, poi = NULL, roi = NULL, inverse_roi = FALSE, by_reference = FALSE)
{
  opt_select(las) <- "*"
  opt_chunk_buffer(las) <- 0

  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, classify_poi, class = class, poi = poi, roi = roi, inverse_roi = inverse_roi, .options = options)
  return(output)
}
