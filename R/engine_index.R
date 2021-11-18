#' @param bboxes list. list of bbox
#' @param shape numeric LIDRRECTANGLE|LIDRCIRCLE
#' @param buffer numeric
#' @param process. logical
#' @param outside_catalog_is_null bool
#' @param by_file bool
#' @noRd
engine_index =	function(ctg, bboxes, shape = LIDRRECTANGLE, buffer = 0, process = TRUE, outside_catalog_is_null = TRUE, by_file = FALSE)
{
  stopifnot(is.list(bboxes))

  MinX <- ctg[["Min.X"]]
  MaxX <- ctg[["Max.X"]]
  MinY <- ctg[["Min.Y"]]
  MaxY <- ctg[["Max.Y"]]

  if (length(process) == 1L)
    process <- rep(process, nrow(ctg@data))

  queries <- vector("list", length(bboxes))

  for (i in 1:length(bboxes))
  {
    bbox  <- bboxes[[i]]
    bbbox <- st_expand_bbox(bbox, buffer)

    tile_is_in_bbox          <- !(MinX >= bbox$xmax  | MaxX <= bbox$xmin  | MinY >= bbox$ymax  | MaxY <= bbox$ymin)
    tile_is_in_buffered_bbox <- !(MinX >= bbbox$xmax | MaxX <= bbbox$xmin | MinY >= bbbox$ymax | MaxY <= bbbox$ymin)

    if (all(!process[tile_is_in_bbox]))
      select <- FALSE
    else if (sum(tile_is_in_bbox) > 0)
      select <- tile_is_in_buffered_bbox
    else
      select <- FALSE

    files  <- ctg$filename[select]

    if (length(files) == 0 & outside_catalog_is_null)
      next
    else if (length(files) == 0 & !outside_catalog_is_null)
      files <- ""

    # If one file is considered the main one put it in first place so it get the header precedence
    # when merged-reading the las files.
    if (length(files) > 1 && by_file)
    {
      main <- ctg$filename[process][i]
      j <- which(files == main)
      if (length(j) == 0) stop("Internal error: the indexation algorithm generated an incorrect list of files. Please report this error.") # nocov
      sfiles <- c(main, files[-j[1]])
      files <- sfiles
    }

    # If one file that encompasses the bbox is set to 'non processing' resize the chunk
    if (!by_file && any(!process[tile_is_in_bbox]))
    {
      k <- process & tile_is_in_bbox
      bbox$xmin <- max(min(MinX[k]), bbox$xmin)
      bbox$ymin <- max(min(MinY[k]), bbox$ymin)
      bbox$xmax <- min(max(MaxX[k]), bbox$xmax)
      bbox$ymax <- min(max(MaxY[k]), bbox$ymax)
      bbox      <- sf::st_bbox(unlist(bbox))
      bbbox     <- st_expand_bbox(bbox, buffer)
    }

    center  <- list(x = (bbox$xmax + bbox$xmin)/2, y = (bbox$ymax + bbox$ymin)/2)
    width   <- (bbox$xmax - bbox$xmin)
    height  <- (bbox$ymax - bbox$ymin)
    cluster <- LAScluster(center, width, height, buffer, shape, files, "noname", crs = st_crs(ctg), index = ctg@index)

    cluster@select <- opt_select(ctg)
    cluster@filter <- paste(cluster@filter, opt_filter(ctg))

    queries[[i]] <- cluster
  }

  return(queries)
}
