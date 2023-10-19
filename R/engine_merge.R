#' @rdname engine
#' @export
#' @param any_list list of LAS, Raster, stars, SpatRaster, sf, sfc, Spatial, data.frame
#' @param ctg LAScatalog
#' @family LAScatalog processing engine
engine_merge = function(ctg, any_list, ...)
{
  object_are_in_files <- opt_output_files(ctg) != ""

  if (!object_are_in_files & length(any_list) == 1)
    return(any_list[[1]])

  # Determine the merging type to apply
  data_types <- sapply(any_list, function(x) { class(x)[1] })
  data_type  <- unique(data_types)
  any_type   <- "unknown"
  elmtone    <- any_list[[1]]

  if (length(data_type) > 1)
  {
    warning("The list returned by 'catalog_apply' contains heterogeneous objects. Merging is impossible. A list has been returned.", call. = FALSE)
    return(any_list)
  }

  # The case with strings is special. Strings should be returned 'as is' but in some cases we can be
  # clever and merge the strings into a LAScatalog or a VRT
  if (object_are_in_files)
  {
    any  <- unlist(any_list)
    ext  <- unique(tools::file_ext(any))

    if (length(ext) > 1)
    {
      warning("The list returned by 'catalog_apply' contains heterogeneous objects. Merging is impossible. A list has been returned.", call. = FALSE)
      return(any_list)
    }

    # LAS and raster are the two special cases supported. In other case we return the list
    # for raster it can be Raster or stars default is stars.
    if (ext %in% c("las", "laz"))
      any_type = "las"
    else if (ext %in% c("grd", "asc", "sdat", "rst", "nc", "tif", "tiff", "envi", "bil", "img"))
    {
      any_type <- attr(elmtone, "rasterpkg")
      if (any_type == "terra") any_type = "rterra"
    }
    else
      return(unlist(any_list))
  }
  else
  {
    x <- elmtone

    if (inherits(x, "Raster")) {
      any_type <- "raster"
    } else if (inherits(x, "stars")) {
      any_type <- "stars"
    } else if (inherits(x, "SpatRaster")) {
      any_type <- "rterra"
    } else if (inherits(x, "SpatVector")) {
      any_type <- "vterra"
    } else if (inherits(x, "LAS")) {
      any_type <- "las"
    } else if (inherits(x, "SpatialPolygons") | inherits(x, "SpatialPoints") | inherits(x, "SpatialLines")) {
      any_type <- "sp"
    } else if (inherits(x, "sf")) {
      any_type <- "sf"
    } else if (inherits(x, "sfc")) {
      any_type <- "sfc"
    } else if  (inherits(x, "data.frame")) {
      any_type <- "dataframe"
    } else {
      any_type <- "unknown"
    }
  }

  # The data_type has been guessed. Merge:
  tryCatch(
  {
    if (any_type == "raster")
    {
      if (object_are_in_files)
      {
        names <- attr(elmtone, "layernames")
        vrt <- raster_build_vrt(any_list, ...)
        raster <- raster::brick(vrt)
        raster_names(raster) <- names

        if (dim(raster)[3] == 1)
          return(raster[[1]])
        else
          return(raster)
      }
      else
      {
        names <- raster_names(any_list[[1]])
        raster <- do.call(raster::mosaic, c(any_list, fun = mean))
        raster_names(raster) <- names
        return(raster)
      }
    }
    if (any_type == "stars")
    {
      if (object_are_in_files)
      {
        names <- attr(elmtone, "layernames")
        vrt <- raster_build_vrt(any_list, ...)
        raster <- stars::read_stars(vrt, proxy = TRUE)
        raster_names(raster) <- names
        return(raster)
      }
      else
      {
        names <- raster_names(any_list[[1]])
        raster <- do.call(stars::st_mosaic, any_list)
        raster_names(raster) <- names
        return(raster)
      }
    }
    if (any_type == "rterra")
    {
      if (object_are_in_files)
      {
        names <- attr(elmtone, "layernames")
        vrt <- raster_build_vrt(any_list, ...)
        raster <- terra::rast(vrt)
        raster_names(raster) <- names
        return(raster)
      }
      else
      {
        names <- raster_names(any_list[[1]])
        raster <- do.call(terra::mosaic, any_list)
        raster_names(raster) <- names
        return(raster)
      }
    }
    else if (any_type == "vterra")
    {
      if (object_are_in_files)
      {
        return(unlist(any_list))
      }
      else
      {
        output <- do.call(rbind, any_list)
        return(output)
      }
    }
    else if (any_type == "las")
    {
      if (object_are_in_files)
      {
        output <- unlist(any_list)
        output <- suppressMessages(suppressWarnings(readLAScatalog(output)))
        opt_copy(output) <- ctg
        return(output)
      }
      else
      {
        return(do.call(rbind, any_list))
      }
    }
    else if (any_type == "sp")
    {
      if (object_are_in_files)
      {
        return(unlist(output))
      }
      else
      {
        output <- do.call(rbind, any_list)
        output@proj4string <- as(st_crs(ctg), "CRS")
        return(output)
      }
    }
    else if (any_type == "sf")
    {
      if (object_are_in_files)
      {
        return(unlist(output))
      }
      else
      {
        output <- data.table::rbindlist(any_list)
        output <- sf::st_as_sf(output)
        sf::st_crs(output) <- st_crs(ctg)
        bbox <- st_bbox(ctg)
        attributes(bbox)$crs <- NULL
        attributes(sf::st_geometry(output))$bbox  <- bbox
        return(output)
      }
    }
    else if (any_type == "sfc")
    {
      if (object_are_in_files)
      {
        return(unlist(output))
      }
      else
      {
        output <- do.call(c, any_list)
        sf::st_crs(output) <- st_crs(ctg)
        return(output)
      }
    }
    else if (any_type == "dataframe")
    {
      if (object_are_in_files)
      {
        return(unlist(output))
      }
      else
      {
        output <- data.table::rbindlist(any_list)
        return(output)
      }
    }
    else
    {
      warning("The list returned by 'catalog_apply' contains unsupported objects. Merging is impossible. A list has been returned.", call. = FALSE)
      return(any_list)
    }
  },
  error = function(e)
  {
    warning("An error occured during the automatic merge of 'catalog_apply'. Merging is impossible. A list has been returned.", call. = FALSE)
    message(e)
    return(any_list)
  })
}
