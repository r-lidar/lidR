catalog_merge_results = function(ctg, any_list, any_type = "auto", ...)
{
  # Any type can only be 4 strings
  if (!any_type %in% c("auto", "raster", "spatial", "las"))
  {
    warning("The registered output type is not supported. Merging is impossible. A list has been returned.", call. = FALSE)
    return(any_list)
  }

  # Automerge: determine the merging type to apply
  if (any_type == "auto")
  {
    data_types <- sapply(any_list, function(x) { class(x)[1] })
    data_type  <- unique(data_types)

    if (length(data_type) > 1)
    {
      warning("The list returned by 'catalog_apply' contains heterogeneous objects. Merging is impossible. A list has been returned.", call. = FALSE)
      return(any_list)
    }

    # The case with strings is special. Strings should be returned 'as is' but in some cases we can be
    # clever and merge the strings into a LAScatalog or a VRT
    if (opt_output_files(ctg) != "")
    {
      any <- unlist(any_list)
      ext <- unique(tools::file_ext(any))

      if (length(ext) > 1)
      {
        warning("The list returned by 'catalog_apply' contains heterogeneous objects. Merging is impossible. A list has been returned.", call. = FALSE)
        return(any_list)
      }

      if (ext %in% c("las", "laz"))
        any_type = "las"
      else if (ext %in% c("grd", "asc", "sdat", "rst", "nc", "tif", "tiff", "envi", "bil", "img"))
        any_type = "raster"
      else
        return(unlist(any_list))
    }
    else
    {
      x <- any_list[[1]]

      if (inherits(x, "Raster")) {
        any_type <- "raster"
      } else if (inherits(x, "LAS")) {
        any_type <- "las"
      } else if (inherits(x, "SpatialPolygons") | inherits(x, "SpatialPoints")) {
        any_type <- "spatial"
      } else if (inherits(x, "sf")) {
        any_type <- "SimpleFeature"
      } else if  (inherits(x, "data.frame")) {
        any_type <- "dataframe"
      } else {
        any_type <- "unknown"
      }
    }
  }

  # The data_type has been guessed or is provided. Merge:
  tryCatch(
  {
    if (any_type == "raster")
    {
      if (opt_output_files(ctg) != "")
      {
        return(rBuildVRT(any_list, ...))
      }
      else
      {
        return(rMergeList(any_list))
      }
    }
    else if (any_type == "las")
    {
      if (opt_output_files(ctg) != "")
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
    else if (any_type == "spatial")
    {
      if (opt_output_files(ctg) != "")
      {
        return(unlist(output))
      }
      else
      {
        output <- do.call(rbind, any_list)
        output@proj4string <- ctg@proj4string
        return(output)
      }
    }
    else if (any_type == "SimpleFeature")
    {
      if (opt_output_files(ctg) != "")
      {
        return(unlist(output))
      }
      else
      {
        output <- do.call(rbind, any_list)
        sf::st_crs(output) <- ctg@proj4string
        return(output)
      }
    }
    else if (any_type == "dataframe")
    {
      if (opt_output_files(ctg) != "")
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
