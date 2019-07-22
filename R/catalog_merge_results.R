catalog_merge_results = function(ctg, any_list, any_type, ...)
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
  else
  {
    stop("Type not supported.")
  }
}
