catalog_merge_results = function(ctg, any_list, any_type, ...)
{
  if (any_type == "raster")
  {
    if (opt_output_files(ctg) != "")
      return(rBuildVRT(any_list, ...))
    else
      return(rMergeList(any_list))
  }
}
