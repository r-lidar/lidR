lasupdateheader = function(las)
{
  stopifnotlas(las)

  header = as.list(las@header)
  new_header = rlas::header_update(header, las@data)
  new_header = LASheader(new_header)
  C_lasupdateheader(las, new_header)
  return(invisible())
}