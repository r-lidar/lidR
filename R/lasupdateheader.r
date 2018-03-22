lasupdateheader = function(las)
{
  stopifnotlas(las)

  header = as.list(las@header)
  new_header = rlas::header_update(header, las@data)
  new_header = LASheader(new_header)
  C_lasupdateheader(las, new_header)
  return(invisible())
}

lasaddextrabyte = function(las, name, desc)
{
  X = las@data[[name]]
  header = as.list(las@header)
  header = rlas::header_add_extrabytes(header, X, name, desc)
  header = LASheader(header)
  C_lasupdateheader(las, header)
  return(invisible())
}