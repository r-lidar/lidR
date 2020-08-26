format_point_type = function(type)
{
  if (type == UKNLAS)
    type = "not registered"
  else if (type == ALSLAS)
    type = "ALS"
  else if (type == TLSLAS)
    type = "TLS"
  else if (type == UAVLAS)
    type = "UAV"
  else if (type == DAPLAS)
    type = "DAP"
  else if (type == MLSLAS)
    type = "mutispectral ALS"
  else if (type == UKNLAS + NLAS)
    type = "not registered (normalized)"
  else if (type == ALSLAS + NLAS)
    type = "ALS (normalized)"
  else if (type == TLSLAS + NLAS)
    type = "TLS (normalized)"
  else if (type == UAVLAS + NLAS)
    type = "UAV (normalized)"
  else if (type == DAPLAS + NLAS)
    type = "DAP (normalized)"
  else if (type == MLSLAS + NLAS)
    type = "mutispectral ALS (normalized)"
  else
    type = "invalid type registred"

  return(type)
}
