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

get_point_type = function(input)
{
  if (is.null(input)) return(UKNLAS)
  if (is.na(input))   return(UKNLAS)

  if (is.numeric(input)) {
    if (input < 0 | input > 15) stop("Incorrect type", call. = FALSE)
    if (as.integer(input) != input) stop("Incorrect type", call. = FALSE)
    return(input)
  } else if (is.character(input)) {
    types <- c("unknown", "ALS", "TLS", "UAV", "DAP")
    i <- which(input == types)
    if (length(i) == 0)  {
      types <- c("nunknown", "nALS", "nTLS", "nUAV", "nDAP")
      i <- which(input == types)
      if (length(i) == 0)
        stop("Incorrect type", call. = FALSE)
      else
        return(i + NLAS - 1)
    } else {
      return(i - 1)
    }
  } else {
    stop("Incorrect type", call. = FALSE)
  }
}
