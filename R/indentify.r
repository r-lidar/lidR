#' @importFrom dplyr lag
.identify_pulse = function(return.number)
{
  boo = dplyr::lag(return.number) >= return.number
  boo[1] = TRUE
  return(cumsum(boo))
}

#' @importFrom dplyr lag
.identify_flightlines = function(time, t = 30)
{
  boo = (time - dplyr::lag(time)) > t
  boo[1] = TRUE
  return(cumsum(boo))
}

#' @importFrom dplyr lag
.identify_scanline = function(ScanDirectionFlag)
{
  boo = ScanDirectionFlag != dplyr::lag(ScanDirectionFlag)
  boo[1] = TRUE
  return(cumsum(boo))
}