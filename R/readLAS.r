#' Read las files
#'
#' @param files character.
#' @param Intensity logical.
#' @param ReturnNumber logical.
#' @param NumberOfReturns logical.
#' @param ScanDirectionFlag logical.
#' @param EdgeofFlightline logical.
#' @param Classification logical.
#' @param ScanAngle logical.
#' @param UserData logical.
#' @param PointSourceID logical.
#' @param RGB logical.
#'
#' @return A LAS object
#' @export readLAS
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = readLAS(LASfile)
#' @importFrom data.table data.table rbindlist
readLAS = function(files,
                   Intensity = TRUE,
                   ReturnNumber = TRUE,
                   NumberOfReturns = TRUE,
                   ScanDirectionFlag = FALSE,
                   EdgeofFlightline = FALSE,
                   Classification = TRUE,
                   ScanAngle = TRUE,
                   UserData = FALSE,
                   PointSourceID = FALSE,
                   RGB = TRUE)
{
  valid = file.exists(files)
  islas = tools::file_ext(files) == "las"

  if( sum(!valid) != 0 )
  {
    lidRError("LAS1", files = files[!valid], behaviour = warning)
    files = files[valid]
  }

  if( sum(!islas) != 0 )
  {
    lidRError("LAS2", files = files[!islas], behaviour = warning)
    files = files[valid]
  }

  data = lapply(files, function(x)
  {
    as.data.table(readLASdata(x, Intensity,
                              ReturnNumber,
                              NumberOfReturns,
                              ScanDirectionFlag,
                              EdgeofFlightline,
                              Classification,
                              ScanAngle,
                              UserData,
                              PointSourceID,
                              RGB))
  })

  data = data.table::rbindlist(data)

  las = LAS(data)
  las@header = readLASheader(files[1])

  # recompute header

  return(las)
}
