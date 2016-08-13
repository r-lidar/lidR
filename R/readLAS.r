#' Read las files
#'
#' Read las files in format 1 to 4 according to LAS specification and return an object of class LAS
#'
#' If several files are given the returned LAS object is considered as one LAS file.
#' The non-recomputable informations in the header are the one read from the first file of the list.
#' The other informations are recomputed on the fly like the extend hull of the data.
#' The optional logical parameters enable to do not load some fields to save memory if the field are useless for user's purpose.
#'
#' @param files array of character.
#' @param Intensity logical. do you want to load Intensity field? default: TRUE
#' @param ReturnNumber logical. do you want to load ReturnNumber field? default: TRUE
#' @param NumberOfReturns logical. do you want to load NumberOfReturns field? default: TRUE
#' @param ScanDirectionFlag logical. do you want to load ScanDirectionFlag field? default: FALSE
#' @param EdgeofFlightline logical. do you want to load EdgeofFlightline field? default: FALSE
#' @param Classification logical. do you want to load Classification field? default: TRUE
#' @param ScanAngle logical. do you want to load intensity field? default: TRUE
#' @param UserData logical. do you want to load UserData field? default: FALSE
#' @param PointSourceID logical. do you want to load PointSourceID field? default: FALSE
#' @param RGB logical. do you want to load intensity R,G and B? default: TRUE
#'
#' @return A LAS object
#' @export readLAS
#' @seealso
#' \link[lidR:LAS-class]{Class LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
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
  islas = tools::file_ext(files) %in% c("las", "laz")

  if( sum(!valid) != 0 )
  {
    lidRError("LAS1", files = files[!valid], behaviour = warning)
    files = files[valid]
  }

  if( sum(!islas) != 0 )
  {
    lidRError("LAS2", files = files[!islas], behaviour = warning)
    files = files[islas]
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
