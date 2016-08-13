#' Write a las file
#'
#' Write a LAS object into a binary file (.las if specified in filename)
#'
#' @param obj an object of class LAS
#' @param file character. a character string naming an output file
#' @return void
#' @export writeLAS
setGeneric("writeLAS", function(obj, file){standardGeneric("writeLAS")})

#' @rdname writeLAS
setMethod("writeLAS", "LAS",
  function(obj, file)
  {
    islas = tools::file_ext(file) %in% c("las", "laz")

    if(length(file) > 1)
      lidRError("LAS5", behaviour = stop)

    if(!islas)
      lidRError("LAS2", files = files[!islas], behaviour = stop)

    I = RN = NoR = SDF = EoF = C = SA = PSI = R = G = B = integer(0)
    time = numeric(0)

    fields = names(obj@data)

    if("Intensity" %in% fields)
      I = obj@data$Intensity
    if("ReturnNumber" %in% fields)
      RN = obj@data$ReturnNumber
    if("NumberOfReturns" %in% fields)
      NoR = obj@data$NumberOfReturns
    if("ScanDirectionFlag" %in% fields)
      SDF = obj@data$ScanDirectionFlag
    if("EdgeofFlightline" %in% fields)
      EoF = obj@data$EdgeofFlightline
    if("Classification" %in% fields)
      C = obj@data$Classification
    if("ScanAngle" %in% fields)
      SA = obj@data$ScanAngle
    if("UserData" %in% fields)
      US = obj@data$UserData
    if("gpstime" %in% fields)
      time = obj@data$gpstime
    if("PointSourceID" %in% fields)
      PSI = obj@data$PointSourceID
    if("R" %in% fields & "G" %in% fields & "B" %in% fields )
    {
      R = obj@data$R
      G = obj@data$G
      B = obj@data$B
    }

    LASlibWrite(file, obj@header, obj@data$X, obj@data$Y, obj@data$Z, I, RN, NoR, SDF, EoF, C, SA, PSI, time, R, G, B)
  }
)