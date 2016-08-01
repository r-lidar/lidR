#' Write las file
#'
#' @param LAS an object of class class
#' @param file character. a character string naming a file
#'
#' @return
#' @export writeLAS
#' @examples
writeLAS = function(LAS, file)
{
  I = RN = NoR = SDF = EoF = C = SA = PSI = R = G = B = integer(0)
  time = numeric(0)

  fields = names(LAS@data)

  if("Intensity" %in% fields)
    I = LAS@data$Intensity
  if("ReturnNumber" %in% fields)
    RN = LAS@data$ReturnNumber
  if("NumberOfReturns" %in% fields)
    NoR = LAS@data$NumberOfReturns
  if("ScanDirectionFlag" %in% fields)
    SDF = LAS@data$ScanDirectionFlag
  if("EdgeofFlightline" %in% fields)
    EoF = LAS@data$EdgeofFlightline
  if("Classification" %in% fields)
    C = LAS@data$Classification
  if("ScanAngle" %in% fields)
    SA = LAS@data$ScanAngle
  if("UserData" %in% fields)
    US = LAS@data$UserData
  if("gpstime" %in% fields)
    time = LAS@data$gpstime
  if("PointSourceID" %in% fields)
  if("PointSourceID" %in% fields)
    PSI = LAS@data$PointSourceID
  if("R" %in% fields)
  {
    R = LAS@data$R
    G = LAS@data$G
    B = LAS@data$B
  }

  liblasWriteLAS(file, LAS@header, LAS@data$X, LAS@data$Y, LAS@data$Z, I, RN, NoR, SDF, EoF, C, SA, PSI, time, R, G, B)
}