#' @importFrom tools file_ext
.loadLAS = function(files, fields="standard")
{
  valid = file.exists(files)
  islas = tools::file_ext(files) == "las"

  if( sum(!valid) != 0 )
    stop(paste("File(s) ", files[!valid], " not found\n", sep=""))

  if( sum(!islas) != 0 )
    stop(paste("File(s) ", files[!islas], " not supported\n", sep=""))

  data = data.table()

  for(file in files)
    data = rbindlist(list(data, readLAS(file, fields) ))

  return(data)
}

#' Read a .las file
#'
#' Methods to read .las file
#' @param LASfile character. filename of .las file
#' @param fields character. \code{"minimal"}, \code{"standard"}, \code{"all"}.
#' @return A data.table
#' @export readLAS
#' @author Michael Sumner and Carlos Alberto Silva.
#' @note The function was firsly wrote by Michael Sumner and Carlos Alberto Silva.
#' This version is a function modify from the first one whichis less greedy in memory
#' and more flexible for the lidR package usages. But the greater work was made by the authors
#' of rLidar package.
#' @importFrom bitops bitAnd bitShiftR
readLAS <- function(LASfile, fields="standard")
{
  if(!(fields %in% c("minimal", "standard", "all")))
      stop("The field parameter is invalid. It must to be a string: 'minimal' or 'standard' or 'all'")

  skip <- 0
  nrows <- NULL

  hd <- publicHeaderDescription()
  pheader <- vector("list", nrow(hd))
  names(pheader) <- hd$Item
  con <- file(LASfile, open = "rb")
  isLASFbytes <- readBin(con, "raw", size = 1, n = 4, endian = "little")
  pheader[[hd$Item[1]]] <- readBin(isLASFbytes, "character", size = 4, endian = "little")

  if (! pheader[[hd$Item[1]]] == "LASF")
    stop("The LASfile input is not a valid LAS file")

  for (i in 2:nrow(hd))
    pheader[[hd$Item[i]]] <- readBin(con, what = hd$what[i], size = hd$Rsize[i], endian = "little", n = hd$n[i])

  close(con)

  numberPointRecords <- pheader[["Number of point records"]]
  offsetToPointData <- pheader[["Offset to point data"]]
  pointDataRecordLength <-pheader[["Point Data Record Length"]]
  xyzScaleOffset <- cbind(unlist(pheader[c("X scale factor", "Y scale factor", "Z scale factor")]),
                          unlist(pheader[c("X offset", "Y offset", "Z offset")]))

  con <- file(LASfile, open = "rb")
  junk <- readBin(con, "raw", size = 1, n = offsetToPointData)

  if (skip > 0)
  {
    junk <- readBin(con, "raw", size = 1, n = pointDataRecordLength * skip)
    numberPointRecords <- numberPointRecords - skip
  }

  if (!is.null(nrows))
  {
    if (numberPointRecords > nrows)
      numberPointRecords <- nrows
  }

  if (numberPointRecords < 1)
    stop("no records left to read")

  allbytes <- matrix(readBin(con, "raw", n = pointDataRecordLength * numberPointRecords, size = 1, endian = "little"),
                     ncol= pointDataRecordLength, nrow = numberPointRecords, byrow = TRUE)


  close(con)

  mm <- matrix(readBin(t(allbytes[,1:(3*4)]), "integer", size = 4, n = 3 * numberPointRecords, endian = "little"), ncol = 3, byrow = TRUE)

  mm[,1] <- mm[ ,1] * xyzScaleOffset[1,1] + xyzScaleOffset[1, 2]
  mm[,2] <- mm[ ,2] * xyzScaleOffset[2,1] + xyzScaleOffset[2, 2]
  mm[,3] <- mm[ ,3] * xyzScaleOffset[3,1] + xyzScaleOffset[3, 2]

  colnames(mm) <- c("X", "Y", "Z")

  Intensity <- readBin(t(allbytes[, 13:14]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

  bytesList <- readBin(t(allbytes[,15]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")


  if (pheader[17][[1]]==00)
  {
    ReturnNumber <- bitAnd(7, bytesList)
    NumberOfReturns <- bitShiftR(bitAnd(56, bytesList), 3)
    ScanDirectionFlag <- bitShiftR(bitAnd(bytesList, 64), 6)
    EdgeofFlightLine <- bitShiftR(bitAnd(bytesList, 128), 7)
    Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
    ScanAngle <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
    UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
    PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

    ReturnNumber = as.integer(ReturnNumber)
    NumberOfReturns = as.integer(NumberOfReturns)

    if (fields=="minimal")
      return(data.table(mm, ReturnNumber,NumberOfReturns))
    else if(fields=="standard")
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,Classification,ScanAngle))
    else
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,EdgeofFlightLine,Classification,ScanAngle,UserData,PointSourceID))
  }

  if (pheader[17][[1]]==01)
  {
    ReturnNumber <- bitAnd(7, bytesList)
    NumberOfReturns <- bitShiftR(bitAnd(56, bytesList), 3)
    ScanDirectionFlag <- bitShiftR(bitAnd(bytesList, 64), 6)
    EdgeofFlightLine <- bitShiftR(bitAnd(bytesList, 128), 7)
    Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
    ScanAngle <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
    UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
    PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
    gpstime <- NULL

    if (ncol(allbytes) == 28) gpstime <- readBin(t(allbytes[ , 21:28]), "numeric", size = 8, n = numberPointRecords, endian = "little")

    # Below modified from Carlos A. Silva and Nicholas L. Crookston and Andrew T. Hudak and Lee A. Vierling
    ReturnNumber = as.integer(ReturnNumber)
    NumberOfReturns = as.integer(NumberOfReturns)

    if (fields=="minimal")
      return(data.table(mm, ReturnNumber,NumberOfReturns,gpstime))
    else if(fields=="standard")
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,Classification,ScanAngle, gpstime))
    else
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,EdgeofFlightLine,Classification,ScanAngle,UserData,PointSourceID,gpstime))
  }


  if (pheader[17][[1]]==02)
  {
    ReturnNumber <- bitAnd(7, bytesList)
    NumberOfReturns <- bitShiftR(bitAnd(56, bytesList), 3)
    ScanDirectionFlag <- bitShiftR(bitAnd(bytesList, 64), 6)
    EdgeofFlightLine <- bitShiftR(bitAnd(bytesList, 128), 7)
    Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")

    ScanAngle <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
    UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
    PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

    R <- readBin(t(allbytes[, 21:22]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
    G <- readBin(t(allbytes[, 23:24]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
    B <- readBin(t(allbytes[, 25:26]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

    # Below modified from Carlos A. Silva and Nicholas L. Crookston and Andrew T. Hudak and Lee A. Vierling
    ReturnNumber = as.integer(ReturnNumber)
    NumberOfReturns = as.integer(NumberOfReturns)

    if (fields=="minimal")
      return(data.table(mm, ReturnNumber,NumberOfReturns))
    else if(fields=="standard")
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,Classification,ScanAngle,R,G,B))
    else
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,EdgeofFlightLine,Classification,ScanAngle,UserData,PointSourceID, R,G,B))
  }

  if (pheader[17][[1]]==03)
  {
    ReturnNumber <- bitAnd(7, bytesList)
    NumberOfReturns <- bitShiftR(bitAnd(56, bytesList), 3)
    ScanDirectionFlag <- bitShiftR(bitAnd(bytesList, 64), 6)
    EdgeofFlightLine <- bitShiftR(bitAnd(bytesList, 128), 7)
    Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")

    ScanAngle <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
    UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
    PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

    gpstime <- NULL
    gpstime <- readBin(t(allbytes[ , 21:28]), "numeric", size = 8, n = numberPointRecords, endian = "little")

    R <- readBin(t(allbytes[, 29:30]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
    G <- readBin(t(allbytes[, 31:32]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
    B <- readBin(t(allbytes[, 33:34]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

    # Below modified from Carlos A. Silva and Nicholas L. Crookston and Andrew T. Hudak and Lee A. Vierling
    ReturnNumber = as.integer(ReturnNumber)
    NumberOfReturns = as.integer(NumberOfReturns)

    if (fields=="minimal")
      return(data.table(mm,ReturnNumber,NumberOfReturns,gpstime))
    else if(fields=="standard")
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,Classification,ScanAngle,R,G,B,gpstime))
    else
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,EdgeofFlightLine,Classification,ScanAngle,UserData,PointSourceID,R,G,B,gpstime))
  }

  if (pheader[17][[1]]==04)
  {

    ReturnNumber <- bitAnd(7, bytesList)
    NumberOfReturns <- bitShiftR(bitAnd(56, bytesList), 3)
    ScanDirectionFlag <- bitShiftR(bitAnd(bytesList, 64), 6)
    EdgeofFlightLine <- bitShiftR(bitAnd(bytesList, 128), 7)
    Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")

    ScanAngle <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
    UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
    PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

    gpstime <- NULL
    gpstime <- readBin(t(allbytes[ , 21:28]), "numeric", size = 8, n = numberPointRecords, endian = "little")

    WavePacket_Descriptor_Index <- readBin(t(allbytes[, 29]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
    Byte_offset_to_waveform_data <- readBin(t(allbytes[, 30:37]), "integer", size = 8, n = numberPointRecords, signed = FALSE, endian = "little")
    Waveform_packet_size_in_bytes <- readBin(t(allbytes[, 38:41]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
    Return_Point_Waveform_Location<- readBin(t(allbytes[, 42:45]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
    X.t<- readBin(t(allbytes[, 46:49]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
    Y.t<- readBin(t(allbytes[, 50:53]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
    Z.t<- readBin(t(allbytes[, 54:57]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")

    # Below modified from Carlos A. Silva and Nicholas L. Crookston and Andrew T. Hudak and Lee A. Vierling
    ReturnNumber = as.integer(ReturnNumber)
    NumberOfReturns = as.integer(NumberOfReturns)

    if (fields=="minimal")
      return(data.table(mm,ReturnNumber,NumberOfReturns,gpstime))
    else if(fields=="standard")
    {
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,Classification,ScanAngle,gpstime,
                   WavePacket_Descriptor_Index,Byte_offset_to_waveform_data,Waveform_packet_size_in_bytes,
                   Return_Point_Waveform_Location,X.t,Y.t,Z.t))
    }
    else
    {
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,EdgeofFlightLine,Classification,ScanAngle,UserData,PointSourceID,gpstime,
                   WavePacket_Descriptor_Index,Byte_offset_to_waveform_data,Waveform_packet_size_in_bytes,
                   Return_Point_Waveform_Location,X.t,Y.t,Z.t))
    }
  }

  if (pheader[17][[1]]==05)
  {

    ReturnNumber <- bitAnd(7, bytesList)
    NumberOfReturns <- bitShiftR(bitAnd(56, bytesList), 3)
    ScanDirectionFlag <- bitShiftR(bitAnd(bytesList, 64), 6)
    EdgeofFlightLine <- bitShiftR(bitAnd(bytesList, 128), 7)
    Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")

    ScanAngle <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
    UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
    PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

    gpstime <- NULL
    gpstime <- readBin(t(allbytes[ , 21:28]), "numeric", size = 8, n = numberPointRecords, endian = "little")

    R <- readBin(t(allbytes[, 29:30]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
    G <- readBin(t(allbytes[, 31:32]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
    B <- readBin(t(allbytes[, 33:34]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

    WavePacket_Descriptor_Index <- readBin(t(allbytes[, 35]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
    Byte_offset_to_waveform_data <- readBin(t(allbytes[, 36:43]), "integer", size = 8, n = numberPointRecords, signed = FALSE, endian = "little")
    Waveform_packet_size_in_bytes <- readBin(t(allbytes[, 44:47]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
    Return_Point_Waveform_Location<- readBin(t(allbytes[, 48:51]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
    X.t<- readBin(t(allbytes[, 52:55]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
    Y.t<- readBin(t(allbytes[, 56:59]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
    Z.t<- readBin(t(allbytes[, 60:63]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")

    # Below modified from Carlos A. Silva and Nicholas L. Crookston and Andrew T. Hudak and Lee A. Vierling
    ReturnNumber = as.integer(ReturnNumber)
    NumberOfReturns = as.integer(NumberOfReturns)

    if (fields=="minimal")
      return(data.table(mm,ReturnNumber,NumberOfReturns,gpstime))
    else if(fields=="standard")
    {
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,Classification,ScanAngle,gpstime,
                   R, G, B, WavePacket_Descriptor_Index,Byte_offset_to_waveform_data,Waveform_packet_size_in_bytes,
                   Return_Point_Waveform_Location,X.t,Y.t,Z.t))
    }
    else
    {
      return(data.table(mm, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,EdgeofFlightLine,Classification,ScanAngle,UserData,PointSourceID,gpstime,
                   R, G, B, WavePacket_Descriptor_Index,Byte_offset_to_waveform_data,Waveform_packet_size_in_bytes,
                   Return_Point_Waveform_Location,X.t,Y.t,Z.t))
    }

  }
}
