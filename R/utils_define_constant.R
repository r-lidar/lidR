LIDRCIRCLE            <- 0L
LIDRRECTANGLE         <- 1L

LIDRNOBUFFER          <- 0L
LIDRBUFFER            <- 1L
LIDRBOTTOMBUFFER      <- 1L
LIDRLEFTBUFFER        <- 2L
LIDRTOPBUFFER         <- 3L
LIDRRIGHTBUFFER       <- 4L


#' @export
LASNONCLASSIFIED      <- 0L
#' @export
LASUNCLASSIFIED       <- 1L
#' @export
LASGROUND             <- 2L
#' @export
LASLOWVEGETATION      <- 3L
#' @export
LASMEDIUMVEGETATION   <- 4L
#' @export
LASHIGHVEGETATION     <- 5L
#' @export
LASBUILDING           <- 6L
#' @export
LASLOWPOINT           <- 7L
#' @export
LASKEYPOINT           <- 8L
#' @export
LASWATER              <- 9L
#' @export
LASRAIL               <- 10L
#' @export
LASROADSURFACE        <- 11L
#' @export
LASWIREGUARD          <- 13L
#' @export
LASWIRECONDUCTOR      <- 14L
#' @export
LASTRANSMISSIONTOWER  <- 15L
#' @export
LASBRIGDE             <- 17L
#' @export
LASNOISE              <- 18L

CHUNK_WAINTING        <- 0L
CHUNK_OK              <- 1L
CHUNK_NULL            <- 2L
CHUNK_ERROR           <- 3L
CHUNK_WARNING         <- 4L
CHUNK_PROCESSING      <- 5L

LASATTRIBUTES         <- c("X", "Y", "Z", "Intensity",
                           "ReturnNumber", "NumberOfReturns",
                           "ScanDirectionFlag", "EdgeOfFlightline",
                           "Classification",
                           "Synthetic_flag", "Keypoint_flag",
                           "Withheld_flag", "Overlap_flag",
                           "ScanAngle", "ScanAngleRank",
                           "ScannerChannel", "NIR",
                           "UserData", "gpstime", "PointSourceID",
                           "R", "G", "B")

LASCATALOGATTRIBUTES <- c("File.Signature", "File.Source.ID", "GUID", "Version.Major",
                          "Version.Minor", "System.Identifier", "Generating.Software",
                          "File.Creation.Day.of.Year", "File.Creation.Year", "Header.Size",
                          "Offset.to.point.data", "Number.of.variable.length.records",
                          "Point.Data.Format.ID", "Point.Data.Record.Length", "Number.of.point.records",
                          "X.scale.factor", "Y.scale.factor", "Z.scale.factor", "X.offset",
                          "Y.offset", "Z.offset", "Max.X", "Min.X", "Max.Y", "Min.Y", "Max.Z",
                          "Min.Z", "EPSG", "Number.of.1st.return", "Number.of.2nd.return",
                          "Number.of.3rd.return", "Number.of.4th.return", "Number.of.5th.return")

LIDRCONTEXTDSM <- "grid_canopy"
LIDRCONTEXTSPI <- c("lasnormalize", "grid_terrain", "spatial_interpolation")
LIDRCONTEXTGND <- "lasground"
LIDRCONTEXTITD <- "tree_detection"
LIDRCONTEXTDEC <- "lasfilterdecimate"
LIDRCONTEXTSHP <- "lasdetectshape"
LIDRCONTEXTSNG <- "lassnags"
LIDRCONTEXTITS <- "lastrees"

