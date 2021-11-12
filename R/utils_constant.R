LIDRCIRCLE            <- 0L
LIDRRECTANGLE         <- 1L

LIDRNOBUFFER          <- 0L
LIDRBUFFER            <- 1L
LIDRBOTTOMBUFFER      <- 1L
LIDRLEFTBUFFER        <- 2L
LIDRTOPBUFFER         <- 3L
LIDRRIGHTBUFFER       <- 4L


#' ASPRS LAS Classification
#'
#' A set of global variables corresponding to the point classification defined by the ASPRS for the
#' LAS format. Instead of remembering the classification table of the specification it is possible
#' to use one of these global variables.
#'
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las = readLAS(LASfile)
#' las2 = filter_poi(las, Classification %in% c(LASGROUND, LASWATER))
#'
#' print(LASGROUND)
#' @rdname asprs
#' @name asprs
#' @export
LASNONCLASSIFIED      <- 0L
#' @export
#' @rdname asprs
LASUNCLASSIFIED       <- 1L
#' @export
#' @rdname asprs
LASGROUND             <- 2L
#' @export
#' @rdname asprs
LASLOWVEGETATION      <- 3L
#' @export
#' @rdname asprs
LASMEDIUMVEGETATION   <- 4L
#' @export
#' @rdname asprs
LASHIGHVEGETATION     <- 5L
#' @export
#' @rdname asprs
LASBUILDING           <- 6L
#' @export
#' @rdname asprs
LASLOWPOINT           <- 7L
#' @export
#' @rdname asprs
LASKEYPOINT           <- 8L
#' @export
#' @rdname asprs
LASWATER              <- 9L
#' @export
#' @rdname asprs
LASRAIL               <- 10L
#' @export
#' @rdname asprs
LASROADSURFACE        <- 11L
#' @export
#' @rdname asprs
LASWIREGUARD          <- 13L
#' @export
#' @rdname asprs
LASWIRECONDUCTOR      <- 14L
#' @export
#' @rdname asprs
LASTRANSMISSIONTOWER  <- 15L
#' @export
#' @rdname asprs
LASBRIGDE             <- 17L
#' @export
#' @rdname asprs
LASNOISE              <- 18L

LASALLECHOS <- 0L
LASFIRST <- 1L
LASINTERMEDIATE <- 2L
LASLASTOFMANY <- 3L
LASSINGLE <- 4L
LASMULTIPLE <- 5L
LASLAST <- 6L

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
