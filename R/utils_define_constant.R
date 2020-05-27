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
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
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

LIDRDSM <- "DigitalSurfaceModel"
LIDRSPI <- "SpatialInterpolation"
LIDRGND <- "GroundSegmentation"
LIDRITD <- "IndividualTreeDetection"
LIDRDEC <- "PointCloudDecimation"
LIDRSHP <- "ShapeDetection"
LIDRSNG <- "SnagsSegmentation"
LIDRITS <- "IndividualTreeSegmentation"
LIDRNIT <- "normalize_intensity"
LIDRTRK <- "SensorTracking"

LIDRCONTEXTDSM <- "grid_canopy"
LIDRCONTEXTSPI <- c("normalize_height", "grid_terrain", "p2r")
LIDRCONTEXTGND <- "classify_ground"
LIDRCONTEXTITD <- "find_trees"
LIDRCONTEXTDEC <- "decimate_points"
LIDRCONTEXTSHP <- "segment_shapes"
LIDRCONTEXTSNG <- "segment_snags"
LIDRCONTEXTITS <- "segment_trees"
LIDRCONTEXTNIT <- "normalize_intensity"
LIDRCONTEXTTRK <- "track_sensor"

LIDRALGORITHM <- "lidRAlgorithm"
LIDRALGORITHMGENERIC = c(LIDRALGORITHM, "ANY", "function")
LIDRALGORITHMOPENMP <- "OpenMP"
LIDRALGORITHMPOINTCLOUDBASED <- "PointCloudBased"
LIDRALGORITHMRASTERBASED <- "RasterBased"
LIDRALGORITHMDEC <- LIDRALGORITHMDSM <- LIDRALGORITHMGND <- LIDRALGORITHMITD <- LIDRALGORITHMITS <- LIDRALGORITHMNIT <- LIDRALGORITHMSNG <- LIDRALGORITHMTRK <- LIDRALGORITHMSHP <- LIDRALGORITHMSPI <- LIDRALGORITHMGENERIC
LIDRALGORITHMDEC[2] <- LIDRDEC
LIDRALGORITHMDSM[2] <- LIDRDSM
LIDRALGORITHMGND[2] <- LIDRGND
LIDRALGORITHMITD[2] <- LIDRITD
LIDRALGORITHMITS[2] <- LIDRITS
LIDRALGORITHMNIT[2] <- LIDRNIT
LIDRALGORITHMSNG[2] <- LIDRSNG
LIDRALGORITHMTRK[2] <- LIDRTRK
LIDRALGORITHMSHP[2] <- LIDRSHP
LIDRALGORITHMSPI[2] <- LIDRSPI
