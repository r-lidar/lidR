LIDRCIRCLE            <- 0L
LIDRRECTANGLE         <- 1L

LIDRNOBUFFER          <- 0L
LIDRBUFFER            <- 1L
LIDRBOTTOMBUFFER      <- 1L
LIDRLEFTBUFFER        <- 2L
LIDRTOPBUFFER         <- 3L
LIDRRIGHTBUFFER       <- 4L

LASNONCLASSIFIED      <- 0L
LASUNCLASSIFIED       <- 1L
LASGROUND             <- 2L
LASLOWVEGETATION      <- 3L
LASMEDIUMVEGETATION   <- 4L
LASHIGHVEGETATION     <- 5L
LASBUILDING           <- 6L
LASLOWPOINT           <- 7L
LASKEYPOINT           <- 8L
LASWATER              <- 9L
LASRAIL               <- 10L
LASROADSURFACE        <- 11L
LASWIREGUARD          <- 13L
LASWIRECONDUCTOR      <- 14L
LASTRANSMISSIONTOWER  <- 15L
LASBRIGDE             <- 17L
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

