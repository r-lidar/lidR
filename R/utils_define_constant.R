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

CHUNK_WAINTING        <- 0L
CHUNK_OK              <- 1L
CHUNK_NULL            <- 2L
CHUNK_ERROR           <- 3L
CHUNK_WARNING         <- 4L
CHUNK_PROCESSING      <- 5L

LASFIELDS             <- c("X", "Y", "Z", "Intensity",
                           "ReturnNumber", "NumberOfReturns",
                           "ScanDirectionFlag", "EdgeOfFlightline",
                           "Classification",
                           "Synthetic_flag", "Keypoint_flag",
                           "Withheld_flag", "Overlap_flag",
                           "ScanAngle", "ScanAngleRank",
                           "ScannerChannel", "NIR",
                           "UserData", "gpstime", "PointSourceID",
                           "R", "G", "B")

