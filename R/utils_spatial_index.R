LIDRSPATIALINDEXES  <- c("auto", "gridpartition", "voxelpartition", "quadtree", "octree", "sparcepartition")
LIDRAUTOINDEX       <- 0L
LIDRGRIDPARTITION   <- 1L
LIDRVOXELPARTITION  <- 2L
LIDRQUADTREE        <- 3L
LIDROCTREE          <- 4L
LIDRSPARSEPARTITION <- 5L

LIDRAQUISITIONDEVICES <- c("unknown", "als", "tls", "uav", "dap", "multispectral")
UKNLAS <- 0L
ALSLAS <- 1L
TLSLAS <- 2L
UAVLAS <- 3L
DAPLAS <- 4L
MLSLAS <- 5L
NLAS   <- 10L

LIDRDEFAULTINDEX <- list(sensor = UKNLAS, index = LIDRAUTOINDEX)
LIDRALSINDEX     <- list(sensor = ALSLAS, index = LIDRAUTOINDEX)
LIDRTLSINDEX     <- list(sensor = TLSLAS, index = LIDRAUTOINDEX)
LIDRUAVINDEX     <- list(sensor = UAVLAS, index = LIDRAUTOINDEX)
LIDRDAPINDEX     <- list(sensor = DAPLAS, index = LIDRAUTOINDEX)
LIDRMLSINDEX     <- list(sensor = MLSLAS, index = LIDRAUTOINDEX)

#' @rdname lidR-spatial-index
#' @export
sensor = function(las, h = FALSE)
{
  if (isTRUE(h))
    index = format_index(las)
  else
    index = las@index

  return(index[["sensor"]])
}

#' @rdname lidR-spatial-index
#' @export
`sensor<-` = function(las, value)
{
  if (is.character(value))
  {
    assert_is_a_string(value)
    value <- tolower(value)
    match.arg(value, LIDRAQUISITIONDEVICES)
    value <- match(value, LIDRAQUISITIONDEVICES) - 1
  }
  else if (is.numeric(value))
  {
    assert_is_a_number(value)
    assert_all_are_in_closed_range(value, 0L, length(LIDRAQUISITIONDEVICES)-1)
    value <- as.integer(value)
  }
  else
  {
    stop("'value' must be an integer or a string.", call. = FALSE)
  }

  las@index[["sensor"]] <- value
  return(las)
}

#' @rdname lidR-spatial-index
#' @export
index = function(las, h = FALSE)
{
  if (isTRUE(h))
    index = format_index(las)
  else
    index = las@index

  return(index[["index"]])
}

#' @rdname lidR-spatial-index
#' @export
`index<-` = function(las, value)
{
  if (is.character(value))
  {
    assert_is_a_string(value)
    value <- tolower(value)
    match.arg(value, LIDRSPATIALINDEXES)
    value <- match(value, LIDRSPATIALINDEXES) - 1L
  }
  else if (is.numeric(value))
  {
    assert_is_a_number(value)
    assert_all_are_in_closed_range(value, 0L, length(LIDRSPATIALINDEXES)-1)
    value <- as.integer(value)
  }
  else
  {
    stop("'value' must be an integer or a string.", call. = FALSE)
  }

  las@index[["index"]] <- value
  return(las)
}

format_index = function(las)
{
  index = las@index
  sensor = index$sensor
  index = index$index

  sensor_names <- c("not registered", "ALS", "TLS", "UAV", "DAP", "mutispectral ALS", NA_character_, NA_character_, NA_character_, NA_character_,
                    "not registered (normalized)", "ALS (normalized)", "TLS (normalized)", "UAV (normalized)", "DAP (normalized)", "mutispectral ALS (normalized)", NA_character_, NA_character_, NA_character_, NA_character_)
  index_names <- c("automatic", "grid partition", "voxel partition", "quadtree", "octree")

  sensor <- sensor_names[sensor + 1]
  if (is.na(sensor)) stop("invalid sensor registred", call. = FALSE)

  index = index_names[index + 1]
  if (is.na(index)) stop("invalid spatial index registered", call. = FALSE)

  return(return(list(sensor = sensor, index = index)))
}

`force_autoindex<-` = function(las, value)
{
  if (index(las) == LIDRAUTOINDEX)
    index(las) <- value

  return(las)
}
