LIDRSPATIALINDEXES <- c("auto", "gridpartition", "voxelpartition", "quadtree", "octree")
LIDRAUTOINDEX      <- 0L
LIDRGRIDPARTITION  <- 1L
LIDRVOXELPARTITION <- 2L
LIDRQUADTREE       <- 3L
LIDROCTREE         <- 4L

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

  if (sensor == UKNLAS)
    sensor = "not registered"
  else if (sensor == ALSLAS)
    sensor = "ALS"
  else if (sensor == TLSLAS)
    sensor = "TLS"
  else if (sensor == UAVLAS)
    sensor = "UAV"
  else if (sensor == DAPLAS)
    sensor = "DAP"
  else if (sensor == MLSLAS)
    sensor = "mutispectral ALS"
  else if (sensor == UKNLAS + NLAS)
    sensor = "not registered (normalized)"
  else if (sensor == ALSLAS + NLAS)
    sensor = "ALS (normalized)"
  else if (sensor == TLSLAS + NLAS)
    sensor = "TLS (normalized)"
  else if (sensor == UAVLAS + NLAS)
    sensor = "UAV (normalized)"
  else if (sensor == DAPLAS + NLAS)
    sensor = "DAP (normalized)"
  else if (sensor == MLSLAS + NLAS)
    sensor = "mutispectral ALS (normalized)"
  else
    stop("invalid sensor registred", call. = FALSE)

  if (index == LIDRAUTOINDEX)
    index = "automatic"
  else if (index == LIDRGRIDPARTITION)
    index = "grid partition"
  else if (index == LIDRVOXELPARTITION)
    index = "voxel partition"
  else if (index == LIDRQUADTREE)
    index = "quadtree"
  else if (index == LIDROCTREE)
    index = "octree"
  else
    stop("invalid spatial index registered", call. = FALSE)

  return(return(list(sensor = sensor, index = index)))
}

`force_autoindex<-` = function(las, value)
{
  if (index(las) == LIDRAUTOINDEX)
    index(las) <- value

  return(las)
}
