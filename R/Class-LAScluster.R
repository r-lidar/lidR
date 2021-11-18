setClass(
  Class = "LAScluster",
  representation(
    center = "list",
    bbox   = "matrix",
    bbbox  = "matrix",
    width  = "numeric",
    height = "numeric",
    buffer = "numeric",
    shape  = "numeric",
    select = "character",
    filter = "character",
    wkt    = "character",
    files  = "character",
    name   = "character",
    save   = "character",
    alt_dir = "character",
    crs     = "crs",
    index   = "list"
  )
)

setMethod("initialize", "LAScluster", function(.Object, center, width, height, buffer, shape, files, name, wkt, crs, index)
{
  hw = width/2
  hh = height/2
  xc = center$x
  yc = center$y

  .Object@center <- center
  .Object@bbox   <- matrix(c(xc - hw, yc - hh, xc + hw, yc + hh), ncol = 2)
  .Object@bbbox  <- .Object@bbox + buffer * matrix(c(-1, -1, +1, +1), ncol = 2)
  .Object@width  <- width  + 2 * buffer
  .Object@height <- height + 2 * buffer
  .Object@buffer <- buffer
  .Object@shape  <- shape
  .Object@name   <- name
  .Object@files  <- files
  .Object@save   <- ""
  .Object@wkt    <- ""
  .Object@crs    <- crs
  .Object@index  <- index

  if (shape == LIDRCIRCLE)
    .Object@filter = paste("-inside_circle", xc, yc, hw + buffer)
  else if (shape == LIDRRECTANGLE)
    .Object@filter = paste("-inside", .Object@bbbox[1], .Object@bbbox[2], .Object@bbbox[3], .Object@bbbox[4])
  else
    stop("Something went wrong internally initializing a cluster. Process aborted.")

  return(.Object)
})
