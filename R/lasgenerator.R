lasgenerator <- function(n, seeds = 1) {
  set.seed(seeds)
  X <- round_any(stats::runif(n, 0, 100), 0.001)
  set.seed(seeds + 1)
  Y <- round_any(stats::runif(n, 0, 100), 0.001)
  set.seed(seeds + 2)
  Z <- round_any(c(stats::runif(0.8 * n, 0, 25), rep(0, 0.2 * n)), 0.001)
  Classification <- as.integer(c(rep(1, 0.8 * n), rep(2, 0.2 * n)))
  set.seed(seeds + 3)
  Intensity <- as.integer(stats::runif(n, 10, 50))
  ReturnNumber <- as.integer(rep(c(1, 1, 1, 2, 3, 1, 2, 1, 2, 1), n / 10))
  NumberOfReturns <- as.integer(rep(c(1, 1, 3, 3, 3, 2, 2, 2, 2, 1), n / 10))

  data <- data.table::data.table(X, Y, Z, Classification, Intensity, ReturnNumber, NumberOfReturns)
  header <- rlas::header_create(data)
  header[["X scale factor"]] <- 0.001
  header[["Y scale factor"]] <- 0.001
  header[["Z scale factor"]] <- 0.001
  header[["X offset"]] <- 0
  header[["Y offset"]] <- 0
  header[["Z offset"]] <- 0
  las <- suppressMessages(suppressWarnings(LAS(data, header, check = FALSE)))
  epsg(las) <- 2008
  return(las)
}

catalog_generator <- function(n, size = 100, seed = 1) {
  X <- Y <- NULL

  # Generate a catalog
  xshift <- c(0, size, 0, size)
  yshift <- c(0, 0, size, size)
  shift  <- cbind(xshift, yshift)
  temp   <- sapply(1:nrow(shift), function(x) tempfile(fileext = ".las"))

  for (i in 1:nrow(shift))
  {
    las <- lasgenerator(n, seeds = i + seed)
    las@data[, X := X + shift[i,1]]
    las@data[, Y := Y + shift[i,2]]
    writeLAS(las, temp[i])
  }

  ctg <- readLAScatalog(temp)
  opt_chunk_buffer(ctg)    <- 0
  opt_chunk_size(ctg)      <- 0
  opt_progress(ctg)        <- FALSE
  opt_output_files(ctg)    <- ""

  return(ctg)
}
