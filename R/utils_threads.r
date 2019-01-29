LIDRTHREADS = new.env()
LIDRTHREADS$n = 1L

#' Set or get number of threads that lidR should use
#'
#' Set and get number of threads to be used in lidR functions that are parallelized with OpenMP.
#' Default value 0 means to utilize all CPU available. \code{get_lidr_threads()} returns the number
#' of threads that will be used. This affects \code{lidR} package but also the \code{data.table} package
#' by internally calling \link[data.table:setDTthreads]{setDTthreads} because several functions  of
#' lidR rely on \code{data.table} but it does not change R itself or other packages using OpenMP.
#'
#' This option come in replacement of the former \code{LAScatalog} processing option \code{opt_cores}.
#' The former options enabled to process several chunks at once by loading more data in RAM
#' with a strong overhead but point-cloud processing was not actually parallelised. lidR becomes more
#' parallel internally and thus multithreading is natively used both when processing a point-cloud (LAS
#' objects) or a catalog (LAScatalog objects).
#'
#' @param threads An integer >= 0. Default 0 means use all CPU available and leave the operating system
#' to multi task.
set_lidr_threads = function(threads)
{
  assert_is_a_number(threads)

  max <- R_omp_get_max_threads()

  if (max < 0)
  {
    message("This installation of lidR has not been compiled with OpenMP support")
    LIDRTHREADS$n <- 1L
    data.table::setDTthreads(1L)
    return(invisible())
  }

  if (threads == 0 | threads > max)
  {
    LIDRTHREADS$n <- max
    data.table::setDTthreads(0L)
  }
  else
  {
    LIDRTHREADS$n <- as.integer(threads)
    data.table::setDTthreads(threads)
  }

  return(invisible())
}

#' @rdname set_lidr_threads
#' @export
get_lidr_threads = function()
{
  return(LIDRTHREADS$n)
}

getThread = get_lidr_threads

