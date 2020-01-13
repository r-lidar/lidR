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
#' @seealso \link{lidR-parallelism}
#'
#' @param threads An integer >= 0. Default 0 means use all CPU available and leave the operating system
#' to multi task.
#' @export
set_lidr_threads = function(threads)
{
  assert_is_a_number(threads)

  max <- R_omp_get_max_threads()

  if (max < 0)
  {
    message("This installation of lidR has not been compiled with OpenMP support") # nocov
    LIDRTHREADS$n <- 1L # nocov
    data.table::setDTthreads(1L) # nocov
    return(invisible()) # nocov
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

get_future_workers = function()
{
  n <- formals(future::plan())$workers
  if (is.null(n) || is.character(n)) # e.g. future::plan(future::remote(), workers = "localhost")
    return(1L) # nocov
  else if (is.call(n))               # e.g. future::plan(future::multisession())
    return(future::availableCores())
  else                               # e.g. future::plan(future::multisession(), workers = 2L)
    return(n)  # nocov
}

getThread <- get_lidr_threads
getThreads <- get_lidr_threads
getWorkers <- get_future_workers
setThreads <- set_lidr_threads

