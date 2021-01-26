LIDRTHREADS = new.env()
LIDRTHREADS$n = 1L

#' Set or get number of threads that lidR should use
#'
#' Set and get number of threads to be used in lidR functions that are parallelized with OpenMP.
#' Default value 0 means to utilize all CPU available. \code{get_lidr_threads()} returns the number
#' of threads that will be used. This affects \code{lidR} package but also the \code{data.table} package
#' by internally calling \link[data.table:openmp-utils]{setDTthreads} because several functions  of
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
  assert_all_are_non_negative(threads)

  max <- R_omp_get_max_threads()

  if (max < 0)
  {
    if (threads > 1) message("This installation of lidR has not been compiled with OpenMP support") # nocov
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
  # If NULL returned it means that I'm not able to know if the plan involves local workers
  # or complex architecture such as remote computers or multiple node on a HPC

  # nocov start

  strategy <- future::plan()
  n <- formals(strategy)$workers

  if (is(strategy, "sequential"))
  {
    verbose("Parallel strategy: sequential")
    return(1L)
  }

  if (is(strategy, "remote")) # e.g. plan(remote(), workers = "localhost")
  {
    verbose("Parallel strategy: remote")
    return(1L)
  }

  if (is(strategy, "cluster")) # e.g. plan(multisession or  plan(cluster) or plan(cluster, workers =  makeCluster(3, type='SOCK'))
  {
    verbose("Parallel strategy: cluster")

    if (is.numeric(n))            # e.g. plan(multisession, workers = 2L)
      return(n)

    if (is.call(n))               # e.g. plan(multisession or plan(cluster)
    {
       n <- eval(n)
       if (is.numeric(n))
         return(n)
       else
         return(NULL)
    }

    return(NULL)
  }

  return(NULL)

 # nocov end
}


# Because I made some typos and I rebamed stuff and I did
# not check the code yet
getThread <- get_lidr_threads
getThreads <- get_lidr_threads
getWorkers <- get_future_workers
setThreads <- set_lidr_threads

must_disable_openmp = function()
{
  if (getOption("lidR.threads.manual") == TRUE)
    return(FALSE)

  workers    <- getWorkers()
  threads    <- getThreads()
  cores      <- future::availableCores()

  if (is.null(workers))
  {
    warning("The parallel evaluation strategy was no recognized and lidR does not know if OpenMP should be disabled.
OpenMP has been disabled by security. Use option(lidR.threads.manual = TRUE) and set_lidr_threads() for a fine control of parallelism.", call. = FALSE)
    return(TRUE)
  }

  workers * threads > cores

  if (workers * threads > cores)
  {
    # nocov because tested with a single core on CRAN
    verbose(glue::glue("Cannot nest {workers} future threads and {threads} OpenMP threads. Precedence given to future: OpenMP threads set to 1.")) # nocov
    return(TRUE)
  }

  return(FALSE)
}

