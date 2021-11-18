LIDRTHREADS = new.env()
LIDRTHREADS$n = 1L
LIDRTHREADS$input <- 1L

#' Set or get number of threads that lidR should use
#'
#' Set and get number of threads to be used in lidR functions that are parallelized with OpenMP.
#' 0 means to utilize all CPU available. \code{get_lidr_threads()} returns the number
#' of threads that will be used. This affects \code{lidR} package but also the \code{data.table} package
#' by internally calling \link[data.table:openmp-utils]{setDTthreads} because several functions  of
#' lidR rely on \code{data.table} but it does not change R itself or other packages using OpenMP.
#'
#' @seealso \link{lidR-parallelism}
#'
#' @param threads Positive scalar. Default 0 means use all CPU available. Values > 1 mean
#' using n cores, values in ]0, 1[ mean using a fraction of the cores e.g. 0.5 = half.
#' @export
set_lidr_threads = function(threads)
{
  assert_is_a_number(threads)
  assert_all_are_non_negative(threads)

  max <- R_omp_get_max_threads()

  if (max < 0)
  {
    if (threads > 1) message("This installation of lidR has not been compiled with OpenMP support") # nocov
    LIDRTHREADS$input <- threads
    LIDRTHREADS$n <- 1L # nocov
    data.table::setDTthreads(1L) # nocov
    return(invisible()) # nocov
  }

  if (threads == 0 | threads > max)
  {
    LIDRTHREADS$input <- threads
    LIDRTHREADS$n <- max
    data.table::setDTthreads(0L)
  }
  else if(threads < 1)
  {
    LIDRTHREADS$input <- threads
    LIDRTHREADS$n <- as.integer(max*threads)
    data.table::setDTthreads(as.integer(max*threads))
  }
  else
  {
    LIDRTHREADS$input <- as.integer(threads)
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

#' Counts the number of cores used by simple multi-core future::plan()s
#'
#' lidR employs two different parallelization mechanisms: OpenMP for
#' multi-threaded execution of certain algorithms and the future API for
#' processing LAScatalog tiles in parallel. Since the future API can be used for
#' different kinds of parallelization (including multi-threading but also
#' parallel execution on computer clusters), it is possible for OpenMP and
#' future to have incompatible settings. Example: A user has a single machine
#' with four cores and sets both, the number of OpenMP threads and the number of
#' future processes to four. In this case OpenMP has to be disabled so that it
#' doesn't interfere with the future parallelization.
#'
#' Essentially, this function analyzes the argument to the 'workers' parameter
#' of future::plan(). When this argument is either 1) an integer, 2) a character
#' vector of strings all indicating the "localhost", or 3) a function call which
#' returns 1) or 2), then the integer or the number of localhost strings is
#' returned as the number of used cores. Currently, other cases cannot be
#' evaluated.
#'
#' @return A single integer giving the number of cores or NULL if the current
#'   future::plan() could not be evaluated. The latter might be the case when a
#'   future::plan() involves complex architectures such as remote computers or
#'   multiple nodes on a HPC.
#' @noRd
try_to_get_num_future_cores = function()
{
  # nocov start

  # get the current plan
  plan <- future::plan()

  # if the plan is explicitly single-threaded, return 1
  if (is(plan, "uniprocess")) { return(1L) }
  # "uniprocess" covers future::sequential and future::transparent and hopefully
  # also single-threaded plans provided by other packages

  # if there is no argument called "workers", return NULL
  if (!"workers" %in% names(formals(plan))) { return(NULL) }

  # get the value of the "workers" argument
  workers_arg <- formals(plan)$workers

  # These values might be used to reference the current machine and are compared
  # to the "workers" argument in the code below
  localhosts <- c("localhost", "127.0.0.1", Sys.info()[["nodename"]])
  # taken from the documentation of parallelly::makeClusterPSOCK and
  # parallelly::makeNodePSOCK hoping that they will also be valid for other
  # interfaces

  # Now check whether a number of locally used cores can be extracted from the
  # "workers" argument

  # if "workers" is a positive number, return that number
  if (is.numeric(workers_arg) &&
      length(workers_arg) == 1L &&
      !is.na(workers_arg) &&
      workers_arg >= 1)
  {
    return(as.integer(workers_arg))
  }

  # if "workers" is a character vector of "localhosts", return the vector length
  if (is.character(workers_arg) &&
      length(workers_arg) >= 1L &&
      all(!is.na(workers_arg)) &&
      all(workers_arg %in% localhosts))
  {
    return(length(workers_arg))
  }

  # if "workers" is a function call, check whether the return value is one of
  # the above tested options
  if (is.call(workers_arg))
  {
    # try to evaluate the function call
    evaluated_workers_arg <- tryCatch(
      eval(workers_arg),
      error = function(dummy) { return(NULL) } # on error: return NULL
    )

    if (is.null(evaluated_workers_arg)) # if the "normal" evaluation didn't work
    {
      # Try to evaluate the function call in the environment of the future
      # package. This deals with situations where functions cannot be evaluated
      # because the future package has not been loaded (using library(future),
      # library(lidR), or others).
      evaluated_workers_arg <- tryCatch(
        # use an arbitrary future function to get the environment of the future package
        eval(workers_arg, envir = environment(future::plan)),
        error = function(dummy) { return(NULL) }
      )
    }

    # if the call could not be evaluated, return NULL
    if (is.null(evaluated_workers_arg)) { return(NULL) }

    # If the call could be evaluated, check if the result is a number or a
    # vector of localhosts.
    if (is.numeric(evaluated_workers_arg) &&
        length(evaluated_workers_arg) == 1L &&
        !is.na(evaluated_workers_arg) &&
        evaluated_workers_arg >= 1)
    {
      return(as.integer(evaluated_workers_arg))
    }

    if (is.character(evaluated_workers_arg) &&
        length(evaluated_workers_arg) >= 1L &&
        all(!is.na(evaluated_workers_arg)) &&
        all(evaluated_workers_arg %in% localhosts))
    {
      return(length(evaluated_workers_arg))
    }

  } # end if (is.call(workers_arg))

  # -> "workers" is neither a number, nor a character vector of localhosts,
  # nor a function call returning any of these values.
  return(NULL)

  # nocov end
}


# Because I made some typos and I renamed stuff and I did
# not check the code yet
getThread <- get_lidr_threads
getThreads <- get_lidr_threads
getWorkers <- try_to_get_num_future_cores
setThreads <- set_lidr_threads

must_disable_openmp = function()
{
  if (!engine_use_future()) return(FALSE)
  if (isTRUE(getOption("lidR.threads.manual"))) return(FALSE) # Backward compatibility
  if (isFALSE(getOption("lidR.check.nested.parallelism"))) return(FALSE)

  workers    <- getWorkers()
  threads    <- getThreads()
  cores      <- future::availableCores()

  if (is.null(workers))
  {
    warning("The parallel evaluation strategy was not recognized and lidR does not know if OpenMP should be disabled.
      OpenMP has been disabled by security.
      Use options(lidR.check.nested.parallelism = FALSE) and set_lidr_threads() for a fine control of parallelism.", call. = FALSE)
    return(TRUE)
  }

  if (workers * threads > cores)
  {
    # nocov because tested with a single core on CRAN
    verbose(glue::glue("Cannot nest {workers} future threads and {threads} OpenMP threads. Precedence given to future: OpenMP threads set to 1.")) # nocov
    return(TRUE)
  }

  return(FALSE)
}

