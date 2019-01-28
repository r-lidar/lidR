LIDRTHREADS = new.env()
LIDRTHREADS$n = 1L

#' Set or get number of threads that lidR should use
#'
#' @export
set_lidr_threads = function(n)
{
  max = R_omp_get_max_threads()

  if (max < 0)
  {
    message("This installation of lidR has not been compiled with OpenMP support")
    LIDRTHREADS$n <- 1L
    return(invisible())
  }

  if (n == 0 | n > max)
    LIDRTHREADS$n <- max
  else
    LIDRTHREADS$n <- as.integer(n)

  return(invisible())
}

#' @rdname set_lidr_threads
#' @export
get_lidr_threads = function()
{
  return(LIDRTHREADS$n)
}

getThread = get_lidr_threads

