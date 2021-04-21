#' @section Full waveform:
#' With most recent versions of the `rlas` package, full waveform (FWF) can be read and `lidR`
#' provides some compatible functions. However the support of FWF is still a work in progress
#' in the `rlas` package. How it is read, interpreted and represented in R may change. Consequently,
#' tools provided by `lidR` may also change until the support of FWF becomes mature and
#' stable in `rlas`. See also \link[rlas:read.las]{rlas::read.las}.\cr\cr
#' Remember that FWF represents an insanely huge amount of data. It terms of memory it is like
#' having between 10 to 100 times more points. Consequently loading FWF data in R should be
#' restricted to relatively small point clouds.
