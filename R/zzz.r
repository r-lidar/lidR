.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (interactive())
  {
    v = utils::packageVersion("lidR")
    packageStartupMessage("lidR ", v, ". For help, for the most up-to-date version or to report a bug, visit https://github.com/Jean-Romain/lidR")
  }
}

.onUnload <- function (libpath) {
  library.dynam.unload("lidR", libpath)
}