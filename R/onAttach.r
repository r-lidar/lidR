.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (interactive())
  {
    v = utils::packageVersion("lidR")
    packageStartupMessage("lidR ", v, ". For help, for up-to-date version, to report a bug visit https://github.com/Jean-Romain/lidR")
  }
}