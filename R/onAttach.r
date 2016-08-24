.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (interactive())
  {
    v = packageVersion("lidR")

    packageStartupMessage("lidR ", v, ". For help or up-to-date version check https://github.com/Jean-Romain/lidR")
  }
}