#' Write a .las or .laz file
#'
#' Write a \link[=LAS-class]{LAS} object into a binary .las or .laz file (compression
#' specified in filename)
#'
#' @param las an object of class LAS.
#' @param file character. A character string naming an output file.
#' @param index boolean. Also write a lax file to index the points in the files
#'
#' @return Nothing. This function is used for its side-effect of writing a file.
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' subset = clip_rectangle(las, 684850, 5017850, 684900, 5017900)
#' writeLAS(subset, tempfile(fileext = ".laz"))
writeLAS = function(las, file, index = FALSE)
{
  stopifnotlas(las)
  assert_is_a_string(file)
  assert_is_a_bool(index)

  if (is.empty(las)) stop("Cannot write a file with 0 point", call. = FALSE)

  file  = path.expand(file)
  islas = tools::file_ext(file) %in% c("las", "laz")

  if (!islas) stop(glue::glue("File(s) {file} not supported"), call. = FALSE)

  rlas::write.las(file, as.list(las@header), las@data)

  if (index) rlas::writelax(file)

  return(invisible(file))
}
