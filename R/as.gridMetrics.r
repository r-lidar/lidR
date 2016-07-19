#' Set the class gridMetrics to a data.frame or a data.table
#'
#' Set the class gridMetrics to a data.frame. Useful when reading data from a file.
#' In this case the data.frame does not have the class gridMetrics and cannot easly be
#' plotted or transformed into a matrix.
#'
#' @param x A data.frame
#' @importFrom reshape2 acast
#' @export as.gridMetrics
#' @importFrom magrittr %>%
#' @importFrom data.table as.data.table
as.gridMetrics = function(x)
{
  if(is.data.frame(x))
  {
    x = as.data.table(x)
    attr(x, "class") = c("gridMetrics", attr(x, "class"))
  }

  return(x)
}