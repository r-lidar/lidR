#' Rasterize a triangular irregular network
#'
#' Rasterize a triangular irregular network
#'
#' This function is in development and is currently very slow.
#'
#' @param tin An object of class \code{TIN}
#' @param res scalar. the resolution of the rasterization
#' @return A data.table containing the x, y and z coordinates of the cells
#' @examples
#' x = runif(20)
#' y = runif(20)
#' z = runif(20)
#' tin = TIN(x, y, z)
#' rasterTIN = rasterizeTIN(tin, 0.05)
#' plot(rasterTIN)
#' @note Still in development. Currently very slow.
#' @seealso
#' \link[lidR:TIN]{TIN}
#' @export rasterizeTIN
#' @importFrom magrittr %>% %<>% %$%
#' @importFrom data.table setnames as.data.table
rasterizeTIN <- function(tin, res)
{
  minx = min(tin$x1, tin$x2, tin$x3)
  maxx = max(tin$x1, tin$x2, tin$x3)
  miny = min(tin$y1, tin$y2, tin$y3)
  maxy = max(tin$y1, tin$y2, tin$y3)

  x = seq(minx, maxx, res)
  y = seq(miny, maxy, res)

  gri = expand.grid(x,y)
  names(gri) = c("x", "y")
  gri %<>% as.data.table

  cat("\nTIN interpolation :", dim(tin)[1],"\n")

  p <- dplyr::progress_estimated(dim(tin)[1])

  ret <- apply(tin, 1,
      function(x)
      {
        p$tick()$print()
        A = c(x[1], x[4], x[7])
        B = c(x[2], x[5], x[8])
        C = c(x[3], x[6], x[9])

        .triangle_interpolation(A,B,C, gri)
      })

  ret <- do.call(rbind, ret)

  setnames(ret, c("X", "Y", "Z"))

  ret %<>% unique(by=c("X", "Y"))

  return(ret)
}