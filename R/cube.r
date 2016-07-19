#' @importFrom magrittr %>%
#' @importFrom rgl shade3d
cube <- function(x, y, z, col, scale = 1)
{

  mycube <- rgl::cube3d()

  # Reduce size to unit
  mycube$vb[4,] <- mycube$vb[4,]/scale*2

  for (i in 1:length(x))
  {
    # Add cube border
    bcube <- mycube
    bcube$material$lwd <- 2
    bcube$material$front <- 'line'
    bcube$material$back <- 'line'
    bcube %>% rgl::translate3d(x[i], y[i], z[i]) %>% shade3d

    # Add cube fill
    fcube <- mycube
    fcube$vb[4,] <- fcube$vb[4,]*1.01
    fcube$material$col <- col[i]
    fcube %>% rgl::translate3d(x[i], y[i], z[i]) %>% shade3d
  }
}