#' Automatic colorization
#'
#' Attribute a color to each element of a vector
#'
#' @param x A vector.
#' @param palette function. A color palette function. Default is \code{height.colors} provided by the package lidR.
#' @param trim numeric.
#' @keywords internal
#' @noRd
set.colors = function(x, palette, trim = Inf, value_index = FALSE)
{
  if (all(is.na(x)))
    return()

  # Classification case
  if (value_index)
  {
    x[x >= length(palette)] <- length(palette) - 1
    return(palette[x + 1])
  }

  ncolors <- length(palette)
  if (!is.infinite(trim)) x[x > trim] <- trim
  minx <- min(x, na.rm = T)
  maxx <- max(x, na.rm = T)

  if (maxx - minx == 0) {
    if (!anyNA(x)) {
      colors <- palette[1]
    } else {
      colors <- rep(NA_character_, length(x))
      colors[!is.na(x)] <- palette[1]
    }
  } else {
    idx <- findInterval(x, seq(minx, maxx, length.out = ncolors))
    colors <- palette[idx]
  }

  return(colors)
}

#' @param n The number of colors (> 1) to be in the palette
#' @export
#' @rdname plot
height.colors = function(n)
{
  colfunc <- grDevices::colorRampPalette(c("blue", "cyan2", "yellow", "red"))
  return(colfunc(n))
}

#' @export
#' @rdname plot
forest.colors = function(n)
{
  colfunc <- grDevices::colorRampPalette(c("darkgreen", "lightgreen"))
  return(colfunc(n))
}

amplitude.color = function(n)
{
  colfunc <- grDevices::colorRampPalette(rev(c("#ff0000", "#ff1919", "#ff3333", "#ff4d4d", "#ff6666", "#ff8080", "#ff9999", "#ffb3b3", "#ffcccc", "#ffe6e6")))
  return(colfunc(n))
}

#' @export
#' @rdname plot
random.colors = function(n)
{
  h = stats::runif(n, 0, 1);
  s = stats::runif(n, 0.2, 1);
  l = stats::runif(n, 0.8, 1);

  return(grDevices::hsv(h,s,l))
}

#' @export
#' @rdname plot
pastel.colors = function(n)
{
  h = stats::runif(n, 0, 360);
  c = stats::runif(n, 42, 98);
  l = stats::runif(n, 40, 90);

  return(grDevices::hcl(h,c,l))
}

lasclass.colors = function(...)
{
  return(c("lightgrey",   # 0 never classifiied
           "lightgray",   # 1 unclassified
           "blue",        # 2 ground
           "limegreen",   # 3 low vegetation
           "forestgreen", # 4 medium vegetation
           "darkgreen",   # 5 high vegetation
           "red",         # 6 building
           "yellow",      # 7 low point (noise)
           "yellow",      # 8 reserved
           "#6495ED",     # 9 water
           "yellow",      # 10 rail
           "gray20",      # 11 Road surface
           "yellow",      # 12 reserved
           "pink",        # 13 wire
           "pink",        # 14 wire
           "purple",      # 15 transmission tower
           "pink",        # 16 Wire connector
           "orange",      # 17 bridge deck
           "yellow"))     # +
}
