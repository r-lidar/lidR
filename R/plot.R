#' Plot a LAS* object
#'
#' Plot displays a 3D interactive windows based on rgl for \link{LAS} objects\cr\cr
#' Plot displays an interactive view for \link[=LAScatalog-class]{LAScatalog} objects with pan and
#' zoom capabilities based on `mapview()` from package `mapview`. If the coordinate reference
#' system (CRS) of the \code{LAScatalog} is non empty, the plot can be displayed on top of base maps
#' (satellite data, elevation, street, and so on).\cr\cr
#' Plot displays a \link[=LASheader-class]{LASheader} object exactly like it displays a LAScatalog
#' object.
#'
#'
#' @param x A \code{LAS*} object
#' @param y Unused (inherited from R base)
#' @param color characters. The attribute used to color the point cloud. Default is Z coordinates. RGB
#' is an allowed string even if it refers to three attributes simultaneously.
#' @param pal palette function, similar to heat.colors, or palette values. Default is \code{"auto"}
#' providing an automatic coloring depending on the attribute \code{color}
#' @param bg The color for the background. Default is black.
#' @param clear_artifacts logical. It is a known and documented issue that the 3D visualisation with
#' \code{rgl} displays artifacts. The points look aligned and/or regularly spaced in some view angles.
#' This is because \code{rgl} computes with single precision \code{float}. To fix that the point
#' cloud is shifted to (0,0) to reduce the number of digits needed to represent its coordinates.
#' The drawback is that the point cloud is not plotted at its actual coordinates.
#' @param breaks either a numeric vector with the actual breaks, or a name of a method accepted
#' by the style argument of \link[classInt:classIntervals]{classIntervals}
#' @param nbreaks Number of colors breaks.
#' @param axis logical. Display axis on XYZ coordinates.
#' @param legend logical. Display a gradient colour legend.
#' @param backend character. Can be \code{"rgl"} or \code{"lidRviewer"}. If \code{"rgl"} is chosen
#' the display relies on the \code{rgl} package. If \code{"lidRviewer"} is chosen it relies on the
#' \code{lidRviewer} package, which is much more efficient and can handle million of points
#' using less memory. \code{lidRviewer} is not available on CRAN yet and should
#' be installed from github (see. \url{https://github.com/r-lidar/lidRviewer}).
#' @param add If \code{FALSE} normal behaviour otherwise must be the output of a prior plot function
#' to enable the alignment of a second point cloud.
#' @param voxel boolean or numeric. Displays voxels instead of points. Useful to render the output
#' of \link{voxelize_points}, for example. However it is computationally demanding to render and can
#' easily take 15 seconds for 10000 voxels. It should be reserved for small scenes. If boolean the voxel
#' resolution is guessed automatically. Otherwise users can provide the size of the voxels. To reduce the rendering time,
#' an internal optimization removes voxels that are not visible when surrounded by other voxels.
#' @param NAcol a color for NA values.
#'
#' @param mapview logical. If \code{FALSE} the catalog is displayed in a regular plot from R base.
#' Since v4.0.4 `mapview = TRUE` is also possible with LAS objects.
#' @param chunk_pattern logical. Display the current chunk pattern used to process the catalog.
#' @param overlaps logical. Highlight the overlaps between files.
#'
#' @param ... Will be passed to \link[rgl:3dobjects]{points3d} (LAS) or \link[graphics:plot.default]{plot}
#' if \code{mapview = FALSE} or to `mapview()` if \code{mapview = TRUE} (LAScatalog).
#'
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' plot(las)
#' plot(las, color = "Intensity")
#' plot(las, color = "ScanAngleRank", pal = rainbow)
#'
#' # If outliers break the color range, use the breaks parameter
#' las$Intensity[150] <- 1000L
#' plot(las, color = "Intensity")
#' plot(las, color = "Intensity", breaks = "quantile", nbreaks = 50)
#'
#' plot(las, color = "Classification")
#'
#' # This dataset is already tree segmented
#' plot(las, color = "treeID")
#' plot(las, color = "treeID", pal = random.colors)
#'
#'
#' # single file LAScatalog using data provided in lidR
#' ctg = readLAScatalog(LASfile)
#' plot(ctg)
#' plot(ctg, map = T, map.types = "Esri.WorldImagery")
#' }
#'
#' @export
#' @method plot LAS
setGeneric("plot", function(x, y, ...)
  standardGeneric("plot"))

#' @rdname plot
setMethod("plot", signature(x = "LAS", y = "missing"),
          function(x, y, ...,
                   color = "Z",
                   pal = "auto",
                   bg = "black",
                   breaks = "pretty",
                   nbreaks = "auto",
                   backend = "rgl",
                   clear_artifacts = TRUE,
                   axis = FALSE,
                   legend = FALSE,
                   add = FALSE,
                   voxel = FALSE,
                   NAcol = "lightgray",
                   mapview = FALSE)
{
  plot.LAS(x, y, ..., color = color, pal = pal, bg = bg, breaks = breaks, nbreaks = nbreaks, backend = backend, clear_artifacts = clear_artifacts, axis = axis, legend = legend, add = add, voxel = voxel, NAcol = NAcol, mapview = mapview)
})

#' @export
#' @rdname plot
setMethod("plot", signature(x = "LAScatalog", y = "missing"), function(x, y, mapview = FALSE, chunk_pattern = FALSE, overlaps = FALSE, ...)
{
  plot.LAScatalog(x, y, mapview, chunk_pattern, overlaps, ...)
})

#' @export
#' @rdname plot
setMethod("plot", signature(x = "LASheader", y = "missing"), function(x, y, mapview = FALSE, ...)
{
  PHB  <- x@PHB
  crs  <- st_crs(x)
  xmin <- PHB[["Min X"]]
  xmax <- PHB[["Max X"]]
  ymin <- PHB[["Min Y"]]
  ymax <- PHB[["Max Y"]]
  mtx  <- matrix(c(xmin, xmax, ymin, ymax)[c(1, 1, 2, 2, 1, 3, 4, 4, 3, 3)], ncol = 2)
  geom <- sf::st_polygon(list(mtx))
  geom <- sf::st_sfc(geom)
  sf::st_crs(geom) <- crs

  names(PHB) <- make.names(names(PHB))

  if (!is.null(PHB[["Number.of.points.by.return"]]))
  {
    PHB[["Number.of.1st.return"]] <- PHB[["Number.of.points.by.return"]][1]
    PHB[["Number.of.2nd.return"]] <- PHB[["Number.of.points.by.return"]][2]
    PHB[["Number.of.3rd.return"]] <- PHB[["Number.of.points.by.return"]][3]
    PHB[["Number.of.4th.return"]] <- PHB[["Number.of.points.by.return"]][4]
    PHB[["Number.of.5th.return"]] <- PHB[["Number.of.points.by.return"]][5]
    PHB[["Number.of.points.by.return"]] <- NULL
    PHB[["Global.Encoding"]] <- NULL
  }

  data <- data.table::as.data.table(PHB)
  data <- sf::st_set_geometry(data, geom)

  res <- new("LAScatalog")
  res@data <- data

  plot.LAScatalog(res, mapview = mapview, ...)
})

plot.LAScatalog = function(x, y, mapview = FALSE, chunk_pattern = FALSE, overlaps = FALSE, ...)
{
  assert_is_a_bool(mapview)
  assert_is_a_bool(chunk_pattern)
  assert_is_a_bool(overlaps)

  if (mapview)
  {
    if (!requireNamespace("mapview", quietly = TRUE))
    {
      message("'mapview' is required to display the LAScatalog interactively.") # nocov
      mapview <- FALSE # nocov
    }

    if (overlaps)
      message("overlaps = TRUE is not supported yet with mapview") # nocov
  }

  if (mapview)
  {
    mapview::mapview(x@data, ...)
  }
  else if (chunk_pattern)
  {
    opt_progress(x) <- TRUE
    opt_output_files(x) <- ""
    engine_chunks(x)
    return(invisible())
  }
  else
  {
    # New feature from v2.2.0 to do not process some tiles
    process <- x@data$process
    if (is.null(process)) process <- rep(TRUE, nrow(x@data))
    if (!is.logical(process)) {
      warning("The attribute 'process' of the catalog is not logical.", call. = FALSE)
      process <- rep(TRUE, nrow(x@data))
    }

    alpha   <- ifelse(process, 0.15, 0.03)
    param   <- list(...)
    xmin    <- min(x@data$Min.X)
    xmax    <- max(x@data$Max.X)
    ymin    <- min(x@data$Min.Y)
    ymax    <- max(x@data$Max.Y)
    xcenter <- (xmin + xmax)/2
    ycenter <- (ymin + ymax)/2
    col    <- grDevices::rgb(0, 0, 1, alpha = alpha)

    if (is.null(param$xlim)) param$xlim <- c(xmin, xmax)
    if (is.null(param$ylim)) param$ylim <- c(ymin, ymax)
    if (is.null(param$xlab)) param$xlab <- ""
    if (is.null(param$ylab)) param$ylab <- ""
    if (is.null(param$asp))  param$asp  <- 1
    if (!is.null(param$col)) col <- param$col
    param$map.types = NULL

    param$col <- "white"
    param$x   <- xcenter
    param$y   <- ycenter

    op <- graphics::par(mar = c(2.5,2.5,1,1) + 0.1)

    if (is.null(param$add)) do.call(graphics::plot, param)

    if (!isTRUE(attr(x, "trueshape")))
    {
      graphics::rect(x@data$Min.X, x@data$Min.Y, x@data$Max.X, x@data$Max.Y, col = col)
      graphics::par(op)
    }
    else
    {
      plot(sf::st_geometry(sf::st_as_sf(x)), axes = TRUE)
      graphics::par(op)
    }

    if (overlaps) {
      plot(catalog_overlaps(x), add = T, col = "red", border = "red")
    }

    return(invisible())
  }
}

plot.LAS = function(x, y, ...,
                    color = "Z",
                    pal = "auto",
                    bg = "black",
                    breaks = "pretty",
                    nbreaks = "auto",
                    backend = "rgl",
                    clear_artifacts = TRUE,
                    axis = FALSE,
                    legend = FALSE,
                    add = FALSE,
                    voxel = FALSE,
                    NAcol = "lightgray",
                    mapview = FALSE)
{
  if (mapview)
  {
    return(plot(header(x), mapview = TRUE))
  }

  args <- list(...)
  if (is.null(args$size))
    args$size <- 1.5

  # Backward compatibility
  #nocov start
  if (!is.null(args$colorPalette))
  {
    if (length(args$colorPalette) == 1L && args$colorPalette == "auto")
      pal <- "auto"
    else
      pal <- grDevices::colorRampPalette(args$colorPalette)

    args$colorPalette <- NULL
    message("The argument 'coloPalette' is deprecated. Use 'pal' instead")
  }

  # Backward compatibility
  if (!is.null(args$trim))
  {
    args$trim <- NULL
    message("The argument 'trim' is not longer supported. Use breaks = 'quantile' instead")
  }

  # Backward compatibility
  if (!is.null(args$nbits))
  {
    args$nbits <- NULL
    message("The argument 'nbits' is not longer supported. It is now infered automatically.")
  }
  #nocov end

  backend <- match.arg(backend, c("rgl", "lidRviewer"))
  use_pcv <- backend == "lidRviewer"
  use_rgl <- !use_pcv
  use_vox <- !isFALSE(voxel)
  has_pcv <- "lidRviewer" %in% rownames(utils::installed.packages())
  has_col <- color %in% names(x)
  use_rgb <- color == "RGB"
  has_rgb <- all(c("R", "G", "B") %in% names(x))
  autocol <- is.character(pal) && length(pal) == 1 && pal == "auto"

  # Error handling
  assert_is_a_bool(clear_artifacts)
  assert_is_a_bool(axis)
  assert_is_a_bool(legend)
  if (!isFALSE(add))
  {
    assert_is_numeric(add)
    assert_is_of_length(add, 2)
  }
  if (is.empty(x))         stop("Cannot display an empty point cloud", call. = FALSE)
  if (use_pcv & !has_pcv)  stop("'lidRviewer' package is needed. Please read documentation.", call. = FALSE) # nocov
  if (length(color) > 1)   stop("'color' should contain a single value.", call. = FALSE)
  if (!use_rgb & !has_col) stop("'color' should refer to an attribute of the LAS data.", call. = FALSE)
  if (use_rgb & !has_rgb)  stop("No 'RGB' attributes found.", call. = FALSE)

  # Retrieve voxels size
  if (use_vox)
  {
    verbose("Plotting in voxel mode")

    if (!is.null(attr(x, 'res')) && !is.numeric(voxel))
    {
      voxel <- attr(x, 'res')
      verbose(glue::glue("Resolution of the voxels found in the attribute 'res': {voxel}"))
    }
    else if (isTRUE(voxel))
    {
      xres <- min(diff(sort(unique(x$X))))
      yres <- min(diff(sort(unique(x$Y))))
      zres <- min(diff(sort(unique(x$Z))))
      voxel  <- min(xres, yres, zres)
      verbose(glue::glue("Resolution automatically detected: {voxel}"))
    }
    else if (is.numeric(voxel))
    {
      verbose(glue::glue("Resolution defined by user: {voxel}"))
    }
    else
    {
      stop("Parameter 'voxel' must be a boolean or a number")
    }
  }

  # Create a colour scheme for each point as a function of the attribute plotted
  nbreaks <- if (nbreaks == "auto") 50 else nbreaks
  idcolor <- NULL

  if (autocol)
  {
    if (color == "Z")
      pal <- height.colors
    else if (color == "Intensity")
      pal <- grDevices::heat.colors
    else if (color  == "Classification")
      pal <- lasclass.colors
    else if (color == "ScanAngleRank" | color == "ScanAngle")
      pal <- height.colors
    else if (color == "ReturnNumber")
      pal <- grDevices::colorRampPalette(rev(c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF")))
    else if (color == "treeID")
      pal <- pastel.colors
    else if (color == "Amplitude")
      pal <- amplitude.color
    else
      pal <- height.colors

    if (color  == "Classification")
      idcolor <- x[["Classification"]] + 1L

    if (color == "treeID")
      nbreaks <- data.table::uniqueN(x[[color]])
  }
  else if (is.character(pal))
  {
    pal <- grDevices::colorRampPalette(pal)
  }

  if (use_rgb) # Case RGB
  {
    idcolor <- "RGB"
  }
  else if (!is.null(idcolor)) # Case classification
  {
    pal <- pal()
    idcolor[idcolor > length(pal)] <- length(pal) # No colordefined after class 19. Everything is yellow
  }
  else
  {
    colorattr <- x[[color]]
    nunique   <- data.table::uniqueN(colorattr)

    if (nunique == 1L)
    {
      idcolor <- 1L
      pal   <- pal(1)
    }
    else
    {
      if (is.logical(colorattr)) colorattr <- as.integer(colorattr)
      if (is.character(breaks)) breaks <- suppressWarnings(classInt::classIntervals(colorattr, min(nbreaks, nunique), breaks)$brk)
      nbreaks   <- length(breaks)
      pal       <- pal(nbreaks-1)
      idcolor   <- cut(colorattr, breaks, include.lowest = TRUE, label = FALSE)
    }

    if (anyNA(idcolor))
    {
      idcolor[is.na(idcolor)] <- nbreaks
      pal <- c(pal, NAcol)
    }
  }

  lasplot <- if (use_rgl) .plot_with_rgl else .plot_with_pcv # nocov

  return(lasplot(x, bg, idcolor, pal, clear_artifacts, axis, legend, args, add, voxel, breaks))
}

.plot_with_rgl = function(x, bg, idcolor, pal, clear_artifacts, axis, legend, args, add, use_voxels, breaks)
{
  # The color of the foreground  (axis actually) is the opposite of the background
  # so usually white, maybe black if bg = "white" or something fancy in other cases
  fg <- grDevices::col2rgb(bg)
  fg <- grDevices::rgb(t(255 - fg)/255)

  # If it is a new layer, compute the offset used to get rid of floating point accuracy with rgl
  # otherwise we use the input offset
  if (isFALSE(add))
  {
    minx <- min(x@data$X)
    miny <- min(x@data$Y)
  }
  else
  {
    minx <- add[1]
    miny <- add[2]
  }

  if (is.character(idcolor) && idcolor == "RGB")
  {
    colmax <- max(max(x@data[["R"]]), max(x@data[["G"]]), max(x@data[["B"]]))
    nbits  <- if (colmax > 255) 16 else 8
    maxcol <- 2^nbits - 1
    col <- grDevices::rgb(x@data[["R"]]/maxcol, x@data[["G"]]/maxcol, x@data[["B"]]/maxcol)
  }
  else if (is.integer(idcolor))
  {
    col <- pal[idcolor]
  }
  else
    stop("Internal error in color attribution. Please report.") # nocov

  # Optimize the rendering of the voxels by removing voxels than can't be seen
  if(!isFALSE(use_voxels) && !is_disable_point_metrics())
  {
    nvoxels1 <- npoints(x)
    res <- as.numeric(use_voxels)
    u <- point_metrics(x, ~length(Z), r = res)
    keep <- u$V1 <= 6
    x <- filter_poi(x, keep)
    col <- col[keep]
    nvoxels2 <- npoints(x)
    verbose(glue::glue("Rendering {nvoxels2} voxels. {nvoxels1-nvoxels2} being hidden."))
  }

  with <- c(list(x = x@data[["X"]], y = x@data[["Y"]], z = x@data[["Z"]], col = col), args)

  if (clear_artifacts)
  {
    with$x <- with$x - minx
    with$y <- with$y - miny
  }
  # If it is a new layer, open an rgl windows
  if (isFALSE(add))
  {
    rgl::open3d()
    rgl::bg3d(color = bg)
    rgl::material3d(specular = "black")
  }

  # Two modes, point-cloud rendering or voxel rendering
  if (isFALSE(use_voxels))
  {
    do.call(rgl::points3d, with)
  }
  else
  {
    res <- as.numeric(use_voxels)
    res <- res / 2
    n <- npoints(x)
    u <- vector("list", n)
    for(i in 1:n)
    {
      voxel <- rgl::cube3d(col = col[i])
      voxel <- rgl::scale3d(voxel, res, res, res)
      voxel <- rgl::translate3d(voxel , with$x[i], with$y[i], with$z[i])
      u[[i]] <- voxel
    }

    rgl::shapelist3d(u)
  }

  if (axis)
  {
    rgl::axis3d("x", col = fg)
    rgl::axis3d("y", col = fg)
    rgl::axis3d("z", col = fg)
  }

  if (legend)
  {
    # nocov because this fails on some flavors on CRAN
    f <- .plot_scale_gradient(breaks, fg, pal, bg) # nocov
    rgl::bg3d(texture = f, col = "white") # nocov
  }

  .pan3d(2)

  if (clear_artifacts)
    return(invisible(c(minx, miny)))
  else
    return(invisible(c(0,0)))
}

# nocov start
.plot_with_pcv = function(x, bg, idcolor, pal, clear_artifacts, axis, legend, args, add, use_voxel, breaks)
{
  if (!isFALSE(add)) stop("Argument 'add' is not supported with lidRviewer")
  if (!isFALSE(use_voxel)) stop("Argument 'use_voxel' is not supported with lidRviewer")

  if (is.character(idcolor))
  {
    if (idcolor == "RGB")
      eval(parse(text = "lidRviewer::plot_xyzrgb(x@data$X, x@data$Y, x@data$Z, x@data$R, x@data$G, x@data$B, args$size)"))
    else
      stop("Unexpected error.", call. = FALSE)
  }
  else
  {
    eval(parse(text = "lidRviewer::plot_xyzcol(x@data$X, x@data$Y, x@data$Z, pal, idcolor, args$size)"))
  }

  return(invisible(c(0,0)))
}
# nocov end

# nocov start
.plot_scale_gradient = function(breaks, text.col, scale.col, bg)
{
  f <- tempfile(fileext = ".png")
  labels <- round(breaks, 2)
  ncol   <- length(scale.col)
  nlab   <- length(labels)
  xl <- 1 ; yb <- 1 ; xr <- 1.1 ; yt <- 2
  grDevices::png(f, 1920, 1080, bg = bg)
  graphics::layout(matrix(1:2, nrow = 1), widths = c(0.9,0.1))
  graphics::par(mar = c(5.1, 4.1, 4.1, 2.1))
  graphics::plot(0, ann = FALSE, type = "n", axes = FALSE)
  graphics::par(mar = c(5.1, 0.5, 4.1, 0))
  graphics::plot(NA, type = "n", ann = FALSE, xlim = c(1,2), ylim = c(1,2), xaxt = "n", yaxt = "n", bty = "n")
  graphics::rect(xl, utils::head(seq(yb, yt, (yt - yb)/ncol), -1), xr, utils::tail(seq(yb, yt, (yt - yb)/ncol), -1), col = scale.col, border = NA)
  graphics::mtext(labels, side = 2, at = seq(yb, yt, length.out = nlab), las = 2, cex = 1.2, col = text.col)
  grDevices::dev.off()
  return(f)
}
# nocov end

# From rgl.setMouseCallbacks man page
# nocov start
.pan3d <- function(button, dev = rgl::cur3d(), subscene = rgl::currentSubscene3d(dev))
{
  start <- list()

  begin <- function(x, y)
  {
    activeSubscene <- rgl::par3d("activeSubscene", dev = dev)
    start$listeners <<- rgl::par3d("listeners", dev = dev, subscene = activeSubscene)

    for (sub in start$listeners)
    {
      init <- rgl::par3d(c("userProjection","viewport"), dev = dev, subscene = sub)
      init$pos <- c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5)
      start[[as.character(sub)]] <<- init
    }
  }

  update <- function(x, y)
  {
    for (sub in start$listeners)
    {
      init <- start[[as.character(sub)]]
      xlat <- 2*(c(x/init$viewport[3], 1 - y/init$viewport[4], 0.5) - init$pos)
      mouseMatrix <- rgl::translationMatrix(xlat[1], xlat[2], xlat[3])
      rgl::par3d(userProjection = mouseMatrix %*% init$userProjection, dev = dev, subscene = sub )
    }
  }
  rgl::rgl.setMouseCallbacks(button, begin, update, dev = dev, subscene = subscene)
}
# nocov end
