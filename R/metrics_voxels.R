#' @param all_voxels boolean. By default the function returns only voxels that
#' contain 1 or more points. Empty voxels do not exist as the metrics are undefined.
#' If \code{all_voxels = TRUE} all the voxels are returned and metrics are NA for
#' voxels with 0 points.
#' @rdname aggregate
#' @export
voxel_metrics = function(las, func, res = 1, ..., all_voxels = FALSE)
{
  stopifnotlas(las)
  assert_all_are_non_negative(res)
  assert_is_a_bool(all_voxels)

  if (length(res) == 1L)
    res <- c(res,res)
  else if (length(res) > 2L)
    stop("Wrong resolution provided.")

  .I <- .GRP <- GRP <- NULL
  by <- group_grid_3d(las$X, las$Y, las$Z, res, c(0,0,0.5*res[2]))
  by <- data.table::setDT(by)
  by <- by[, GRP := .GRP, by = c("Xgrid", "Ygrid", "Zgrid")]
  grp <- by$GRP
  by <- by[by[, .I[1], by = GRP]$V1]

  M <- template_metrics(las, func, grp, ...)

  if ("Z" %in% names(M))
    warning("Z is reserved for the coordinates of the voxels and cannot be used as metric name. Metric Z discared.", call. = FALSE)

  M[["X"]] <- by[["Xgrid"]][M[["GRPID"]]]
  M[["Y"]] <- by[["Ygrid"]][M[["GRPID"]]]
  M[["Z"]] <- by[["Zgrid"]][M[["GRPID"]]]
  M[["GRPID"]] <- NULL

  if (all_voxels)
  {
    xrange <- range(M$X)
    yrange <- range(M$Y)
    zrange <- range(M$Z)
    X <- f_grid(seq(xrange[1], xrange[2], by = res[1]), res[1], 0)
    Y <- f_grid(seq(yrange[1], yrange[2], by = res[1]), res[1], 0)
    Z <- f_grid(seq(zrange[1], zrange[2], by = res[2]), res[2], 0.5*res[2])
    all <- expand.grid(X = X,Y = Y, Z = Z)
    data.table::setDT(all)
    data.table::setkey(M, X,Y,Z)
    data.table::setkey(all, X,Y,Z)
    M <- merge(M, all, all = TRUE)
  }

  data.table::setcolorder(M, c("X", "Y", "Z"))
  data.table::setattr(M, "class", c("lasmetrics3d", attr(M, "class")))
  data.table::setattr(M, "res", res[1])
  return(M)
}


