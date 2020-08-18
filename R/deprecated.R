#' Deprecated functions in lidR
#'
#' These functions are provided for compatibility with older versions of lidR but are deprecated
#' since lidR version 3. They will progressively print a message, throw a warning and eventually be
#' removed. The links below point to the documentation of the new names \cr\cr
#' \link[=add_attribute]{lasadd} \link[=las_check]{lascheck} \link[=clip]{lasclip}
#' \link[=segment_shapes]{lasdetectshape} \link[=filter_poi]{lasfilter}
#' \link[=filter_surfacepoints]{lasfiltersurfacepoints} \link[=retrieve_flightlines]{lasflightline}
#' \link[=classify_ground]{lasground} \link[=merge_spatial]{lasmergespatial}
#' \link[=normalize_height]{lasnormalize} \link[=retrieve_pulses]{laspulse}
#' \link[=normalize_intensity]{lasrangecorrection} \link[=retrieve_flightlines]{lasflightline}
#' \link[=las_reoffset]{lasreoffset} \link[=las_rescale]{lasrescale}
#' \link[=retrieve_scanlines]{lasscanlines} \link[=smooth_height]{lassmooth}
#' \link[=segment_snags]{lassnags}
#' \link[=segment_trees]{lastrees} \link[=voxelize_points]{lasvoxelize}
#' \link[=track_sensor]{sensor_tracking} \link[=find_trees]{tree_detection}
#' \link[=delineate_crowns]{tree_hull}
#'
#' @param las See the new functions that replace the old ones
#' @param geometry See the new functions that replace the old ones
#' @param ... See the new functions that replace the old ones
#' @param xleft,ybottom,xright,ytop See the new functions that replace the old ones
#' @param xpoly,ypoly See the new functions that replace the old ones
#' @param xcenter,ycenter,radius See the new functions that replace the old ones
#' @param algorithm See the new functions that replace the old ones
#' @param attribute See the new functions that replace the old ones
#' @param filter See the new functions that replace the old ones
#' @param source See the new functions that replace the old ones
#' @param last_returns See the new functions that replace the old ones
#' @param n,res,dt See the new functions that replace the old ones
#' @param x,name,desc,type,offset,scale,NA_value See the new functions that replace the old ones
#' @param na.rm,use_class,add_lasattribute See the new functions that replace the old ones
#' @param sensor,Rs,f,gpstime,elevation See the new functions that replace the old ones
#' @param xscale,yscale,zscale,xoffset,yoffset,zoffset See the new functions that replace the old ones
#' @param CRSobj See the new functions that replace the old ones
#' @param size,method,shape,sigma See the new functions that replace the old ones
#' @param uniqueness See the new functions that replace the old ones
#' @param interval,pmin,extra_check,thin_pulse_with_time See the new functions that replace the old ones
#' @param concavity,length_threshold,func See the new functions that replace the old ones
#'
#' @rdname deprecated
#' @name deprecated
NULL

# nocov start

#' @export
#' @rdname deprecated
lascheck <- function(las) {
  .lidr3depreciation("las_check")
  return(las_check(las))
}

#' @export
#' @rdname deprecated
lasclip <- function(las, geometry, ...) {
  .lidr3depreciation("clip_roi")
  return(clip_roi(las, geometry, ...))
}

#' @export
#' @rdname deprecated
lasclipRectangle = function(las, xleft, ybottom, xright, ytop, ...) {
  .lidr3depreciation("clip_rectangle")
  return(clip_rectangle(las, xleft, ybottom, xright, ytop, ...))
}

#' @export
#' @rdname deprecated
lasclipPolygon = function(las, xpoly, ypoly, ...) {
  .lidr3depreciation("clip_polygon")
  return(clip_polygon(las, xpoly, ypoly, ...))
}

#' @export
#' @rdname deprecated
lasclipCircle = function(las, xcenter, ycenter, radius, ...) {
  .lidr3depreciation("clip_circle")
  return(clip_circle(las, xcenter, ycenter, radius, ...))
}

#' @export
#' @rdname deprecated
lasdetectshape = function(las, algorithm, attribute = "Shape", filter = NULL) {
  .lidr3depreciation("segment_shapes")
  return(segment_shapes(las, algorithm, attribute, filter))
}

#' @export
#' @rdname deprecated
lasfilter = function(las, ...) {
  .lidr3depreciation("filter_poi")
  return(filter_poi(las, ...))
}

#' @export
#' @rdname deprecated
lasfilterfirst = function(las) {
  .lidr3depreciation("filter_first")
  return(filter_first(las))
}

#' @export
#' @rdname deprecated
lasfilterfirstlast = function(las) {
  .lidr3depreciation("filter_firstlast")
  return(filter_firstlast(las))
}

#' @export
#' @rdname deprecated
lasfilterfirstofmany = function(las) {
  .lidr3depreciation("filter_firstofmany")
  return(filter_firstofmany(las))
}

#' @export
#' @rdname deprecated
lasfilterground = function(las) {
  .lidr3depreciation("filter_ground")
  return(filter_ground(las))
}

#' @export
#' @rdname deprecated
lasfilterlast = function(las) {
  .lidr3depreciation("filter_last")
  return(filter_last(las))
}

#' @export
#' @rdname deprecated
lasfilternth = function(las, n) {
  .lidr3depreciation("filter_nth")
  return(filter_nth(las, n))
}

#' @export
#' @rdname deprecated
lasfiltersingle = function(las) {
  .lidr3depreciation("filter_single")
  return(filter_single(las))
}

#' @export
#' @rdname deprecated
lasfilterdecimate = function(las, algorithm) {
  .lidr3depreciation("decimate_points")
  return(decimate_points(las, algorithm))
}

#' @export
#' @rdname deprecated
lasfilterduplicates = function(las) {
  .lidr3depreciation("filter_duplicates")
  return(filter_duplicates(las))
}

#' @export
#' @rdname deprecated
lasfiltersurfacepoints = function(las, res) {
  .lidr3depreciation("filter_surfacepoints")
  return(filter_surfacepoints(las, res))
}


lasgenerator <- function(n, seeds = 1) {
  .lidr3depreciation("generate_las")
  return(generate_las(n, seeds))
}

#' @export
#' @rdname deprecated
lasground = function(las, algorithm, last_returns = TRUE) {
  .lidr3depreciation("classify_ground")
  return(classify_ground(las, algorithm, last_returns))
}

#' @export
#' @rdname deprecated
laspulse = function(las) {
  .lidr3depreciation("retrieve_pulses")
  return(retrieve_pulses(las))
}

#' @export
#' @rdname deprecated
lasflightline = function(las, dt = 30) {
  .lidr3depreciation("retrieve_flightlines")
  return(retrieve_flightlines(las, dt))
}

#' @export
#' @rdname deprecated
lasscanline = function(las) {
  .lidr3depreciation("retrieve_scanlines")
  return(retrieve_scanlines(las))
}

#' @export
#' @rdname deprecated
lasmergespatial = function(las, source, attribute = NULL) {
  .lidr3depreciation("merge_spatial")
  return(merge_spatial(las, source, attribute))
}

#' @export
#' @rdname deprecated
lasnormalize = function(las, algorithm, na.rm = FALSE, use_class = c(2L,9L), ..., add_lasattribute = FALSE) {
  .lidr3depreciation("normalize_height")
  return(normalize_height(las, algorithm, na.rm, use_class, ..., add_lasattribute = add_lasattribute))
}

#' @export
#' @rdname deprecated
lasunnormalize = function(las) {
  .lidr3depreciation("unnormalize_height")
  return(unnormalize_height(las))
}

#' @export
#' @rdname deprecated
lasrangecorrection <- function(las, sensor, Rs, f = 2.3, gpstime = "gpstime", elevation = "Z") {
  .lidr3depreciation("normalize_intensity")
  return(normalize_intensity(las, range_correction(sensor, Rs, f, gpstime, elevation)))
}

#' @export
#' @rdname deprecated
lasrescale = function(las, xscale, yscale, zscale) {
  .lidr3depreciation("las_rescale")
  return(las_rescale(las, xscale, yscale, zscale))
}

#' @export
#' @rdname deprecated
lasreoffset = function(las, xoffset, yoffset, zoffset) {
  .lidr3depreciation("las_reoffset")
  return(las_reoffset(las, xoffset, yoffset, zoffset))
}

#' @export
#' @rdname deprecated
lassmooth = function(las, size, method = c("average", "gaussian"), shape = c("circle", "square"), sigma = size/6) {
  .lidr3depreciation("smooth_height")
  return(smooth_height(las, size, method, shape, sigma))
}

#' @export
#' @rdname deprecated
lasunsmooth = function(las) {
  .lidr3depreciation("unsmooth_height")
  return(unsmooth_height(las))
}

#' @export
#' @rdname deprecated
lassnags = function(las, algorithm, attribute = "snagCls") {
  .lidr3depreciation("segment_snags")
  return(segment_snags(las, algorithm, attribute))
}

#' @export
#' @rdname deprecated
lastransform = function(las, CRSobj) {
  .lidr3depreciation("spTransform")
  return(spTransform(las, CRSobj))
}

#' @export
#' @rdname deprecated
lastrees = function(las, algorithm, attribute = "treeID", uniqueness = 'incremental') {
  .lidr3depreciation("segment_trees")
  return(segment_trees(las, algorithm, attribute, uniqueness))
}


#' @export
#' @rdname deprecated
lasadddata = function(las, x, name) {
  .lidr3depreciation("add_attribute")
  return(add_attribute(las, x, name))
}

#' @export
#' @rdname deprecated
lasaddextrabytes = function(las, x, name, desc) {
  .lidr3depreciation("add_lasattribute")
  return(add_lasattribute(las, x, name, desc))
}

#' @export
#' @rdname deprecated
lasaddextrabytes_manual = function(las, x, name, desc, type, offset = NULL, scale = NULL, NA_value = NULL) {
  .lidr3depreciation("add_lasattribute_manual")
  return(add_lasattribute_manual(las, x, name, desc, type, offset, scale, NA_value))
}

#' @export
#' @rdname deprecated
lasremoveextrabytes = function(las, name) {
  .lidr3depreciation("remove_lasattribute")
  return(remove_lasattribute(las, name))
}

#' @export
#' @rdname deprecated
lasvoxelize = function(las, res) {
  .lidr3depreciation("voxelize_points")
  return(voxelize_points(las, res))
}

#' @export
#' @rdname deprecated
sensor_tracking <- function(las, interval = 0.5, pmin = 50, extra_check = TRUE, thin_pulse_with_time = 0.001) {
  .lidr3depreciation("track_sensor")
  return(track_sensor(las, Roussel2020(interval, pmin), extra_check, thin_pulse_with_time))
}

#' @export
#' @rdname deprecated
tree_detection = function(las, algorithm) {
  .lidr3depreciation("find_trees")
  return(find_trees(las, algorithm))
}

#' @export
#' @rdname deprecated
tree_hulls = function(las, type = c("convex", "concave", "bbox"), concavity = 3, length_threshold = 0, func = NULL, attribute = "treeID") {
  .lidr3depreciation("delineate_crowns")
  return(delineate_crowns(las, type, concavity, length_threshold, func, attribute))
}

.lidr3depreciation <- function(name)
{
  # no depreciation v3.0.0
  #return(invisible())

  # message v 3.1.0
  msg = paste(as.character(sys.call(sys.parent()))[1L], "is deprecated. Use", name, "instead.")
  message(msg)
  return(invisible())

  # warning v3.2.0
  msg = paste(as.character(sys.call(sys.parent()))[1L], "is deprecated. Use", name, "instead.")
  warning(msg, call. = FALSE)
  return(invisible())

  # error v3.3.0
  msg = paste(as.character(sys.call(sys.parent()))[1L], "is defunct. Use", name, "instead.")
  stop(msg, call. = FALSE)
  return(invisible())
}

# nocov end
