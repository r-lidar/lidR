# nocov start

# ==== Callbacks ======
# These functions are the pan zoom and rotate callback functions. When user is
# zooming/panning/rotating these functions are called.
# When called they calculates which chunk of points must be added and removed such as
# rgl never displays more that the maximum authorized amount of points. When zooming in
# the algorithm select fewer but denser chunks such as the user see more points but the
# rendering actually contains no more than the authorized limit


pan3d <- function(button, dev = rgl::rgl.cur(), subscene = rgl::currentSubscene3d(dev))
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
      cat("pan\n")
    }
  }

  rgl::rgl.setMouseCallbacks(button, begin, update, dev = dev, subscene = subscene)
}

spatially_indexed_pan_and_zoom = function(las, clear_artifacts = T, nmaxpoints = 3e6, dev = rgl::cur3d(), subscene = rgl::currentSubscene3d(dev))
{
  offsets = c(0,0)
  if (clear_artifacts)
    offsets <- c(min(las$X), min(las$Y))

  # Build a spatial index
  spatial_index <- build_index(las, offsets, 200, nmaxpoints)
  update_rendering(spatial_index, offsets)
  gc()

  # That's a counter to skip some updates and gain reactivity
  i <- 0
  rotate <- function(x)
  {
    zoom(x)

    # It is more reactive if we do not update every time
    if (i %% 2 == 0)
    {
      ti = Sys.time()
      update_rendering(spatial_index, offsets)
      tf = Sys.time()
      cat("Zoom rendering updated in ", round(tf-ti,2), "s\n", sep = "")
    }

    i <<- i+1
  }

  start <- list()

  # From rgl.setMouseCallbacks man page
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

      # It is more reactive if we do not update every time
      if (i %% 2 == 0)
      {
        ti = Sys.time()
        update_rendering(spatial_index, offsets)
        tf = Sys.time()
        cat("Pan rendering updated in ", round(tf-ti,2), "s\n", sep = "")
      }
    }
  }

  rgl::rgl.setMouseCallbacks(2L, begin, update, dev = dev, subscene = subscene)
  rgl::rgl.setWheelCallback(rotate, dev = dev, subscene = subscene)
}

# ==== Spatial index ====

build_index = function(las, offsets = c(0,0), cellsize = 200, nmaxpoints = 2e6)
{
  verbose("Building the spatial index")

  .N <- NULL

  # Build a partition in a spatial format that give the bbox of each chunk
  partition <- sf::st_as_sf(sf::st_make_grid(las, cellsize = cellsize))
  xcut <- sort(unique(sf::st_coordinates(partition)[,1]))
  ycut <- sort(unique(sf::st_coordinates(partition)[,2]))
  partition$id <- 1:nrow(partition)
  sf::st_agr(partition) <- "constant"
  hull <- st_convex_hull(las[sample.int(npoints(las), 1e5)])
  partition <- sf::st_intersection(partition, hull)

  offset = sf::st_sfc(sf::st_point(offsets))
  sf::st_geometry(partition) <- sf::st_geometry(partition) - offsets

  npartitions <- nrow(partition)
  data.table::setattr(partition, "maxpoints", nmaxpoints)
  #plot(partition)

  # Pre-compute some numbers of interest
  n <- npoints(las)
  nlevels <- 5L
  perc_at_level1 <- round(nmaxpoints/n,2)
  perc_per_level <- seq(perc_at_level1, 1, length.out = nlevels)
  nsamples <- ceiling(perc_per_level * n / npartitions)
  probs <- nsamples/sum(nsamples)

  # Create a table of existing chunks
  chunks <- expand.grid(id = partition$id, lvl = 1:nlevels)
  chunk  <- chunks$lvl*1000L + chunks$id
  chunks <- data.frame(chunk)
  data.table::setDT(chunks)
  chunks[, rglid := -1L]
  chunks[, visible := FALSE]
  data.table::setkey(chunks, chunk)

  # Compute, for each point, the chunk it belongs to
  # Assign an id (integer) to each point such as we can decompose the id into partition id + level of resolution
  # We want a single number to save memory and computation time.
  xsub <- findInterval(las$X, xcut)
  ysub <- findInterval(las$Y, ycut)
  indx <- (xsub)+(ysub-1L)*(length(xcut)-1L)
  lvl <- sample.int(nlevels, n, replace = TRUE, prob = probs)  # Assign a level of resolution to each point
  id <- lvl*1000L+indx

  # Assign the chunk id to the point cloud.  No copy of the point cloud here as
  # coordinates3D does not perform any copy.
  data <- coordinates3D(las)
  data[, .COLOR := las[[".COLOR"]]]
  data[, chunk :=  id]
  data.table::setindex(data, chunk)

  # Compute the number of points per chunk and level such as, for a given bbox,
  # we can count the number of visible point from the partition and choose a level
  # of resolution
  N <- tabulate(data$chunk)
  k <- N > 0
  points_per_chunk <- data.table::data.table(chunk = which(k), N = N[k])
  chunks <- chunks[points_per_chunk, on = c("chunk")]

  ans = list(partition = partition, chunks = chunks, points = data)
  return(ans)
}

# ==== Internals =====

get_lvl = function(chunkid)
{
  trunc(chunkid/1000)
}

get_id = function(chunkid)
{
  chunkid %% 1000
}

update_rendering = function(spatial_index, offsets)
{
  chunks <- spatial_index$chunks
  partition <- spatial_index$partition

  visibility <- are_bbox_visible(partition)
  visibility <- partition$id[visibility]
  visibility <- get_id(chunks$chunk) %in% visibility
  chunks[, visible := FALSE]
  chunks[visibility, visible := TRUE]
  depth <- find_depth_max(spatial_index)
  chunks[get_lvl(chunk) > depth, visible := FALSE]

  verbose("Point-cloud resolution: ", depth, "\n", sep = "")

  is_visible = chunks$visible
  is_displayed = chunks$rglid != -1L
  node_to_remove = (!is_visible) & is_displayed
  node_to_add = is_visible & !is_displayed

  #geom = partition
  #geom = sf::st_as_sf(geom)
  #geom$visible = factor(are_bbox_visible(partition), levels = c(T,F))
  #plot(geom)

  save <- rgl::par3d(skipRedraw = TRUE, ignoreExtent = T)
  remove_points(chunks, node_to_remove)
  add_points(spatial_index, node_to_add, offsets)
  rgl::par3d(save)

  u <- rgl::rgl.ids()$id[-1] |> sort()
  v <- Filter(\(x) x != -1, chunks$rglid) |> sort()
  if (any(u != v)) stop("Internal error: rgl ids mismatch beetween rgl and the spatial index databases", call. = FALSE)

  return(invisible(NULL))
}

add_points = function(spatial_index, node_to_add, offsets)
{
  chunks <- spatial_index$chunks
  partition <- spatial_index$partition
  cloud <- spatial_index$points

  for (i in 1:nrow(chunks))
  {
    if (node_to_add[i])
    {
      idchunk = chunks$chunk[i]
      sub = cloud[chunk == idchunk]
      if (nrow(sub) > 0)
      {
        chunks[chunk == idchunk, rglid := rgl::points3d(sub$X - offsets[1], sub$Y - offsets[2], sub$Z, col = sub$`.COLOR`, size = 1.5)]
      }
    }
  }

  #cat("Adding", chunks$rglid[node_to_add], "\n")
  return(invisible(NULL))
}

remove_points = function(chunks, node_to_remove)
{
  rglid = chunks[node_to_remove]$rglid
  if (length(rglid) > 0)
  {
    #cat("Removing", rglid, "\n")
    rgl::pop3d(id = rglid)
  }
  chunks[node_to_remove, rglid := -1L]
}

find_depth_max = function(spatial_index)
{
  .SD <- NULL
  nmaxpoints <- attr(spatial_index$partition, "maxpoints")
  visible_points <- spatial_index$chunks[, .(N = sum(N*visible)), keyby = get_lvl(chunk)]
  visible_points <- cumsum(visible_points$N)
  max(which(visible_points <= nmaxpoints))
}

are_bbox_visible <- function(geom, M = rgl::par3d("modelMatrix"), P = rgl::par3d("projMatrix"))
{
  geom <- sf::st_geometry(geom)
  M <- rgl::par3d("modelMatrix")
  P <- rgl::par3d("projMatrix")
  res <- sapply(geom, function(x)
  {
    X <- x[[1]]
    bbx <- range(X[,1])
    bby <- range(X[,2])
    bbx <- c(bbx[1], bbx[2], bbx[2], bbx[1])
    bby <- c(bby[1], bby[1], bby[2], bby[2])
    sq  <- cbind(bbx, bby)
    is_square_visible(sq, M, P)
  })

  return(res)
}

is_square_visible = function(sq,  M = rgl::par3d("modelMatrix"), P = rgl::par3d("projMatrix"))
{
  p = projection3d(sq, M, P)
  w = structure(c(-1, 1, 1, -1, -1, -1, -1, 1, 1, -1), dim = c(5L, 2L))

  #p = sf::st_polygon(list(xy[,1:2]))
  #w = sf::st_as_sfc(sf::st_bbox(c(xmin = -1, xmax = 1,ymin = -1,ymax =1)))
  #plot(p)
  #plot(w, add = T)

  u = sp::point.in.polygon(p[,1], p[,2], w[,1], w[,2])
  v = sp::point.in.polygon(w[,1], w[,2], p[,1], p[,2])
  return(any(u > 0) || any(v > 0))
}

projection3d = function(m, M = rgl::par3d("modelMatrix"), P = rgl::par3d("projMatrix"))
{
  R = m
  for (i in 1:nrow(m))
  {
    v = c(m[i,],0,1)
    u = M %*% v
    w = P %*% u
    w = w/w[4,]
    R[i, 1:2] = w[1:2,]
  }

  return(R[,1:2])
}

zoom = function(x, f = 1.1)
{
  zoom = rgl::par3d("zoom")

  if (x == 1)
    rgl::par3d(zoom = zoom*f)
  else
    rgl::par3d(zoom = zoom/f)
}

# nocov end
