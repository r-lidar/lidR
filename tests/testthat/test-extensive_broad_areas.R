if (Sys.getenv("LIDR_EXTENSIVE_TESTS") == "TRUE")
{
  gcol = gray(seq(0,1, length.out = 30))

  haliburton_bbox <- sf::st_bbox(c(xmin = 685000, xmax = 695000, ymin = 5010000, ymax = 5015000))
  haliburton <- "/media/jr/Seagate Expansion Drive/Ontario/Haliburton/Landscape LiDAR"
  haliburton <- readLAScatalog(haliburton)
  haliburton <- catalog_intersect(haliburton, haliburton_bbox, subset = "flag_processed")
  plot(haliburton)

  montmorency_bbox <- sf::st_bbox(c(xmin = 325000, xmax = 333000, ymin = 521000, ymax = 5229000))
  montmorency <- "/media/jr/Seagate Expansion Drive/Quebec/Montmorency/las/"
  montmorency <- readLAScatalog(montmorency)
  montmorency <- catalog_intersect(montmorency, montmorency_bbox, subset = "flag_processed")
  montmorency_tile33 <- readLAS(montmorency$filename[33])
  plot(montmorency)

  lidRbook <- "/home/jr/Documents/Ulaval/lidR/lidRbook/data/ENGINE/catalog/"
  lidRbook <- readLAScatalog(lidRbook)

  library(future)
  plan(multisession(workers = 2))

  test_that("las_check works",
  {
    res <- las_check(haliburton)
    res <- lapply(res, length)
    res <- unname(unlist(res))
    expect_equal(res, c(0,0,0))

    res <- las_check(montmorency)
    res <- lapply(res, length)
    res <- unname(unlist(res))
    expect_equal(res, c(0,2,0))
  })

  test_that("rasterize_canopy works with haliburton with small chunks with stars",
  {
    # Small chunk to trigger potential errors with empty chunks
    # Approx 7 minutes
    options(lidR.raster.default = "stars")
    opt_chunk_size(haliburton) <- 250
    opt_output_files(haliburton) <- "{tempdir()}/haliburton-canopy-{ID}"
    chm <- rasterize_canopy(haliburton, 1, p2r(0.15))
    expect_is(chm, "stars_proxy")

    plot(haliburton)
    plot(chm, col = height.colors(25), breaks = "equal", add = T)
  })

  test_that("rasterize_canopy works with haliburton by files with stars",
  {
    # Approx 40 seconds
    options(lidR.raster.default = "stars")
    opt_chunk_size(haliburton) <- 0
    opt_output_files(haliburton) <- ""
    chm = rasterize_canopy(haliburton, 2, p2r())
    expect_is(chm, "stars")

    plot(chm, col = height.colors(25), breaks = "equal")
  })

  test_that("rasterize_canopy works with haliburton by files with raster",
  {
    # Approx 40 seconds
    options(lidR.raster.default = "raster")
    opt_chunk_size(haliburton) <- 0
    opt_output_files(haliburton) <- ""
    chm = rasterize_canopy(haliburton, 2, p2r())
    expect_is(chm, "RasterLayer")
    expect_false(raster::inMemory(chm))

    plot(chm, col = height.colors(25))
  })

  test_that("rasterize_canopy works with haliburton by files with terra",
  {
    # Approx 40 seconds
    options(lidR.raster.default = "terra")
    opt_chunk_size(haliburton) <- 0
    opt_output_files(haliburton) <- ""
    chm = rasterize_canopy(haliburton, 2, p2r())
    expect_is(chm, "SpatRaster")

    plot(chm, col = height.colors(25))
  })

  test_that("rasterize_terrain works with montmorency with small chunks",
  {
    options(lidR.raster.default = "stars")

    # Small chunk to trigger potential errors with empty chunks
    # Approx 10 minutes
    # Need to check if Nearest neighbour was used but interpolation is weak for those points is
    # by passed for the buffer
    opt_chunk_size(montmorency) <- 250
    opt_output_files(montmorency) <- "{tempdir()}/montmorency-terrain-{ID}"
    dtm <- rasterize_terrain(montmorency, 2, tin())
    expect_is(dtm, "stars_proxy")
    expect_equal(basename(dtm[[1]]), "rasterize_terrain.vrt")

    plot(dtm, breaks = "equal", nbreaks = 50)

    ntile33_1 <- normalize_height(montmorency_tile33, dtm)
    plot(ntile33_1)

    ntile33_2 <- normalize_height(montmorency_tile33, tin(), dtm = dtm)
    plot(ntile33_2)
  })

  test_that("rasterize_terrain works with montmorency with by files", {

    options(lidR.raster.default = "stars")

    # Small chunk to trigger potential errors with empty chunks
    # Approx 4 minutes
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- "{tempdir()}/montmorency-terrain-{ID}"
    dtm <- rasterize_terrain(montmorency, 2, tin())
    expect_is(dtm, "stars_proxy")

    plot(dtm, breaks = "equal", nbreaks = 50)

    ntile33_1 <- normalize_height(montmorency_tile33, dtm)
    plot(ntile33_1)

    ntile33_2 <- normalize_height(montmorency_tile33, tin(), dtm = dtm)
    plot(ntile33_2)
  })

 test_that("rasterize_canopy works with montmorency with in memory and stars",
 {
   options(lidR.raster.default = "stars")

    # Approx 4 minutes
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- ""
    dtm = rasterize_canopy(montmorency, 2, p2r())
    expect_is(dtm, "stars")
    expect_false(is(dtm, "stars_proxy"))

    plot(dtm, breaks = "equal")
 })

 test_that("rasterize_terrain works with montmorency with in memory and raster",
 {
    # Approx 5 minutes
    options(lidR.raster.default = "raster")
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- ""
    dtm = rasterize_terrain(montmorency, 2)
    expect_is(dtm, "RasterLayer")
    expect_false(raster::inMemory(dtm))

    plot(dtm, col = gcol)
    ntile33_2 <- normalize_height(montmorency_tile33, tin(), dtm = dtm)
    plot(ntile33_2)
  })

 test_that("rasterize_terrain works with montmorency with in memory and terra",
 {
    # Approx ?? minutes
    options(lidR.raster.default = "terra")
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- ""
    dtm = rasterize_terrain(montmorency, 2) # terra::mosaic run out of memory
    expect_is(dtm, "SpatRaster")
    expect_false(raster::inMemory(dtm))

    plot(dtm, col = gcol)

    ntile33_1 <- normalize_height(montmorency_tile33, dtm)
    plot(ntile33_1)

    ntile33_2 <- normalize_height(montmorency_tile33, tin(), dtm = dtm)
    expect_true(min(ntile33_2$Z) > -1)
    plot(ntile33_2)
  })

 test_that("locate_trees works with haliburton with small chunks",
 {
   # Small chunks to trigger potential errors chunks with 0 trees
   # Approx 20 minutes
   opt_chunk_size(haliburton) <- 250
   opt_output_files(haliburton) <- ""
   ttops <- locate_trees(haliburton, lmf(3)) # never end to merge
   expect_is(ttops, "sf")

   plot(haliburton)
   plot(ttops, col = height.colors(25), breaks = "equal", add = T)
  })

  test_that("lidRbook example with stars and dalponte",
  {
    ctg <- lidRbook

    opt_output_files(ctg) <- paste0(tempdir(), "/{*}_dtm_stars")
    dtm <- rasterize_terrain(ctg, 1, tin(), pkg = "stars")
    dtm
    plot(dtm)

    opt_output_files(ctg) <- paste0(tempdir(), "/{*}_norm")
    ctg_norm <- normalize_height(ctg, dtm)
    plot(ctg_norm)

    opt_output_files(ctg_norm) <- ""
    ttops <- locate_trees(ctg_norm, lmf(4), uniqueness = "bitmerge")
    plot(ttops["treeID"], cex = 0.01, pch = 19)

    opt_output_files(ctg_norm) <- paste0(tempdir(), "/chm_stars_{*}")
    chm <- rasterize_canopy(ctg_norm, 1, p2r(0.15), , pkg = "stars")
    chm
    plot(chm, col = height.colors(50), breaks = "equal")

    expect_error(dalponte2016(chm, ttops)(), "stored on disk")

    opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_segmented")
    algo <- dalponte2016(chm, ttops)
    ctg_segmented <- segment_trees(ctg_norm, algo)

    opt_output_files(ctg_segmented) <- ""
    lasplot <- clip_circle(ctg_segmented, 338500, 5238600, 40)
    plot(lasplot, color = "treeID", bg = "white", size = 4)
    pol = crown_metrics(lasplot, NULL, geom = "convex")
    plot(pol, col = pastel.colors(50))
  })

  test_that("lidRbook example with terra and silva",
  {
    ctg <- lidRbook

    opt_output_files(ctg) <- paste0(tempdir(), "/{*}_dtm_terra")
    dtm <- rasterize_terrain(ctg, 1, tin(), pkg = "terra")
    dtm
    plot(dtm)

    opt_output_files(ctg) <- paste0(tempdir(), "/{*}_norm")
    ctg_norm <- normalize_height(ctg, dtm)
    plot(ctg_norm)

    opt_output_files(ctg_norm) <- ""
    ttops <- locate_trees(ctg_norm, lmf(4), uniqueness = "bitmerge")
    plot(ttops["treeID"], cex = 0.01, pch = 19)

    opt_output_files(ctg_norm) <- paste0(tempdir(), "/chm_terra_{*}")
    chm <- rasterize_canopy(ctg_norm, 1, p2r(0.15), pkg = "terra")
    chm
    plot(chm, col = height.colors(50))

    expect_error(silva2016(chm, ttops)(), "stored on disk")

    opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_segmented")
    algo <- silva2016(chm, ttops)
    ctg_segmented <- segment_trees(ctg_norm, algo)

    opt_output_files(ctg_segmented) <- ""
    lasplot <- clip_circle(ctg_segmented, 338500, 5238600, 40)
    plot(lasplot, color = "treeID", bg = "white", size = 4)
    pol = crown_metrics(lasplot, NULL, geom = "convex")
    plot(pol, col = pastel.colors(50))
  })

  test_that("rasterize supports proxy",
  {
    ctg <- lidRbook

    opt_output_files(ctg) <- paste0(tempdir(), "/{*}_metrics")
    m <- pixel_metrics(ctg, ~mean(Z), 13)
    vrt <- terra::sources(m)

    # terra
    opt_output_files(ctg) <- ""
    dtm = rasterize_terrain(ctg, m)
    dtm
    plot(dtm)

    chm = rasterize_canopy(ctg, m)
    chm
    plot(chm)
    plot(chm-dtm)

    # stars
    m <- stars::read_stars(vrt, proxy = T)

    opt_output_files(ctg) <- ""
    dtm = rasterize_terrain(ctg, m)
    dtm
    plot(dtm)

    chm = rasterize_canopy(ctg, m)
    chm
    plot(chm)
    plot(chm-dtm)

    # raster
    m <- raster::raster(terra::sources(vrt))
    opt_output_files(ctg) <- ""
    dtm = rasterize_terrain(ctg, m)
    dtm
    plot(dtm)

    chm = rasterize_canopy(ctg, m)
    chm
    plot(chm-dtm)
  })

  test_that("Spat* object are serialized",
  {
    LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
    ctg <- readLAScatalog(LASfile, select = "xyz", chunk_size = 140, chunk_buffer = 0, progress = FALSE)
    opt_chunk_alignment(ctg) <- c(0,20)

    library(future)
    plan(multisession, workers = 2)
    res <- pixel_metrics(ctg, ~max(Z), 20)

    expect_is(res, "SpatRaster")
  })

}
