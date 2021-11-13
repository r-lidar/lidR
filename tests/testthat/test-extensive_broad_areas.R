if (Sys.getenv("LIDR_EXTENSIVE_TESTS") == "TRUE")
{
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
    res <- las_check(haliburton) |> lapply(length) |> unlist() |> unname()
    expect_equal(res, c(0,0,0))

    res <- las_check(montmorency) |> lapply(length) |> unlist() |> unname()
    expect_equal(res, c(0,2,0))
  })

  test_that("rasterize_canopy works with haliburton with small chunks",
  {
    # Small chunk to trigger potential errors with empty chunks
    # Approx 7 minutes
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
    # Small chunk to trigger potential errors with empty chunks
    # Approx 10 minutes
    opt_chunk_size(montmorency) <- 250
    opt_output_files(montmorency) <- "{tempdir()}/montmorency-terrain-{ID}"
    dtm <- rasterize_terrain(montmorency, 2, tin())
    expect_is(dtm, "stars_proxy")
    expect_equal(basename(dtm[[1]]), "rasterize_terrain.vrt")

    plot(dtm, breaks = "equal", nbreaks = 50)

    ntile33_1 <- normalize_height(montmorency_tile33, dtm) # nawak
    expect_true(min(ntile33_1$Z) > -1)
    plot(ntile33_1)

    ntile33_2 <- normalize_height(montmorency_tile33, tin(), dtm = dtm) # nawak
    expect_true(min(ntile33_2$Z) > -1)
    plot(ntile33_2)
  })

  test_that("rasterize_terrain works with montmorency with by files", {

    # Small chunk to trigger potential errors with empty chunks
    # Approx 10 minutes
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- "{tempdir()}/montmorency-terrain-{ID}"
    dtm <- rasterize_terrain(montmorency, 2, tin())
    expect_is(dtm, "stars_proxy")

    plot(dtm, breaks = "equal", nbreaks = 50)

    ntile33_1 <- normalize_height(montmorency_tile33, dtm)
    expect_true(min(ntile33_1$Z) > -1)
    plot(ntile33_1)

    ntile33_2 <- normalize_height(montmorency_tile33, tin(), dtm = dtm)
    expect_true(min(ntile33_2$Z) > -1)
    plot(ntile33_2)
  })

 test_that("rasterize_terrain works with montmorency with in memory and stars",
 {
    # Approx ?? minutes
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- ""
    dtm = rasterize_canopy(montmorency, 2, p2r())
    expect_is(dtm, "stars")
    expect_false(is(dtm, "stars_proxy"))

    plot(dtm, breaks = "equal")
 })

 test_that("rasterize_terrain works with montmorency with in memory and raster",
 {
    # Approx ?? minutes
    options(lidR.raster.default = "raster")
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- ""
    dtm = rasterize_canopy(montmorency, 2, p2r())
    expect_is(dtm, "RasterLayer")
    expect_false(raster::inMemory(dtm))

    plot(dtm, col = gray.colors(30,0,1))

    ntile33_1 <- normalize_height(montmorency_tile33, dtm) # nawak
    expect_true(min(ntile33_1$Z) > -1)
    plot(ntile33_1)

    ntile33_2 <- normalize_height(montmorency_tile33, tin(), dtm = dtm) # nawak
    expect_true(min(ntile33_2$Z) > -1)
    plot(ntile33_2)
  })

 test_that("rasterize_terrain works with montmorency with in memory and terra",
 {
    # Approx ?? minutes
    options(lidR.raster.default = "terra")
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- ""
    dtm = rasterize_canopy(montmorency, 2, p2r()) # terra::mosaic run out of memory
    expect_is(dtm, "SpatRaster")
    expect_false(raster::inMemory(dtm))

    plot(dtm, col = height.colors(25))

    ntile33_1 <- normalize_height(montmorency_tile33, dtm) # nawak
    expect_true(min(ntile33_1$Z) > -1)
    plot(ntile33_1)

    ntile33_2 <- normalize_height(montmorency_tile33, tin(), dtm = dtm) # nawak
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

  test_that("lidRbook example",
  {
    ctg <- lidRbook

    opt_output_files(ctg) <- paste0(tempdir(), "/{*}_dtm")
    dtm <- rasterize_terrain(ctg, 1, tin())
    plot(dtm)

    opt_output_files(ctg) <- paste0(tempdir(), "/{*}_norm")
    ctg_norm <- normalize_height(ctg, dtm)
    plot(ctg_norm)

    opt_output_files(ctg_norm) <- ""
    ttops <- locate_trees(ctg_norm, lmf(4), uniqueness = "bitmerge")
    plot(ttops["treeID"], cex = 0.01, pch = 19)

    opt_output_files(ctg_norm) <- paste0(tempdir(), "/chm_{*}")
    chm <- rasterize_canopy(ctg_norm, 1, p2r(0.15))
    plot(chm, col = height.colors(50), breaks = "equal")

    opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_segmented")
    algo <- dalponte2016(chm, ttops)
    ctg_segmented <- segment_trees(ctg_norm, algo)

    opt_output_files(ctg_segmented) <- ""
    lasplot <- clip_circle(ctg_segmented, 338500, 5238600, 40)
    plot(lasplot, color = "treeID", bg = "white", size = 4)
    pol = crown_metrics(lasplot, NULL, geom = "convex")
    plot(pol, col = pastel.colors(50))
  })
}
