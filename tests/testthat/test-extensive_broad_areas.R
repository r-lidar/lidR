if (Sys.getenv("LIDR_EXTENSIVE_TESTS") == "TRUE")
{
  test_that("rasterize_canopy works with haliburton", {

    bbox <- sf::st_bbox(c(xmin = 685000, xmax = 695000, ymin = 5010000, ymax = 5015000))

    haliburton <- "/media/jr/Seagate Expansion Drive/Ontario/Haliburton/Landscape LiDAR"
    haliburton <- readLAScatalog(haliburton)
    haliburton <- catalog_intersect(haliburton, bbox, subset = "flag_processed")

    plot(haliburton)


    res <- las_check(haliburton) |> lapply(length) |> unlist() |> unname()
    expect_equal(res, c(0,0,0))

    # Small chunk to trigger potential errors with empty chunks
    # Approx 7 minutes
    opt_chunk_size(haliburton) <- 250
    opt_output_files(haliburton) <- "{tempdir()}/haliburton-canopy-{ID}"
    chm <- rasterize_canopy(haliburton, 1, p2r(0.15))
    expect_is(chm, "stars_proxy")

    plot(haliburton)
    plot(chm, col = height.colors(25), breaks = "equal", add = T)

    # Approx 40 seconds
    opt_chunk_size(haliburton) <- 0
    opt_output_files(haliburton) <- ""
    chm = rasterize_canopy(haliburton, 2, p2r())
    expect_is(chm, "stars")

    plot(chm, col = height.colors(25), breaks = "equal")

    # Approx 40 seconds
    options(lidR.raster.default = "raster")
    opt_chunk_size(haliburton) <- 0
    opt_output_files(haliburton) <- ""
    chm = rasterize_canopy(haliburton, 2, p2r())
    expect_is(chm, "RasterLayer")
    expect_false(raster::inMemory(chm))

    plot(chm, col = height.colors(25))

    # Approx 40 seconds
    options(lidR.raster.default = "terra")
    opt_chunk_size(haliburton) <- 0
    opt_output_files(haliburton) <- ""
    chm = rasterize_canopy(haliburton, 2, p2r())
    expect_is(chm, "SpatRaster")

    plot(chm, col = height.colors(25))
  })

  test_that("rasterize_terrain works with montmorency", {

    bbox <- sf::st_bbox(c(xmin = 325000, xmax = 333000, ymin = 521000, ymax = 5229000))

    montmorency <- "/media/jr/Seagate Expansion Drive/Quebec/Montmorency/las/"
    montmorency <- readLAScatalog(montmorency)
    montmorency <- catalog_intersect(montmorency, bbox, subset = "flag_processed")

    plot(montmorency)

    res <- las_check(montmorency) |> lapply(length) |> unlist() |> unname()
    expect_equal(res, c(0,2,0))

    # Small chunk to trigger potential errors with empty chunks
    # Approx 10 minutes
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- "{tempdir()}/montmorency-terrain-{ID}"
    dtm <- rasterize_terrain(montmorency, 2, tin())
    expect_is(dtm, "stars_proxy")

    plot(dtm, breaks = "equal")

    # Approx ?? minutes
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- ""
    dtm = rasterize_canopy(montmorency, 2, p2r())
    expect_is(dtm, "stars")

    plot(dtm, col = height.colors(25), breaks = "equal")

    # Approx ?? minutes
    options(lidR.raster.default = "raster")
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- ""
    dtm = rasterize_canopy(montmorency, 2, p2r())
    expect_is(dtm, "RasterLayer")
    expect_false(raster::inMemory(dtm))

    plot(dtm, col = height.colors(25))

    # Approx ?? minutes
    options(lidR.raster.default = "terra")
    opt_chunk_size(montmorency) <- 0
    opt_output_files(montmorency) <- ""
    dtm = rasterize_canopy(montmorency, 2, p2r())
    expect_is(dtm, "SpatRaster")

    plot(dtm, col = height.colors(25))
  })
}
