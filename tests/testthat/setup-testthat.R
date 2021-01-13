options(lidR.progress = FALSE)
rgdal::set_thin_PROJ6_warnings(TRUE)

#Sys.setenv(NOT_CRAN = "false")

megaplot_laz_path <- system.file("extdata", "Megaplot.laz", package="lidR")
mixedconifer_laz_path <- system.file("extdata", "MixedConifer.laz", package="lidR")
topography_laz_path <- system.file("extdata", "Topography.laz", package="lidR")
example_laz_path <- system.file("extdata", "example.laz", package="rlas")

megaplot_las_path <- tempfile(fileext = ".las")
mixedconifer_las_path <- tempfile(fileext = ".las")
topography_las_path <- tempfile(fileext = ".las")
example_las_path <- tempfile(fileext = ".las")

megaplot <- readLAS(megaplot_laz_path) # 110 ms
mixedconifer <- readLAS(mixedconifer_laz_path)
topography <- readLAS(topography_laz_path) # 110 ms
example <- readLAS(example_laz_path) # 30 ms
random_10_points <- lidR:::generate_las(10)
random_500_points <- lidR:::generate_las(500)

writeLAS(megaplot, megaplot_las_path)
writeLAS(mixedconifer, mixedconifer_las_path)
writeLAS(topography, topography_las_path)
writeLAS(example, example_las_path)

megaplot_ctg <- readLAScatalog(megaplot_las_path) # 50 ms + 30 ms at read time
mixedconifer_ctg <- readLAScatalog(mixedconifer_las_path)
topography_ctg <- readLAScatalog(topography_las_path)
example_ctg <- readLAScatalog(example_las_path)
random_2files_250points <- lidR:::catalog_generator(2, 250) # 180 ms
