library(lidR)
library(profvis)

l = profvis(catalog("~/Documents/Haliburton dataset/Landscape LiDAR/"), interval = 0.005)
l

prj = catalog("~/Documents/Haliburton dataset/Landscape LiDAR/")

x = c(680000, 690000, 695000) + runif(3, 0, 1000)
y = c(5015000, 5015100, 5015100) + runif(3, 0, 1000)

catalog_options(multicore = 3)

l = profvis(catalog_queries(prj, x, y, 20), interval = 0.005)
l