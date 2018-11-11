library(lidR)
library(profvis)

profvis(catalog("~/Documents/ALS data/Haliburton dataset/Landscape LiDAR/"), interval = 0.005)

prj = catalog("~/Documents/ALS data/Haliburton dataset/Landscape LiDAR/")

x = c(680000, 690000, 695000) + runif(3, 0, 1000)
y = c(5015000, 5015100, 5015100) + runif(3, 0, 1000)

cores(prj) = 3L

profvis(catalog_queries(prj, x, y, 20), interval = 0.005)
