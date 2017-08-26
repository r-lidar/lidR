library(lidR)
library(profvis)

las = readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

zmean = grid_metrics(las, mean(Z), 1)

plot(zmean)

l = profvis(as.raster(zmean), interval = 0.005)
l

metrics = grid_metrics(las, .stdmetrics_z, 10)

l = profvis(as.raster(metrics))
l
