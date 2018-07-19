library(lidR)
library(profvis)

las = readLAS("~/Documents/ALS data/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

profvis(grid_metrics(las, mean(Z)))
profvis(grid_metrics(las, .stdmetrics))
