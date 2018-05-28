library(lidR)
library(profvis)

l = profvis(readLAS("~/Documents/ALS data/Haliburton dataset/Landscape LiDAR/CN_683_5016.las"))
l

# v1.3.0 las pulse takes the half the time (setorder to be exact).
# - Is is really need to compute laspulse?
# - Must rewrite grid_density and lasdecimate first.