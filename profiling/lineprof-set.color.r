library(lidR)

x = runif(1e8, 0, 25)

profvis::profvis(lidR:::set.colors(x, palette = height.colors(50)))