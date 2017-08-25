library(lidR)
library(lineprof)

l = lineprof(lidR:::lascheck(las@data, las@header))
shine(l)
