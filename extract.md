---
layout: default
title: Filter a cloud of points
---

The function `extract` enable to filter the cloud of points based on a set of condition. This function is based on `dplyr::filter` and works by the same way.

## get only the first returns

    firstReturns = lidar %>% extract(ReturnNumber == 1)
    
## get only non ground returns

    vegetation = lidar %>% extract(Classification == 1)
    
## get only first returns with a scan angle of 0 degree

    first0 = lidar %>% extract(ReturnNumber == 1, ScanAngle == 0)
    
# Alias

Some extract functions are already defined in the package with aliased name.

    lidar %>% getFirst
    lidar %>% getLast
    lidar %>% getFirstLast
    lidar %>% getFirstOfMany
    lidar %>% getSingle
    lidar %>% getGround
