---
layout: default
title: Compute a series of descriptive statistics
---

The `gridMetrics` function makes rasters (cells) and enables to compute one or more metrics for each cells.
The size of the cells is given by the parameter `resolution`. The cell area is the square of the resolution. The metric wanted is given by an expression in parameter `func`. The `gridMetrics` function returns a `gridMetrics` object.

## Grid one metric

[`canopyModel`](canopy.html) was an alias for:

    canopy = lidar %>% gridMetrics(2, max(Z))

`pulseDensity` was an alias for:

    pulseDensity = lidar %>% gridMetrics(3, length(unique(pulseID))/9)

Compute the mean height for 400 m^2 plots

    hmean = lidar %>% gridMetrics(20, mean(Z))

Some functions are already avaible in the package like `entropy` or `vci`

    entropy = lidar %>% gridMetrics(20, entropy(Z))
    vci     = lidar %>% gridMetrics(20, vci(Z, zmax = 40))


## Grid multiple metrics

When we want to compute several metrics we would like to compute each metric in one time. We can use `gridMetrics` with a function which return several values in a labelled `list`:

### Define your own metric function

    myMetrics = function(z, i, angle, pulseID, area)
    {
        ret = list(
            density = length(unique(pulseID))/area,
            hmean   = mean(z),
            hmax    = max(z),
            imean   = mean(i),
            angle   = mean(abs(angle))
            )
    
      return(ret)
    }
    
The page about [common miss usage of gridMetrics](gridMetrics-error.html) provide further detail on how this function works.

### Use you own function in gridMetrics

    metrics = gridMetrics(lidar, 20, myMetrics(Z, Intensity, ScanAngle, pulseID, 400))

## split_flightline option

`gridMetrics` allows an optional parameter named `option`. You can set this parameter to `"split_flighline"`. In this case, the algorithm will compute the metrics on each flighline individually. In this case in the overlaps, for example, you will have twice the same raster.

    metrics = gridMetrics(lidar, 20, myMetrics(Z, Intensity, ScanAngle, pulseID, 400), option = "split_flightline")

## cloudMetrics

The function `cloudMetrics` works exactly like `gridMetrics` but it does no have a `resolution` parameter and it does not make cells. It only computes one or several metrics on a cloud of points based on the function given in parameter. It is useful to compute metrics on a single plot inventory.

    metrics = cloudMetrics(lidar, myMetrics(Z, Intensity, ScanAngle, pulseID, 400), option = "split_flightline")

## Quality control for gridMetrics and geographic data

The previous sections showed how to efficiently process a dataset. It returns a list of plots with the associated metrics. But some plots fall in water and the algorithm cannot guess that. Some plots are incomplete because they fall in the edge of the file or in the edge of a flightline, or in this edge of a void area (providers removed some data) or other good reason. The algorithm makes cells but it does not control what was in the cells. You can control the cells quality using two ways.

- Using quality metrics
- Filtering data based on shapefiles
