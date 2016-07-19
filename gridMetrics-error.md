---
layout: default
title: Common misusage of gridMetrics
---

The user can misuse the `gridMetrics` function by using a function which is not appropriate. For example, the following are not allowed: 

    gridMetrics(lidar, 20, LAD(Z))
    gridMetrics(lidar, 20, head(Z))
    gridMetrics(lidar, 20, range(Intensity))
    gridMetrics(lidar, 20, quantile(Z))

1. `LAD` returns a `data.frame`.
2. `head`returns a  `vector`
3. `range`returns a `vector`
4. `quantile`returns a `vector`

The expression provided to `gridMetrics` must return a single number or a `list` of single numbers. If it is not the case, the function will crash and the error given by `data.table` is incomprehensible for users.

## Example of quantiles

By defining a new quantile function which calls the original you can change the output type. And you can choose the names of the outputs renaming the list.

    myQuantile = function(x, probs = seq(0, 1, 0.25))
    {
        q = quantile(x, probs)
        q = as.list(q)
        names(q) = paste("q", probs*100, sep="")
        return(q)
    }
    
Then use this function. This the basic way `gridMetrics` works.
    
    metrics = gridMetrics(lidar, 20, myQuantile(Z))
    head(metrics)

            X       Y   q0   q25   q50     q75  q100
    1: 685310 5017810 0.00 2.020 5.715 10.3325 18.59
    2: 685310 5017790 0.32 0.320 0.320  0.3200  0.32
    3: 685330 5017790 0.00 0.000 0.185  3.2500 11.71
    4: 685330 5017810 0.00 0.290 3.710  6.7600 19.19
    5: 685350 5017790 0.00 0.285 5.540  8.9750 14.67
    6: 685350 5017810 0.00 2.880 6.980  9.9200 15.21
    
You can use it with optional arguments.

    metrics = gridMetrics(lidar, 20, myQuantile(Z, seq(0.1,0.9,0.1)))
    hean(metrics)
    
            X       Y  q10   q20   q30   q40   q50   q60   q70    q80    q90
    1: 685310 5017810 0.00 0.516 3.030 4.160 5.715 7.428 9.292 11.480 14.093
    2: 685310 5017790 0.32 0.320 0.320 0.320 0.320 0.320 0.320  0.320  0.320
    3: 685330 5017790 0.00 0.000 0.020 0.082 0.185 0.400 1.127  5.162  7.500
    4: 685330 5017810 0.06 0.230 0.430 2.170 3.710 4.920 6.020  7.650 10.630
    5: 685350 5017790 0.00 0.120 1.590 3.600 5.540 6.560 8.390  9.800 11.420
    6: 685350 5017810 0.11 0.512 4.202 5.830 6.980 7.870 9.210 10.490 11.652

## Common errors

     gridMetrics(lidar, 20, LAD(Z))

> \> Erreur : The expression 'LAD(Z)' returned a data.frame. A single number or a list of single number is expected.

     gridMetrics(lidar, 20, quantile(Z))

> \> Erreur : The expression 'quantile(Z)' returned a vector of lenght  5. A single number or a list of single number is expected.

    myMetrics = function(z, i, angle, pulseID)
    {
       ret = list(
             npulse  = length(unique(pulseID)),
             hmean   = mean(z),
             hmax    = max(z),
             lad     = LAD(Z),
             iq      = quantile(i))
  
       return(ret)
    }
  
    metrics = gridMetrics(lidar, 20, myMetrics(Z, Intensity, ScanAngle, pulseID))

> \> Erreur : The expression 'myMetrics(Z, Intensity, ScanAngle, pulseID)' returned a list in which all elements are not a single numeric or logical value. The metric lad is a data.frame.

    myMetrics = function(z, i, angle, pulseID)
    {
       ret = list(
             npulse  = length(unique(pulseID)),
             hmean   = mean(z),
             hmax    = max(z),
             iq      = quantile(i))
  
       return(ret)
    }
  
    metrics = gridMetrics(lidar, 20, myMetrics(Z, Intensity, ScanAngle, pulseID))
    
> \>  Erreur : The expression 'myMetrics(Z, Intensity, ScanAngle, pulseID)' returned a list in which all elements are not a single value. The metric iq has a length of 5.