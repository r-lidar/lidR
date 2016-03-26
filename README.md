# lidR
R package for airborne LiDAR data manipulation and visualisation for forestry application

lidR package provides functions to read `.las` files, plot a cloud of points, compute metrics in an area-based approach, compute digital model of canopy, thin lidar data, automatically extract ground inventories, process a set of tiles on multicore data with your own function. LidR is designed mainly for research purposes in an area-based approach.

# Install lidR on R with devtools

    install.package("devtools")
    devtools::instal.github("Jean-Romain/lidR")
    
# Functionnalities 

- Read `.las`files
- Retrieved indiviual pulses
- Retrieve individual flightlines
- Compute any set of metrics on a cloud of point
- Rasterize and apply any function to compute a set of metrics in an area based approach
- Classify and filter data from geographic shapefiles
- Filter cloud of point
- Compute point and pulse densities
- Manage a catalog of `.las` tiles
- Extract automatically a set of ground plot inventories (even plot falling between 2 or more tiles)
- Analyse a full set a tiles in parallel computing
    
# Some examples

## Read a las file

    lidar = LoadLidar("file.las")
    
## Plot data

    plot(lidar)

## Compute a simple metric

    metric = gridMetrics(lidar, 20, mean(Z))
    plot(metric)
