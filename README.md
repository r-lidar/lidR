# lidR
R package for airborne LiDAR data manipulation and visualisation for forestry application

lidR is package provides functions to read `.las` files, plot cloud of points, compute metrics in an area-based approach, compute digital model of canopy, thin lidar data and enable to process LiDAR data with our own function. LidR is designed mainloy for research purposes.

# Install lidR on R with devtools

    install.package("devtools")
    devtools::instal.github("Jean-Romain/lidR")
    
# Some examples

## Read a las file

    lidar = LoadLidar("file.las")
    
## Plot data

    plot(lidar)

## Compute a simple metric

    metric = gridMetrics(lidar, 20, mean(Z))
    plot(metric)
