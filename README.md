# Devellopment branch for package lidR

The develop branch contains the code of the next release. Next release will be 1.1.0. It is given without guaranty. It could works or couln't not... 

## Feature

- `classifyFromShapefile` is, at least, 3 times faster. 
    - Parts of the function have been rewritten in C++
    - The new column is added by reference
- Add functions `DEM` and `DTM` : note that they are experimental functions ! They cannot be used for normalization purpose
- `readLAs` have been rewritten in C++ using `liblas`. It is 2 times faster and it's safer.
- Add function `writeLAS` using `liblas`

## Install lidR developpment branch from Github with devtools

    install.packages("devtools")
    devtools::install_github("Jean-Romain/lidR", ref="develop")
    library(lidR)