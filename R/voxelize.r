#' convert lidar point cloud into voxels
#'
#' @param pointcloud lidar pointcloud, object of class "Lidar".
#' @param voxel_size size of the voxels
#' @return an object of class "data.table" with coordinates of the voxels.
#' @author Piotr Tompalski
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar <-  LoadLidar(LASfile)
#' lidar_vox <- voxelize(lidar)
#' @export


voxelize <- function(pointcloud,voxel_size = 1) {
  if (class(lidar) != "Lidar") stop("Input data not of class Lidar")
  
  #keep only XYZ
  pointcloud <- pointcloud@data[, 1:3, with = FALSE]
  
  # this check below can be probably removed
  if (("X" %in% colnames(pointcloud) & 
      "Y" %in% colnames(pointcloud) & 
      "Z" %in% colnames(pointcloud))==F) {
    stop("Input data does not contain columns X, Y, and Z") 
  }

    # round to nearest vox size
    p.round <- as.data.table(lidR:::mround(pointcloud,voxel_size))
    
    #remove duplicates
    voxels <- unique(p.round)
    
    #return voxels
    return(voxels)
}


mround <- function(x,base=10){
  base*round(x/base)
}