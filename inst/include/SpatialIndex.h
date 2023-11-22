#ifndef INDEX_H
#define INDEX_H

#include <Rcpp.h>
#include "lidR/GridPartition.h"
#include "lidR/QuadTree.h"
#include "lidR/Octree.h"
#include "lidR/Shapes.h"
#include "lidR/Point.h"

namespace lidR
{

/*
 * Spatial index using transparently 4 possible indexation structures.
 * public members are:
 * - Constructors
 * - Lookup (templated to search any arbitrary shape)
 * - knn (both in 2D or 3D)
 */
class SpatialIndex
{
public:
  SpatialIndex(const Rcpp::S4 las);
  SpatialIndex(const Rcpp::S4 las, const std::vector<bool>& filter);
  template<typename T> void lookup(T& shape, std::vector<PointXYZ>& res);
  void knn(const PointXY& p, const unsigned int k, std::vector<PointXYZ>& res);
  void knn(const PointXYZ& p, const unsigned int k, std::vector<PointXYZ>& res);
  void knn(const PointXY& p, const unsigned int k, const double r, std::vector<PointXYZ>& res);
  void knn(const PointXYZ& p, const unsigned int k, const double r, std::vector<PointXYZ>& res);

private:
  int index_selector(const Rcpp::S4 las) const;

private:
  GridPartition grid;
  QuadTree quadtree;
  Octree octree;
  int type;
  enum TYPES {UKN = 0, ALS = 1, TLS = 2, UAV = 3, DAP = 4, MLS = 5};
  enum INDEXES {AUTOINDEX = 0, GRIDPARTITION = 1, VOXELPARTITION = 2, QUADTREE = 3, OCTREE = 4};
};

/*
 * Default constructor using an S4 LAS object. The LAS object contains a tag
 * that enables to choose automatically between spatial index possibilities
 */
inline SpatialIndex::SpatialIndex(const Rcpp::S4 las)
{
  type = index_selector(las);

  switch(type)
  {
  case GRIDPARTITION:
  case VOXELPARTITION: grid = GridPartition(las); break;
  case QUADTREE: quadtree = QuadTree(las); break;
  case OCTREE: Rcpp::stop("Error: octree no longer supported."); break; // # nocov
  default: Rcpp::stop("Internal error: spatial index code inccorect."); break; // # nocov
  }
}

/*
 * Constructor with a filter. To work with a subset of a point cloud (e.g. first
 * return only) lidR never actually subsets a LAS object because this would create an
 * intermediate (partial) copy of the point cloud. Instead lidR allocates a boolean
 * vector that is used to skip some points on-the-fly. Points of 'las' where 'f' is false
 * are simply not inserted in the index.
 */
inline SpatialIndex::SpatialIndex(const Rcpp::S4 las, const std::vector<bool>& f)
{
  type = index_selector(las);

  switch(type)
  {
  case GRIDPARTITION:
  case VOXELPARTITION: grid = GridPartition(las, f); break;
  case QUADTREE: quadtree = QuadTree(las, f); break;
  case OCTREE: octree = Octree(las, f); break;
  default: Rcpp::stop("Internal error: spatial index code inccorect."); break; // # nocov
  }
}

/*
 * Query points within a shape. The function being templated any shape is possible
 * lidR defines some shapes in Shapes.h. Some shapes are 2D (e.g. Circle) other
 * are 3D (e.g. Sphere).
 */
template<typename T> void SpatialIndex::lookup(T& shape, std::vector<PointXYZ>& res)
{
  switch(type)
  {
  case GRIDPARTITION:
  case VOXELPARTITION: grid.lookup(shape, res); break;
  case QUADTREE: quadtree.lookup(shape, res); break;
  case OCTREE: octree.lookup(shape, res); break;
  }

  return;
}


/*
 * Query the knn of a given 2D point. In that case the Z coordinates is not
 * considered for searching the neighbours. It is a search on XY only.
 */
inline void SpatialIndex::knn(const PointXY& p, const unsigned int k, std::vector<PointXYZ>& res)
{
  switch(type)
  {
  case GRIDPARTITION:
  case VOXELPARTITION: grid.knn(p, k, res); break;
  case QUADTREE: quadtree.knn(p, k, res); break;
  case OCTREE: octree.knn(p, k, res); break;
  }

  return;
}

/*
 * Query the knn of a given 2D point with a maximum radius search. If there are
 * less than k neighbours it returns less than k points
 */
inline void SpatialIndex::knn(const PointXYZ& p, const unsigned int k, std::vector<PointXYZ>& res)
{
  switch(type)
  {
  case GRIDPARTITION:
  case VOXELPARTITION:  grid.knn(p, k, res); break;
  case QUADTREE: quadtree.knn(p, k, res); break;
  case OCTREE: octree.knn(p, k, res); break;
  }

  return;
}

/*
 * Query the knn of a given 3D point.
 */
inline void SpatialIndex::knn(const PointXY& p, const unsigned int k, const double radius, std::vector<PointXYZ>& res)
{
  switch(type)
  {
  case GRIDPARTITION:
  case VOXELPARTITION: grid.knn(p, k, radius, res); break;
  case QUADTREE: quadtree.knn(p, k, radius,  res); break;
  case OCTREE: octree.knn(p, k, radius, res); break;
  }

  return;
}

/*
 * Query the knn of a given 3D point with a maximum radius.
 */
inline void SpatialIndex::knn(const PointXYZ& p, const unsigned int k, const double radius, std::vector<PointXYZ>& res)
{
  switch(type)
  {
  case GRIDPARTITION:
  case VOXELPARTITION: grid.knn(p, k, radius, res); break;
  case QUADTREE: quadtree.knn(p, k, radius,  res); break;
  case OCTREE: octree.knn(p, k, radius, res); break;
  }

  return;
}

/*
 * PRIVATE MEMBERS
 */

inline int SpatialIndex::index_selector(const Rcpp::S4 las) const
{
  int code = GRIDPARTITION;
  int sensor = UKN;

  if (las.hasSlot("index"))
  {
    Rcpp::List index = las.slot("index");
    code = index["index"];
    sensor = index["sensor"];

    // Automatic index selection
    if (code == AUTOINDEX)
    {
      if (sensor == UKN || sensor == ALS)
        code = GRIDPARTITION;
      else if (sensor == TLS || sensor == UAV || sensor == DAP)
        code = VOXELPARTITION;
      else
        code = QUADTREE;
    }
  }

  return(code);
}

}

#endif //INDEX_H

