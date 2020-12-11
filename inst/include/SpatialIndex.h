#ifndef INDEX_H
#define INDEX_H

#include "GridPartition.h"
#include "QuadTree.h"
#include "Octree.h"

namespace lidR
{

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
  int index_selector(const Rcpp::S4 las);

private:
  GridPartition grid;
  QuadTree quadtree;
  Octree octree;
  int type;
  enum TYPES {UKN = 0, ALS = 1, TLS = 2, UAV = 3, DAP = 4, MLS = 5};
  enum INDEXES {AUTOINDEX = 0, GRIDPARTITION = 1, VOXELPARTITION = 2, QUADTREE = 3, OCTREE = 4};
};

inline SpatialIndex::SpatialIndex(const Rcpp::S4 las)
{
  type = index_selector(las);

  switch(type)
  {
  case GRIDPARTITION:
  case VOXELPARTITION: grid = GridPartition(las); break;
  case QUADTREE: quadtree = QuadTree(las); break;
  case OCTREE: octree = Octree(las); break;
  default: Rcpp::stop("Internal error: spatial index code inccorect."); break; // # no cov
  }
}

inline SpatialIndex::SpatialIndex(const Rcpp::S4 las, const std::vector<bool>& f)
{
  type = index_selector(las);

  switch(type)
  {
  case GRIDPARTITION:
  case VOXELPARTITION: grid = GridPartition(las, f); break;
  case QUADTREE: quadtree = QuadTree(las, f); break;
  case OCTREE: octree = Octree(las, f); break;
  default: Rcpp::stop("Internal error: spatial index code inccorect."); break; // # no cov
  }
}

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

inline int SpatialIndex::index_selector(const Rcpp::S4 las)
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
        code = OCTREE;
      else
        code = QUADTREE;
    }
  }

  return(code);
}

}

#endif //INDEX_H

