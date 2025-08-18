#ifndef SPARSEPARTITION_H
#define SPARSEPARTITION_H

#include <Rcpp.h>
#include "Point.h"
#include "Shapes.h"
#include "Grid3D.h"

namespace lidR
{

class SparsePartition3D : public Grid3D
{
public:
  SparsePartition3D();
  SparsePartition3D(const Rcpp::S4 las);
  SparsePartition3D(const Rcpp::S4 las, const std::vector<bool>& filter);
  template<typename T> void lookup(T& shape, std::vector<PointXYZ>&);
  void knn(const PointXY&, const unsigned int, std::vector<PointXYZ>&);
  void knn(const PointXY&, const unsigned int, const double, std::vector<PointXYZ>&);
  void knn(const PointXYZ&, const unsigned int, std::vector<PointXYZ>&);
  void knn(const PointXYZ&, const unsigned int, const double, std::vector<PointXYZ>&);

private:
  std::unordered_map<int, std::vector<int>> heap;
};

inline SparsePartition3D::SparsePartition3D() : Grid3D()
{

}

/*
 * Default constructor using an S4 LAS object. The LAS object contains a tag
 * that enables to choose automatically between a grid- or voxel-based indexation
 */
inline SparsePartition3D::SparsePartition3D(const Rcpp::S4 las) : Grid3D(las, 0.1)
{
  // Pre-compute cell indexes and number of points per cells
  std::vector<int64_t> cell_index(X.size(), 0);
  std::unordered_map<int64_t, int> cell_points;
  for (auto i = 0 ; i < X.size() ; i++)
  {
    int64_t cell = get_cell(X[i], Y[i], Z[i]);
    cell_index[i] = cell;
    cell_points[cell]++;
  }

  // Allocate the strict amount of memory required
  // The goal is to avoid over memory allocation when using push_back, which double
  // the size of the container when resized.
  // heap.resize(ncells);
  for (const auto& it : cell_points)
    heap[it.first].reserve(it.second);

  // Insert the points. No segfault possible here because get_cell() already check
  // if the values it returns are < 0 or > ncells-1 so we are sure to do not access
  // memory beyond heap range. No need to extra security tests (hopefully)
  int64_t key;
  for (int i = 0 ; i < X.size() ; i++)
  {
    key = cell_index[i];
    heap[key].emplace_back(i);
  }
}

inline SparsePartition3D::SparsePartition3D(const Rcpp::S4 las, const std::vector<bool>& filter) : Grid3D(las, 0.1)
{
  // Pre-compute cell indexes and number of points per cells
  std::vector<int64_t> cell_index(X.size(), 0);
  std::unordered_map<int64_t, int> cell_points;
  for (auto i = 0 ; i < X.size() ; i++)
  {
    int64_t cell = get_cell(X[i], Y[i], Z[i]);
    cell_index[i] = cell;
    cell_points[cell]++;
  }

  // Allocate the strict amount of memory required
  // The goal is to avoid over memory allocation when using push_back, which double
  // the size of the container when resized.
  // heap.resize(ncells);
  for (const auto& it : cell_points)
    heap[it.first].reserve(it.second);

  // Insert the points. No segfault possible here because get_cell() already check
  // if the values it returns are < 0 or > ncells-1 so we are sure to do not access
  // memory beyond heap range. No need to extra security tests (hopefully)
  int64_t key;
  for (int i = 0 ; i < X.size() ; i++)
  {
    if (filter[i])
    {
      key = cell_index[i];
      heap[key].emplace_back(i);
    }
  }
}


/*
 * Query points within a shape. The function being templated any shape is possible
 * lidR defines some shapes in Shape.h. Some shapes are 2D (e.g. Circle) other
 * are 3D (e.g. Sphere).
 */
template<typename T> void SparsePartition3D::lookup(T& shape, std::vector<PointXYZ>& res)
{
  double xmin = shape.xmin;
  double xmax = shape.xmax;
  double ymin = shape.ymin;
  double ymax = shape.ymax;
  double zmin = shape.zmin;
  double zmax = shape.zmax;

  int64_t colmin = std::floor((xmin - this->xmin) / xres);
  int64_t colmax = std::ceil((xmax - this->xmin) / xres);
  int64_t rowmin = std::floor((ymin - this->ymin) / yres);
  int64_t rowmax = std::ceil((ymax - this->ymin) / yres);

  int64_t laymin = 0;
  int64_t laymax = nlayers;
  if (zmin > this->zmin && zmax < this->zmax)
  {
    laymin = std::floor((zmin - this->zmin) / zres);
    laymax = std::ceil((zmax - this->zmin) / zres);
  }

  int64_t cell;
  const int64_t zero = 0;

  res.clear();
  for (int64_t col = std::max(colmin, zero) ; col <= std::min(colmax, ncols-1) ; col++)
  {
    for (int64_t row = std::max(rowmin, zero) ; row <= std::min(rowmax, nrows-1) ; row++)
    {
      for (int64_t lay = std::max(laymin, zero) ; lay <= std::min(laymax, nlayers-1) ; lay++)
      {
        cell = lay * nrows * ncols + row * ncols + col;

        auto it0 = heap.find(cell);
        if (it0 == heap.end()) continue;

        for (auto it = it0->second.begin() ; it != it0->second.end() ; it++)
        {
          PointXYZ p(X[*it], Y[*it], Z[*it], *it);
          if (shape.contains(p))
            res.emplace_back(p);
        }
      }
    }
  }

  return;
}

/*
 * Query the knn of a given 2D point. In that case the Z coordinates is not
 * considered for searching the neighbors. It is a search on XY only.
 */
inline void SparsePartition3D::knn(const PointXY& p, const unsigned int k, std::vector<PointXYZ>& res)
{
  double density = npoints / area;
  double radius  = std::sqrt((double)k / (density * 3.14));

  std::vector<PointXYZ> pts;
  while (pts.size() < k && pts.size() < npoints)
  {
    pts.clear();
    Circle circ(p.x, p.y, radius);
    this->lookup(circ, pts);
    radius *= 1.5;
  }

  PointXYZ pp(p.x, p.y, 0, 0);
  std::sort(pts.begin(), pts.end(), DSort2D<PointXYZ>(pp));
  res.clear();
  for (auto i = 0 ; i < std::min((int)k, (int)pts.size()) ; i++) res.emplace_back(pts[i]);
  return;
}

/*
 * Query the knn of a given 2D point with a maximum radius search. If there are
 * less than k neighbours it returns less than k points
 */
inline void SparsePartition3D::knn(const PointXY& p, const unsigned int k, const double maxradius, std::vector<PointXYZ>& res)
{
  double density = npoints / area;
  double radius  = std::sqrt((double)k / (density * 3.14));

  std::vector<PointXYZ> pts;
  if (radius < maxradius)
  {
    while (pts.size() < k && pts.size() < npoints && radius <= maxradius) {
      pts.clear();
      Circle circ(p.x, p.y, radius);
      this->lookup(circ, pts);
      radius *= 1.5;
    }
  }

  if (radius >= maxradius)
  {
    Circle circ(p.x, p.y, maxradius);
    this->lookup(circ, pts);
  }

  PointXYZ pp(p.x, p.y, 0, 0);
  std::sort(pts.begin(), pts.end(), DSort2D<PointXYZ>(pp));
  res.clear();
  for (auto i = 0 ; i < std::min((int)k, (int)pts.size()) ; i++) res.emplace_back(pts[i]);
  return;
}

/*
 * Query the knn of a given 3D point.
 */
inline void SparsePartition3D::knn(const PointXYZ& p, const unsigned int k, std::vector<PointXYZ>& res)
{
  double density = npoints / area;
  double radius  = std::sqrt((double)k / (density * 3.14));

  std::vector<PointXYZ> pts;
  while (pts.size() < k && pts.size() < npoints) {
    pts.clear();
    Sphere sphere(p.x, p.y, p.z, radius);
    this->lookup(sphere, pts);
    radius *= 1.5;
  }

  std::sort(pts.begin(), pts.end(), DSort3D<PointXYZ>(p));
  res.clear();
  for (auto i = 0 ; i < std::min((int)k, (int)pts.size()) ; i++)
    res.emplace_back(pts[i]);
  return;
}

/*
 * Query the knn of a given 3D point with a maximum radius.
 */
inline void SparsePartition3D::knn(const PointXYZ& p, const unsigned int k, const double maxradius, std::vector<PointXYZ>& res)
{
  double density = npoints / area;
  double radius  = std::sqrt((double)k / (density * 3.14));

  std::vector<PointXYZ> pts;
  if (radius < maxradius)
  {
    while (pts.size() < k && pts.size() < npoints && radius <= maxradius)
    {
      pts.clear();
      Sphere sphere(p.x, p.y, p.z, radius);
      this->lookup(sphere, pts);
      radius *= 1.5;
    }
  }

  if (radius >= maxradius)
  {
    Sphere sphere(p.x, p.y, p.z, maxradius);
    this->lookup(sphere, pts);
  }

  std::sort(pts.begin(), pts.end(), DSort3D<PointXYZ>(p));
  res.clear();
  for (auto i = 0 ; i < std::min((int)k, (int)pts.size()) ; i++) res.emplace_back(pts[i]);
  return;
}

}

#endif //SPARSEPARTITION_H

