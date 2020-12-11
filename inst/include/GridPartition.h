#ifndef GP_H
#define GP_H

#include <Rcpp.h>
#include "Point.h"
#include "Shapes.h"

namespace lidR
{

/*
 * Spatial index using a grid-based indexation. The grid-based indexation
 * can be extended with multiple layers to become a voxel-based indexation.
 * public members are:
 * - Constructors
 * - Lookup (templated to search any arbitrary shape)
 * - knn (both in 2D or 3D)
 */
class GridPartition
{
  public:
    GridPartition();
    GridPartition(const Rcpp::S4 las);
    GridPartition(const Rcpp::S4 las, const std::vector<bool>& filter);
    GridPartition(const Rcpp::NumericVector, const Rcpp::NumericVector);
    GridPartition(const Rcpp::NumericVector, const Rcpp::NumericVector, const Rcpp::NumericVector);
    template<typename T> void lookup(T& shape, std::vector<PointXYZ>&);
    void knn(const PointXY&, const unsigned int, std::vector<PointXYZ>&);
    void knn(const PointXY&, const unsigned int, const double, std::vector<PointXYZ>&);
    void knn(const PointXYZ&, const unsigned int, std::vector<PointXYZ>&);
    void knn(const PointXYZ&, const unsigned int, const double, std::vector<PointXYZ>&);

  private:
    bool multilayer;
    unsigned int  npoints;
    unsigned int ncols, nrows, nlayers, ncells;
    double xmin,ymin,xmax,ymax,zmin,zmax;
    double xres, yres, zres;
    double area, volume;
    std::vector<bool> filter;
    std::vector<std::vector<PointXYZ>> heap;
    enum TYPES {UKN = 0, ALS = 1, TLS = 2, UAV = 3, DAP = 4, MLS = 5};
    enum INDEXES {AUTOINDEX = 0, GRIDPARTITION = 1, VOXELPARTITION = 2};

  private:
    int get_cell(double, double, double);
    void build(const Rcpp::NumericVector, const Rcpp::NumericVector, const Rcpp::NumericVector);
    bool multilayered(const Rcpp::S4 las);
};

inline GridPartition::GridPartition()
{
}

/*
 * Default constructor using an S4 LAS object. The LAS object contains a tag
 * that enables to choose automatically between a grid- or voxel-based indexation
 */
inline GridPartition::GridPartition(const Rcpp::S4 las)
{
  Rcpp::DataFrame data = Rcpp::as<Rcpp::DataFrame>(las.slot("data"));
  Rcpp::NumericVector x = data["X"];
  Rcpp::NumericVector y = data["Y"];
  Rcpp::NumericVector z = data["Z"];

  // Number of points
  npoints = data.nrow();

  //Initialize a filter with true (all points are kept)
  filter.resize(npoints);
  std::fill(filter.begin(), filter.end(), true);

  // Define the search type. Most of the cases are ALS data. Despite it is 3D
  // point clouds, only a 2D index is sufficient because most of the dispersion
  // is on x-y with proportionally no dispersion on z. E.g. when searching points
  // within sphere centred on (0,0,15) a 2D search in a cylinder centred on (0,0)
  // enables to eliminate 99% of the points and there is no advantage at adding an
  // index on z. This is not true for TLS data that do need a 3D index. Ultimately
  // an octree but here we only extended the original partition to add layer index
  // capability
  multilayer = multilayered(las);

  build(x,y,z);
}

/*
 * Constructor with a filter. To work with a subset of a point cloud (e.g. first
 * return only) lidR never actually subset the point cloud because it creates an
 * intermediate (partial) copy of the point cloud. Instead lidR allocates a boolean
 * vector that is used to skip points one the fly. Points of 'las' where 'f' is false
 * are simply not inserted in the index.
 */
inline GridPartition::GridPartition(const Rcpp::S4 las, const std::vector<bool>& f)
{
  Rcpp::DataFrame data = Rcpp::as<Rcpp::DataFrame>(las.slot("data"));
  Rcpp::NumericVector x = data["X"];
  Rcpp::NumericVector y = data["Y"];
  Rcpp::NumericVector z = data["Z"];

  // Number of points
  npoints = std::accumulate(f.begin(), f.end(), 0);

  //Initialize a filter to discard some points on the fly
  std::copy(f.begin(), f.end(), std::back_inserter(filter));

  // Define the search type. Most of the cases are ALS data. Despite it is 3D
  // point clouds, only a 2D index is sufficient because most of the dispersion
  // is on x-y with proportionally no dispersion on z. e.g. when searching points
  // within sphere centred on (0,0,15) a 2D search in a cylinder centred on (0,0)
  // enables to eliminate 99% of the points and there is no advantage at adding an
  // index on z. This is not true for TLS data that do need a 3D index. Ultimately
  // an octree but here we only extended the original partition to add layer index
  // capability
  multilayer = multilayered(las);

  build(x,y,z);
}

/*
 * Legacy constructor for 2D data. Should not be used but is still used in lidR
 * for code that need refactoring + to build simple unit tests.
 */
inline GridPartition::GridPartition(const Rcpp::NumericVector x, const Rcpp::NumericVector y)
{
  if (x.size() != y.size())
    Rcpp::stop("Internal error in spatial index: x and y have different sizes."); // # nocov

  // Number of points
  npoints = x.size();

  // Initialize a filter with true (all points are kept)
  filter.resize(npoints);
  std::fill(filter.begin(), filter.end(), true);

  // Use legacy 2D index
  multilayer = false;

  // Create a dummy z vector with 0s
  Rcpp::NumericVector z(npoints);

  build(x,y,z);
}

/*
 * Legacy constructor for 3D data. Should not be used but is still used in lidR
 * for code that need refactoring + to build simple unit tests.
 */
inline GridPartition::GridPartition(const Rcpp::NumericVector x, const Rcpp::NumericVector y, const Rcpp::NumericVector z)
{
  if (x.size() != y.size())
    Rcpp::stop("Internal error in spatial index: x and y have different sizes."); // # nocov

  if (x.size() != z.size())
    Rcpp::stop("Internal error in spatial index: x and z have different sizes."); // # nocov

  // Number of points
  npoints = x.size();

  // Initialize a filter with true (all points are kept)
  filter.resize(npoints);
  std::fill(filter.begin(), filter.end(), true);

  // Use legacy 2D index
  multilayer = true;

  build(x,y,z);
}

/*
 * Query points within a shape. The function being templated any shape is possible
 * lidR defines some shapes in Shape.h. Some shapes are 2D (e.g. Circle) other
 * are 3D (e.g. Sphere).
 */
template<typename T> void GridPartition::lookup(T& shape, std::vector<PointXYZ>& res)
{
  double xmin = shape.xmin;
  double xmax = shape.xmax;
  double ymin = shape.ymin;
  double ymax = shape.ymax;
  double zmin = shape.zmin;
  double zmax = shape.zmax;

  int colmin = std::floor((xmin - this->xmin) / xres);
  int colmax = std::ceil((xmax - this->xmin) / xres);
  int rowmin = std::floor((this->ymax - ymax) / yres);
  int rowmax = std::ceil((this->ymax - ymin) / yres);
  int laymin = std::floor((zmin - this->zmin) / zres);
  int laymax = std::ceil((zmax - this->zmin) / zres);
  int cell;

  res.clear();
  for (int col = std::max(colmin,0) ; col <= std::min(colmax, (int)ncols-1) ; col++) {
    for (int row = std::max(rowmin,0) ; row <= std::min(rowmax, (int)nrows-1) ; row++) {
      for (int lay = std::max(laymin,0) ; lay <= std::min(laymax, (int)nlayers-1) ; lay++) {
        cell = lay * nrows * ncols + row * ncols + col;
        for (std::vector<PointXYZ>::iterator it = heap[cell].begin() ; it != heap[cell].end() ; it++) {
          if (shape.contains(*it))
            res.emplace_back(*it);
        }
      }
    }
  }

  return;
}

/*
 * Query the knn of a given 2D point. In that case the Z coordinates is not
 * considered for searching the neighbours. It is a search on XY only.
 */
inline void GridPartition::knn(const PointXY& p, const unsigned int k, std::vector<PointXYZ>& res)
{
  double density = npoints / area;
  double radius  = std::sqrt((double)k / (density * 3.14));

  std::vector<PointXYZ> pts;
  while (pts.size() < k && pts.size() < npoints) {
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
inline void GridPartition::knn(const PointXY& p, const unsigned int k, const double maxradius, std::vector<PointXYZ>& res)
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
inline void GridPartition::knn(const PointXYZ& p, const unsigned int k, std::vector<PointXYZ>& res)
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
inline void GridPartition::knn(const PointXYZ& p, const unsigned int k, const double maxradius, std::vector<PointXYZ>& res)
{
  double density = npoints / area;
  double radius  = std::sqrt((double)k / (density * 3.14));

  std::vector<PointXYZ> pts;
  if (radius < maxradius)
  {
    while (pts.size() < k && pts.size() < npoints && radius <= maxradius) {
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

inline void GridPartition::build(const Rcpp::NumericVector x, const Rcpp::NumericVector y,  const Rcpp::NumericVector z)
{
  //if (npoints == 0)
  //  Rcpp::stop("Internal error in spatial index: impossible to build an index with 0 points."); // # nocov

  // Compute the bounding box
  xmin =  XYINF;
  xmax = -XYINF;
  ymin =  XYINF;
  ymax = -XYINF;
  zmin =  ZINF;
  zmax = -ZINF;

  for (auto i = 0 ; i < x.size() ; i++) {
    if (x[i] < xmin) xmin = x[i];
    if (x[i] > xmax) xmax = x[i];
    if (y[i] < ymin) ymin = y[i];
    if (y[i] > ymax) ymax = y[i];
    if (z[i] < zmin) zmin = z[i];
    if (z[i] > zmax) zmax = z[i];
  }

  double buf = 1;
  xmin -= buf;
  xmax += buf;
  ymin -= buf;
  ymax += buf;
  zmin -= buf;
  zmax += buf;

  // Historically the spatial index was a quadtree defined by a depth
  // The depth is still used to compute the number of cells
  int depth = 0;
  depth = std::floor(std::log(npoints)/std::log(4));
  depth = (depth >= 0) ? depth : 0;
  depth = (depth >= 8) ? 8 : depth;
  ncells = (1 << depth) * (1 << depth);

  // Compute some indicator of shape
  double xrange = xmax - xmin;
  double yrange = ymax - ymin;
  double zrange = zmax - zmin;
  double xyratio = xrange/yrange;
  double xzratio = xrange/zrange;
  //double yzratio = yrange/zrange;

  if (!multilayer)
  {
    // Compute the number of rows and columns in such a way that there is approximately
    // the number of wanted cells but the organization of the cell is well balanced
    // so the resolutions on x-y are close. We want:
    // ncols/nrows = xyratio
    // ncols*nrows = ncells
    ncols = std::round(std::sqrt(ncells*xyratio));
    if (ncols <= 0) ncols = 1;
    nrows = std::round(ncols/xyratio);
    if (nrows <= 0) nrows = 1;
    nlayers = 1;

    ncells = ncols*nrows*nlayers;
  }
  else
  {
    // Compute the number of rows and columns in such a way that there is approximately
    // the number of wanted cells but the organization of the cell is well balanced
    // so the resolutions on x-y-z are close. We want:
    // ncols/nrows = xyratio
    // ncols/nlays = xzratio
    // nrows/nlays = yzratio
    // ncols*nrows*nlayers = ncells
    ncols = std::round(std::cbrt(ncells*xyratio*xzratio));
    if (ncols <= 0) ncols = 1;
    nrows = std::round(ncols/xyratio);
    if (nrows <= 0) nrows = 1;
    nlayers = std::round(ncols/xzratio);
    if (nlayers <= 0) nlayers = 1;

    ncells = ncols*nrows*nlayers;
  }

  xres = xrange / (double)ncols;
  yres = yrange / (double)nrows;
  zres = zrange / (double)nlayers;

  area = xrange * yrange;
  volume = area * zrange;

  // Precompute cell indexes and number of points per cells
  std::vector<int> cell_index(x.size(), 0);
  std::vector<unsigned int> cell_points(ncells, 0);
  for (auto i = 0 ; i < x.size() ; i++) {
    int cell = get_cell(x[i], y[i], z[i]);
    cell_index[i] = cell;
    cell_points[cell]++;
  }

  // Allocate the strict amount of memory required
  // The goal is to avoid over memory allocation when using push_back, which double
  // the size of the container when resized.
  heap.resize(ncells);
  for (unsigned int i = 0 ; i < ncells; i++)
    heap[i].reserve(cell_points[i]);

  //Rprintf("Partition [%.2lf, %.2lf] x [%.2lf, %.2lf] x [%.2lf, %.2lf] with %d cells in %d layers \n", xmin,xmax,ymin,ymax,zmin,zmax,ncells, nlayers);

  // Insert the points. No segfault possible here because get_cell() already check
  // if the values it returns are < 0 or > ncells-1 so we are sure to do not access
  // memory beyond heap range. No need to extra security tests (hopefully)
  unsigned int key;
  for (int i = 0 ; i < x.size() ; i++) {
    if (filter[i]) {
      key = cell_index[i];
      heap[key].emplace_back(x[i], y[i], z[i], i);
    }
  }
}

inline int GridPartition::get_cell(double x, double y, double z)
{
  int col = std::floor((x - xmin) / xres);
  int row = std::floor((ymax - y) / yres);
  int lay = std::floor((z - zmin) / zres);
  if (row < 0 || row > (int)nrows-1 || col < 0 || col > (int)ncols-1 || lay < 0 || lay > (int)nlayers-1)
    Rcpp::stop("Internal error in spatial index: point out of the range."); // # nocov
  int cell = lay * nrows * ncols + row * ncols + col;
  if (cell < 0 || cell >= (int)ncells)
    Rcpp::stop("Internal error in spatial index: cell out of the range."); // # nocov
  return cell;
}

inline bool GridPartition::multilayered(const Rcpp::S4 las)
{
  if (las.hasSlot("index"))
  {
    Rcpp::List index = las.slot("index");
    int code = index["index"];
    int sensor = index["sensor"];

    // Automatic index selection
    if (code == AUTOINDEX)
    {
      if (sensor == TLS || sensor == UAV || sensor == DAP)
        return true;
      else
        return false;
    }
    else
    {
      if (code == GRIDPARTITION)
        return false;
      else if (code == VOXELPARTITION)
        return true;
      else
        Rcpp::stop("Internal error in GridPartition: las object registered a spatial index that is not a partition."); // # nocov
    }
  }

  // Backward compatibility
  return false; // # nocov
}

}

#endif //GP_H
