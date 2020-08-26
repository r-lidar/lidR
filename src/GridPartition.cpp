#include "GridPartition.h"

GridPartition::GridPartition(const Rcpp::S4 las)
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
  int search_type = 0;
  if (las.hasSlot("type")) search_type = las.slot("type");
  setLayers(search_type);

  init(x,y,z);
}

GridPartition::GridPartition(const Rcpp::S4 las, const std::vector<bool>& f)
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
  int search_type = 0;
  if (las.hasSlot("type")) search_type = las.slot("type");
  setLayers(search_type);

  init(x,y,z);
}

GridPartition::GridPartition(const Rcpp::NumericVector x, const Rcpp::NumericVector y)
{
  if (x.size() != y.size())
    Rcpp::stop("Internal error in spatial index: x and y have different sizes.");

  // Number of points
  npoints = x.size();

  // Initialize a filter with true (all points are kept)
  filter.resize(npoints);
  std::fill(filter.begin(), filter.end(), true);

  // Use legacy 2D index
  nlayers = 1;

  // Create a dummy z vector with 0s
  Rcpp::NumericVector z(npoints, 0);

  init(x,y,z);
}

GridPartition::GridPartition(const Rcpp::NumericVector x, const Rcpp::NumericVector y,  const Rcpp::NumericVector z)
{
  if (x.size() != y.size())
    Rcpp::stop("Internal error in spatial index: x and y have different sizes.");

  if (x.size() != z.size())
    Rcpp::stop("Internal error in spatial index: x and z have different sizes.");

  // Number of points
  npoints = x.size();

  // Initialize a filter with true (all points are kept)
  filter.resize(npoints);
  std::fill(filter.begin(), filter.end(), true);

  // Use legacy 2D index
  nlayers = 1;

  init(x,y,z);
}

void GridPartition::setLayers(const int search_type)
{
  // nlayers = 1 is equivalent to legacy code with no layer defined (was implicitly one)
  // with other search type we can add layers to get a z index.
  switch (search_type)
  {
  case 0: // Legacy
    nlayers = 1;
    break;
  case 1: // ALS
    nlayers = 1;
    break;
  case 2: // TLS
    nlayers = 10;
    break;
  case 3: // Normalized ALS
    nlayers = 1;
    break;
  default:
    Rcpp::stop("Internal error in spatial index: the LAS object has a slot 'type' that is not recognized.");
  }
}

void GridPartition::init(const Rcpp::NumericVector x, const Rcpp::NumericVector y,  const Rcpp::NumericVector z)
{
  if (npoints == 0)
    Rcpp::stop("Internal error in spatial index: impossible to build an index with 0 points.");

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
  // The depth is still use to compute the number of cells
  int depth = 0;
  depth = std::floor(std::log(npoints)/std::log(4));
  depth = (depth >= 0) ? depth : 0;
  depth = (depth >= 8) ? 8 : depth;
  unsigned int ncells = (1 << depth) * (1 << depth);

  // Compute some indicator of shape
  double xrange = xmax - xmin;
  double yrange = ymax - ymin;
  double zrange = zmax - zmin;
  double ratio = xrange/yrange;

  // Compute the number of rows and columns in such a way that there is approximately
  // the number of wanted cells but the organization of the cell is well balanced
  // so the resolutions on x-y are close. We want:
  // ncols/nrows = ratio
  // ncols*nrows*nlayers = ncells
  nrows = std::round(std::sqrt(ncells/(ratio*nlayers)));
  if (nrows <= 0) nrows = 1;
  ncols = std::round(ncells/(nrows*nlayers));
  if (ncols <= 0) ncols = 1;
  ncells = ncols*nrows*nlayers;

  xres = xrange / (double)ncols;
  yres = yrange / (double)nrows;
  zres = zrange / (double)nlayers;

  //Rprintf("Spatial index of %d points with %d x %d x %d = %d cells \n", npoints, ncols, nrows, nlayers, ncells);
  //Rprintf("xres = %.3lf yres = %.3lf zres = %.3lf \n", xres, yres, zres);

  area = xrange * yrange;
  volume = area * zrange;

  registry.resize(ncells);

  for (int i = 0 ; i < x.size() ; i++) {
    if (filter[i]) {
      PointXYZ p(x[i],y[i], z[i], i);
      if (!insert(p)) Rcpp::stop("Internal error in GridPartition. Point not inserted.");
    }
  }
}

inline bool GridPartition::insert(const PointXYZ& p)
{
  int key = getCell(p);
  if (key < 0 || key >= (int)registry.size()) return false;
  registry[key].emplace_back(p);
  return true;
}

inline int GridPartition::getCell(const PointXYZ& p)
{
  int col = std::floor((p.x - xmin) / xres);
  int row = std::floor((ymax - p.y) / yres);
  int lay = std::floor((p.z - zmin) / zres);
  if (row < 0 || row > (int)nrows-1 || col < 0 || col > (int)ncols-1 || lay < 0 || lay > (int)nlayers-1)
    Rcpp::stop("Internal error in spatial index: point out of the range.");
  int cell = lay * nrows * ncols + row * ncols + col;
  return cell;
}

void GridPartition::knn(const Point& p, const unsigned int k, std::vector<PointXYZ*>& res)
{
  double density = npoints / area;
  double radius  = std::sqrt((double)k / (density * 3.14));

  std::vector<PointXYZ*> pts;
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

void GridPartition::knn(const Point& p, const unsigned int k, const double maxradius, std::vector<PointXYZ*>& res)
{
  double density = npoints / area;
  double radius  = std::sqrt((double)k / (density * 3.14));

  std::vector<PointXYZ*> pts;
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

void GridPartition::knn(const PointXYZ& p, const unsigned int k, std::vector<PointXYZ*>& res)
{
  double density = npoints / area;
  double radius  = std::sqrt((double)k / (density * 3.14));

  std::vector<PointXYZ*> pts;
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

void GridPartition::knn(const PointXYZ& p, const unsigned int k, const double maxradius, std::vector<PointXYZ*>& res)
{
  double density = npoints / area;
  double radius  = std::sqrt((double)k / (density * 3.14));

  std::vector<PointXYZ*> pts;
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
