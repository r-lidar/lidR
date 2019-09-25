#include "GridPartition.h"

GridPartition::GridPartition(const Rcpp::NumericVector x, const Rcpp::NumericVector y)
{
  if (x.size() != y.size()) Rcpp::stop("Internal error in GridPartition. x and y have different sizes.");

  npoints = x.size();

  depth = 0;
  depth = std::floor(std::log(npoints)/std::log(4));
  depth = (depth >= 1) ? depth : 1;
  depth = (depth >= 8) ? 8 : depth;

  ncols = 1 << depth;
  nrows = 1 << depth;
  use3D = false;
  xmin  = Rcpp::min(x);
  xmax  = Rcpp::max(x);
  ymin  = Rcpp::min(y);
  ymax  = Rcpp::max(y);

  if ((xmax - xmin) == 0) {
    xmin -= (ymax - ymin)/2;
    xmax += (ymax - ymin)/2;
  }

  if ((ymax - ymin) == 0) {
    ymin -= (xmax - xmin)/2;
    ymax += (xmax - xmin)/2;
  }

  xmin -= 1;
  xmax += 1;
  ymin -= 1;
  ymax += 1;

  xres  = (xmax - xmin) / (double)ncols;
  yres  = (ymax - ymin) / (double)nrows;
  area  = ncols * nrows * xres * yres;

  registry.resize((ncols+1)*(nrows+1));

  for (int i = 0 ; i < x.size() ; i++) {
    Point p(x[i],y[i], i);
    if (!insert(p)) Rcpp::stop("Internal error in GridPartition. Point not inserted.");
  }
}

GridPartition::GridPartition(const Rcpp::NumericVector x, const Rcpp::NumericVector y, const std::vector<bool>& f)
{
  if (x.size() != y.size()) Rcpp::stop("Internal error in GridPartition. x and y have different sizes.");
  if (x.size() != (int)f.size()) Rcpp::stop("Internal error in GridPartition. x and f have different sizes.");

  npoints = std::count(f.begin(), f.end(), true);

  depth = 0;
  depth = std::floor(std::log(npoints)/std::log(4));
  depth = (depth >= 1) ? depth : 1;
  depth = (depth >= 8) ? 8 : depth;

  ncols = 1 << depth;
  nrows = 1 << depth;
  use3D = false;

  xmin  =  std::numeric_limits<double>::infinity();
  xmax  = -std::numeric_limits<double>::infinity();
  ymin  =  std::numeric_limits<double>::infinity();
  ymax  = -std::numeric_limits<double>::infinity();

  for (auto i = 0 ; i < x.size() ; i++) {
    if (x[i] < xmin) xmin = x[i];
    if (x[i] > xmax) xmax = x[i];
    if (y[i] < ymin) ymin = y[i];
    if (y[i] > ymax) ymax = y[i];
  }

  if ((xmax - xmin) == 0) {
    xmin -= (ymax - ymin)/2;
    xmax += (ymax - ymin)/2;
  }

  if ((ymax - ymin) == 0) {
    ymin -= (xmax - xmin)/2;
    ymax += (xmax - xmin)/2;
  }

  xmin -= 1;
  xmax += 1;
  ymin -= 1;
  ymax += 1;

  xres  = (xmax - xmin) / (double)ncols;
  yres  = (ymax - ymin) / (double)nrows;
  area  = ncols * nrows * xres * yres;

  registry.resize((ncols+1)*(nrows+1));

  for (auto i = 0 ; i < x.size() ; i++) {
    if (f[i]) {
      Point p(x[i], y[i], i);
      if(!insert(p)) Rcpp::stop("Internal error in GridPartition. Point not inserted.");
    }
  }
}

GridPartition::GridPartition(const Rcpp::NumericVector x, const Rcpp::NumericVector y, const Rcpp::NumericVector z) : GridPartition(x,y)
{
  if (x.size() != z.size()) Rcpp::stop("Internal error in GridPartition. x and z have different sizes.");
  this->use3D = true;
  this->Z = z;
}

GridPartition::GridPartition(const Rcpp::NumericVector x, const Rcpp::NumericVector y, const Rcpp::NumericVector z, const std::vector<bool>& f) : GridPartition(x,y,f)
{
  if (x.size() != z.size()) Rcpp::stop("Internal error in GridPartition. x and z have different sizes.");
  this->use3D = true;
  this->Z = z;
}

inline bool GridPartition::insert(const Point& p)
{
  int key = getCell(p.x, p.y);
  if (key < 0 || key >= (int)registry.size()) return false;
  registry[key].emplace_back(p);
  return true;
}

inline int GridPartition::getCell(const double x, const double y)
{
  int col = std::floor((x - xmin) / xres);
  int row = std::floor((ymax - y) / yres);
  if (y == ymin) row = nrows-1;
  if (x == xmax) col = ncols-1;
  if (row < 0 || row > (int)nrows-1 || col < 0 || col > (int)ncols-1) Rcpp::stop("Internal error");
  int cell = row * ncols + col;
  return cell;
}

void GridPartition::knn(const Point& p, const unsigned int k, std::vector<Point*>& res)
{
  double density = npoints / area;
  double radius  = std::sqrt((double)k / (density * 3.14));

  std::vector<Point*> pts;
  while (pts.size() < k && pts.size() < npoints) {
    pts.clear();
    Circle circ(p.x, p.y, radius);
    this->lookup(circ, pts);
    radius *= 1.5;
  }

  std::sort(pts.begin(), pts.end(), DSort2D<Point>(p));
  res.clear();
  for (auto i = 0 ; i < std::min((int)k, (int)pts.size()) ; i++) res.emplace_back(pts[i]);
  return;
}

void GridPartition::knn(const PointXYZ& p, const unsigned int k, std::vector<PointXYZ>& res)
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
  for (auto i = 0 ; i < std::min((int)k, (int)pts.size()) ; i++) res.emplace_back(pts[i]);
  return;
}
