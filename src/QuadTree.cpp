#include "QuadTree.h"

QuadTree::QuadTree()
{
  init();
}

QuadTree::QuadTree(const double xcenter, const double ycenter, const double range)
{
  use3D = false;
  init();
  boundary = BoundingBox(Point(xcenter, ycenter), Point(range, range));
}

QuadTree::QuadTree(const BoundingBox boundary, const QuadTree* parent)
{
  init();
  this->boundary = boundary;
  this->MAX_DEPTH = parent->MAX_DEPTH;
  this->depth = parent->depth + 1;
  this->Z = parent->Z;
  this->use3D = parent->use3D;
}

QuadTree::QuadTree(Rcpp::NumericVector x, Rcpp::NumericVector y)
{
  use3D = false;
  init(x,y);
}

QuadTree::QuadTree(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericVector z)
{
  use3D = true;
  init(x,y,z);
}

QuadTree::QuadTree(Rcpp::S4 las)
{
  Rcpp::DataFrame data = Rcpp::as<Rcpp::DataFrame>(las.slot("data"));
  Rcpp::NumericVector x = data["X"];
  Rcpp::NumericVector y = data["Y"];
  Rcpp::NumericVector z = data["Z"];
  use3D = true;
  init(x,y,z);
}

QuadTree::QuadTree(Rcpp::S4 las, std::vector<bool>& f)

{
  Rcpp::DataFrame data = Rcpp::as<Rcpp::DataFrame>(las.slot("data"));
  Rcpp::NumericVector x = data["X"];
  Rcpp::NumericVector y = data["Y"];
  Rcpp::NumericVector z = data["Z"];
  use3D = true;
  init(x,y,z,f);
}

QuadTree::~QuadTree()
{
  delete NE;
  delete NW;
  delete SE;
  delete SW;
}

bool QuadTree::insert(const Point& p)
{
  if(!boundary.contains(p))
    return false;

  npoints++;

  if(depth == MAX_DEPTH)
  {
    points.push_back(p);
    return true;
  }

  if(NW == 0)
    subdivide();

  if(NW->insert(p))
    return true;
  if(NE->insert(p))
    return true;
  if(SW->insert(p))
    return true;
  if(SE->insert(p))
    return true;

  return false;
}

void QuadTree::subdivide()
{
  double half_res_half = boundary.half_res.x * 0.5;

  Point p(half_res_half+0.0001, half_res_half+0.0001);
  Point pNE(boundary.center.x + half_res_half, boundary.center.y + half_res_half);
  Point pNW(boundary.center.x - half_res_half, boundary.center.y + half_res_half);
  Point pSE(boundary.center.x + half_res_half, boundary.center.y - half_res_half);
  Point pSW(boundary.center.x - half_res_half, boundary.center.y - half_res_half);

  NE = new QuadTree(BoundingBox(pNE, p), this);
  NW = new QuadTree(BoundingBox(pNW, p), this);
  SE = new QuadTree(BoundingBox(pSE, p), this);
  SW = new QuadTree(BoundingBox(pSW, p), this);
}

void QuadTree::knn(const Point& p, const unsigned int k, std::vector<Point*>& res)
{
  double area = 4 * boundary.half_res.x * boundary.half_res.y ; // Dimension of the Quadtree
  double density = npoints / area;                              // Approx point density

  // Radius of the first circle lookup. Computed based on point density to reduce lookup iterations
  double radius = std::sqrt((double)k / (density * 3.14));

  // Get at least k point within a circle
  std::vector<Point*> pts;
  while (pts.size() < k)
  {
    pts.clear();
    Circle circ(p.x, p.y, radius);
    this->lookup(circ, pts);
    radius *= 1.5;
  }

  std::sort(pts.begin(), pts.end(), DSort2D<Point>(p));

  for (unsigned int i = 0 ; i < k ; i++)
    res.push_back(pts[i]);

  return;
}

void QuadTree::knn(const PointXYZ& p, const unsigned int k, std::vector<PointXYZ>& res)
{
  double area = 4 * boundary.half_res.x * boundary.half_res.y ; // Dimension of the Quadtree
  double density = npoints / area;                              // Approx point density

  // Radius of the first circle lookup. Computed based on point density to reduce lookup iterations
  double radius = std::sqrt((double)k / (density * 3.14));

  // Get at least k point within a sphere
  std::vector<PointXYZ> pts;
  while (pts.size() < k)
  {
    pts.clear();
    Sphere sphere(p.x, p.y, p.z, radius);
    this->lookup(sphere, pts);
    radius *= 1.5;
  }

  std::sort(pts.begin(), pts.end(), DSort3D<PointXYZ>(p));

  for (unsigned int i = 0 ; i < k ; i++)
    res.push_back(pts[i]);

  return;
}


void QuadTree::init()
{
  MAX_DEPTH = 6;
  depth = 1;
  npoints = 0;
  NE = 0;
  NW = 0;
  SE = 0;
  SW = 0;
}

void QuadTree::init(Rcpp::NumericVector x, Rcpp::NumericVector y)
{
  if (x.size() != y.size())
    throw(std::runtime_error("Internal error in QuadTree. x and y have different sizes."));

  init();
  unsigned int n = x.size();
  double xmin = Rcpp::min(x);
  double ymin = Rcpp::min(y);
  double xmax = Rcpp::max(x);
  double ymax = Rcpp::max(y);
  double xrange = xmax - xmin;
  double yrange = ymax - ymin;
  double range = xrange > yrange ? xrange/2 : yrange/2;
  boundary = BoundingBox(Point((xmin+xmax)/2, (ymin+ymax)/2), Point(range+0.001, range+0.001));

  int computed_depth = std::floor(std::log(n)/std::log(4));
  computed_depth = (computed_depth >= 1) ? computed_depth : 1;
  computed_depth = (computed_depth >= 8) ? 8 : computed_depth;
  MAX_DEPTH = computed_depth;

  for(int i = 0 ; i < x.size() ; i++)
  {
    Point p(x[i], y[i], i);
    insert(p);
  }
}

void QuadTree::init(Rcpp::NumericVector x, Rcpp::NumericVector y, std::vector<bool>& f)
{
  if (x.size() != y.size())
    throw(std::runtime_error("Internal error in QuadTree. x and y have different sizes."));

  init();
  double xmin = Rcpp::min(x);
  double ymin = Rcpp::min(y);
  double xmax = Rcpp::max(x);
  double ymax = Rcpp::max(y);
  double xrange = xmax - xmin;
  double yrange = ymax - ymin;
  double range = xrange > yrange ? xrange/2 : yrange/2;
  boundary = BoundingBox(Point((xmin+xmax)/2, (ymin+ymax)/2), Point(range+0.001, range+0.001));

  for(int i = 0 ; i < x.size() ; i++)
  {
    if (f[i])
    {
      Point p(x[i], y[i], i);
      insert(p);
    }
  }
}

void QuadTree::init(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericVector z)
{
  if (x.size() != z.size())
    throw(std::runtime_error("Internal error in QuadTree. x and z have different sizes."));

  Z = z;
  init();
  init(x,y);
}

void QuadTree::init(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::NumericVector z, std::vector<bool>& f)
{
  if (x.size() != z.size())
    throw(std::runtime_error("Internal error in QuadTree. x and z have different sizes."));

  Z = z;
  init();
  init(x,y,f);
}

BoundingBox QuadTree::bbox()
{
  return this->boundary;
}

int QuadTree::count()
{
  return this->npoints;
}
