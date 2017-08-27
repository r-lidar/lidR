#include "QuadTree.h"
#include <cmath>
#include <limits>
#include <algorithm>
#include <iostream>

Point::Point(){}
Point::Point(const double x, const double y) : x(x), y(y), id(0) {}
Point::Point(const double x, const double y, const int id) : x(x), y(y), id(id) {}

BoundingBox::BoundingBox(){}
BoundingBox::BoundingBox(const Point center, const Point half_res) : center(center), half_res(half_res) {}

double dist(const Point& lhs, const Point& rhs)
{
  double dx = lhs.x - rhs.x;
  double dy = lhs.y - rhs.y;
  return dx * dx + dy * dy;
}

static inline double max (double a, double b, double c)
{
  if (a < b)
    return (b < c ? c : b);
  else
    return (a < c ? c : a);
}

static inline double min (double a, double b, double c)
{
  if (a > b)
    return (b > c ? c : b);
  else
    return (a > c ? c : a);
}

bool BoundingBox::contains(const Point& p)
{
  if(p.x >= center.x - half_res.x &&
     p.x <= center.x + half_res.x &&
     p.y >= center.y - half_res.y &&
     p.y <= center.y + half_res.y)
    return true;
  else
    return false;
}

bool BoundingBox::intersects(const BoundingBox& b)
{

  if(center.x - half_res.x <= b.center.x + b.half_res.x &&
     center.x + half_res.x >= b.center.x - b.half_res.x &&
     center.y - half_res.y <= b.center.y + b.half_res.y &&
     center.y + half_res.y >= b.center.y - b.half_res.y)
  {
    return true;
  }
  else
    return false;
}

QuadTree::QuadTree(const double cx, const double cy, const double range)
{
  MAX_DEPTH = 6;
  npoints = 0;

  boundary = BoundingBox(Point(cx, cy), Point(range, range));
  depth = 1;

  NE = 0;
  NW = 0;
  SE = 0;
  SW = 0;
}

QuadTree::QuadTree(const BoundingBox boundary, const QuadTree* parent) : boundary(boundary)
{
  MAX_DEPTH = 6;
  npoints = 0;

  depth = parent->depth + 1;

  NE = 0;
  NW = 0;
  SE = 0;
  SW = 0;
}

QuadTree::~QuadTree()
{
  delete NE;
  delete NW;
  delete SE;
  delete SW;
}

QuadTree* QuadTree::create(const std::vector<double> x, const std::vector<double> y)
{
  int n = x.size();

  double xmin = x[0];
  double ymin = y[0];
  double xmax = x[0];
  double ymax = y[0];

  for(int i = 0 ; i < n ; i++)
  {
    if(x[i] < xmin)
      xmin = x[i];
    else if(x[i] > xmax)
      xmax = x[i];
    if(y[i] < ymin)
      ymin = y[i];
    else if(y[i] > ymax)
      ymax = y[i];
  }

  double xrange = xmax - xmin;
  double yrange = ymax - ymin;
  double range = xrange > yrange ? xrange/2 : yrange/2;

  QuadTree *tree = new QuadTree( (xmin+xmax)/2, (ymin+ymax)/2, range*1.01);

  for(int i = 0 ; i < n ; i++)
  {
    Point p(x[i], y[i], i);
    tree->insert(p);
  }

  return tree;
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

  Point p(half_res_half, half_res_half);
  Point pNE(boundary.center.x + half_res_half, boundary.center.y + half_res_half);
  Point pNW(boundary.center.x - half_res_half, boundary.center.y + half_res_half);
  Point pSE(boundary.center.x + half_res_half, boundary.center.y - half_res_half);
  Point pSW(boundary.center.x - half_res_half, boundary.center.y - half_res_half);

  NE = new QuadTree(BoundingBox(pNE, p), this);
  NW = new QuadTree(BoundingBox(pNW, p), this);
  SE = new QuadTree(BoundingBox(pSE, p), this);
  SW = new QuadTree(BoundingBox(pSW, p), this);
}

void QuadTree::range_lookup(const BoundingBox bb, std::vector<Point*>& res, const int method)
{
  if(!boundary.intersects(bb))
    return;

  if(depth == MAX_DEPTH)
  {
    switch(method)
    {
    case 1: getPointsSquare(bb, points, res);
      break;

    case 2: getPointsCircle(bb, points, res);
      break;
    }
  }

  if(NW == 0)
    return;

  NE->range_lookup(bb, res, method);
  NW->range_lookup(bb, res, method);
  SE->range_lookup(bb, res, method);
  SW->range_lookup(bb, res, method);

  return;
}

void QuadTree::rect_lookup(const double xc, const double yc, const double half_width, const double half_height, std::vector<Point*>& res)
{
  range_lookup(BoundingBox(Point(xc, yc), Point(half_width, half_height)), res, 1);
  return;
}


void QuadTree::circle_lookup(const double cx, const double cy, const double range, std::vector<Point*>& res)
{
  range_lookup(BoundingBox(Point(cx, cy), Point(range, range)), res, 2);
  return;
}

void QuadTree::triangle_lookup(const Point& A, const Point& B, const Point& C, std::vector<Point*>& res)
{
  // Boundingbox of A B C
  double rminx = min(A.x, B.x, C.x);
  double rmaxx = max(A.x, B.x, C.x);
  double rminy = min(A.y, B.y, C.y);
  double rmaxy = max(A.y, B.y, C.y);

  double xcenter = (rminx + rmaxx)/2;
  double ycenter = (rminy + rmaxy)/2;
  double half_width = (rmaxx - rminx)/2;
  double half_height = (rmaxy - rminy )/2;

  // Boundingbox lookup
  std::vector<Point*> points;
  rect_lookup(xcenter, ycenter, half_width, half_height, points);

  // Compute if the points are in A B C
  for(std::vector<Point*>::iterator it = points.begin(); it != points.end(); it++)
  {
    if (in_triangle(**it, A, B, C))
      res.push_back(*it);
  }

  return;
}

void QuadTree::knn_lookup(const double cx, const double cy, const int k, std::vector<Point*>& res)
{
  double area = 4 * boundary.half_res.x * boundary.half_res.y ; // Dimension of the Quadtree
  double density = npoints / area;                              // Approx point density

  // Radius of the first circle lookup. Computed based on point density to reduce lookup iterations
  double radius = std::sqrt((double)k / (density * 3.14));

  Point p(cx, cy);
  std::vector<Point*> pts;

  // Get at least k point within a circle
  int n = 0;
  while (n < k)
  {
    pts.clear();
    circle_lookup(p.x, p.y, radius, pts);
    n = pts.size();
    radius *= 1.5;
  }

  std::sort(pts.begin(), pts.end(), DistanceFunc(p));

  for (int i = 0 ; i < k ; i++)
    res.push_back(pts[i]);

  return;
}

void QuadTree::getPointsSquare(const BoundingBox bb, std::vector<Point>& points, std::vector<Point*>& res)
{
  for(std::vector<Point>::iterator it = points.begin(); it != points.end(); it++)
  {
    if(in_rect(bb, *it))
      res.push_back(&(*it));
  }
  return;
}

void QuadTree::getPointsCircle(const BoundingBox bb, std::vector<Point>& points, std::vector<Point*>& res)
{
  for(std::vector<Point>::iterator it = points.begin(); it != points.end(); it++)
  {
    if(in_circle(bb.center, (*it), bb.half_res.x))
      res.push_back(&(*it));
  }
  return;
}

bool QuadTree::in_circle(const Point& p1, const Point& p2, const double r)
{
  double A = p1.x - p2.x;
  double B = p1.y - p2.y;
  double d = sqrt(A*A + B*B);

  return(d <= r);
}

bool QuadTree::in_rect(const BoundingBox& bb, const Point& p)
{
  double A = bb.center.x - p.x;
  double B = bb.center.y - p.y;
  A = A < 0 ? -A : A;
  B = B < 0 ? -B : B;

  return(A <= bb.half_res.x && B <= bb.half_res.y);
}

bool QuadTree::in_triangle(const Point& p, const Point& p0, const Point& p1, const Point& p2)
{
  double s = p0.y * p2.x - p0.x * p2.y + (p2.y - p0.y) * p.x + (p0.x - p2.x) * p.y;
  double t = p0.x * p1.y - p0.y * p1.x + (p0.y - p1.y) * p.x + (p1.x - p0.x) * p.y;

  if ((s <= 0) != (t <= 0))
    return false;

  double  A = -p1.y * p2.x + p0.y * (p2.x - p1.x) + p0.x * (p1.y - p2.y) + p1.x * p2.y;

  if (A < 0)
  {
    s = -s;
    t = -t;
    A = -A;
  }

  return s >= 0 && t >= 0 && (int)((s + t)*1000) <= (int)(A*1000);
}

BoundingBox QuadTree::bbox()
{
  return this->boundary;
}

int QuadTree::count()
{
  return this->npoints;
}
