#include "QuadTree.h"
#include <cmath>
#include <limits>
#include <algorithm>
#include <iostream>

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

QuadTree::QuadTree(const double cx, const double cy, const double range)
{
  MAX_DEPTH = 6;
  EPSILON = 0.001;
  EPSILONSQ = EPSILON*EPSILON;
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
  EPSILON = 0.001;
  EPSILONSQ = EPSILON*EPSILON;
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

  QuadTree *tree = new QuadTree( (xmin+xmax)/2, (ymin+ymax)/2, range+0.01);

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

  Point p(half_res_half+EPSILONSQ, half_res_half+EPSILONSQ);
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
  double half_width = (rmaxx - rminx)/2 + EPSILON;
  double half_height = (rmaxy - rminy )/2 + EPSILON;

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
  double dx = bb.center.x - p.x;
  double dy = bb.center.y - p.y;
  dx = dx < 0 ? -dx : dx;
  dy = dy < 0 ? -dy : dy;

  return(dx <= bb.half_res.x && dy <= bb.half_res.y);
}

bool QuadTree::in_triangle(const Point& p, const Point& p0, const Point& p1, const Point& p2)
{
  double denominator = (p0.x*(p1.y - p2.y) + p0.y*(p2.x - p1.x) + p1.x*p2.y - p1.y*p2.x);
  double t1 = (p.x*(p2.y - p0.y) + p.y*(p0.x - p2.x) - p0.x*p2.y + p0.y*p2.x) / denominator;
  double t2 = (p.x*(p1.y - p0.y) + p.y*(p0.x - p1.x) - p0.x*p1.y + p0.y*p1.x) / -denominator;
  double s = t1 + t2;

  if (0 <= t1 && t1 <= 1 && 0 <= t2 && t2 <= 1 && s <= 1)
    return true;

  // see http://totologic.blogspot.com/2014/01/accurate-point-in-triangle-test.html

  if (distanceSquarePointToSegment(p0, p1, p) <= EPSILONSQ)
    return true;
  if (distanceSquarePointToSegment(p1, p2, p) <= EPSILONSQ)
    return true;
  if (distanceSquarePointToSegment(p2, p0, p) <= EPSILONSQ)
    return true;

  return false;
}

double QuadTree::distanceSquarePointToSegment(const Point& p1, const Point& p2, const Point& p)
{
  double p1_p2_squareLength = (p2.x - p1.x)*(p2.x - p1.x) + (p2.y - p1.y)*(p2.y - p1.y);
  double dotProduct = ((p.x - p1.x)*(p2.x - p1.x) + (p.y - p1.y)*(p2.y - p1.y)) / p1_p2_squareLength;

  if ( dotProduct < 0 )
  {
    return (p.x - p1.x)*(p.x - p1.x) + (p.y - p1.y)*(p.y - p1.y);
  }
  else if ( dotProduct <= 1 )
  {
    double p_p1_squareLength = (p1.x - p.x)*(p1.x - p.x) + (p1.y - p.y)*(p1.y - p.y);
    return p_p1_squareLength - dotProduct * dotProduct * p1_p2_squareLength;
  }
  else
  {
    return (p.x - p2.x)*(p.x - p2.x) + (p.y - p2.y)*(p.y - p2.y);
  }
}

BoundingBox QuadTree::bbox()
{
  return this->boundary;
}

int QuadTree::count()
{
  return this->npoints;
}
