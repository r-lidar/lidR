#include "Shapes.h"
#include <algorithm>

#define EPSILON 2e-6

Shape::Shape()
{
  this->bbox = BoundingBox();
}

Shape::Shape(double xmin, double xmax, double ymin, double ymax)
{
  Point center;
  Point half_res;
  center.x = (xmax + xmin)/2;
  center.y = (ymax + ymin)/2;
  half_res.x = (xmax - xmin)/2;
  half_res.y = (ymax - ymin)/2;
  this->bbox = BoundingBox(center, half_res);
}

Shape::Shape(double xcenter, double ycenter, double radius)
{
  Point center;
  Point half_res;
  center.x = xcenter;
  center.y = ycenter;
  half_res.x = radius;
  half_res.y = radius;
  this->bbox = BoundingBox(center, half_res);
}

bool Shape::contains(const Point& p)
{
  return(p.x >= bbox.center.x - bbox.half_res.x - EPSILON &&
         p.x <= bbox.center.x + bbox.half_res.x + EPSILON &&
         p.y >= bbox.center.y - bbox.half_res.y - EPSILON &&
         p.y <= bbox.center.y + bbox.half_res.y + EPSILON);
}

Rectangle::Rectangle(double xmin, double xmax, double ymin, double ymax) : Shape(xmin, xmax, ymin, ymax)
{
  A.x = xmin;
  B.x = xmax;
  A.y = ymin;
  B.y = ymax;
}

bool Rectangle::contains(const Point& p)
{
  return(p.x >= A.x - EPSILON &&
         p.x <= B.x + EPSILON &&
         p.y >= A.y - EPSILON &&
         p.y <= B.y + EPSILON);
}

#include <stdio.h>

OrientedRectangle::OrientedRectangle(double xmin, double xmax, double ymin, double ymax, double angle)
{
  // Rectangle center
  double cx = (xmax + xmin)/2;
  double cy = (ymax + ymin)/2;

  // translate points to origin
  A.x = xmin - cx;
  B.x = xmax - cx;
  C.x = xmax - cx;
  D.x = xmin - cx;
  A.y = ymin - cy;
  B.y = ymin - cy;
  C.y = ymax - cy;
  D.y = ymax - cy;

  // apply rotation
  double rAx = A.x*cos(angle) - A.y*sin(angle);
  double rAy = A.x*sin(angle) + A.y*cos(angle);
  double rBx = B.x*cos(angle) - B.y*sin(angle);
  double rBy = B.x*sin(angle) + B.y*cos(angle);
  double rCx = C.x*cos(angle) - C.y*sin(angle);
  double rCy = C.x*sin(angle) + C.y*cos(angle);
  double rDx = D.x*cos(angle) - D.y*sin(angle);
  double rDy = D.x*sin(angle) + D.y*cos(angle);

  // translate back
  A.x = rAx + cx;
  A.y = rAy + cy;
  B.x = rBx + cx;
  B.y = rBy + cy;
  C.x = rCx + cx;
  C.y = rCy + cy;
  D.x = rDx + cx;
  D.y = rDy + cy;

  // Update the bounding box
  std::vector<double> x = {A.x, B.x, C.x, D.x};
  std::vector<double> y = {A.y, B.y, C.y, D.y};
  xmin = *std::min_element(std::begin(x), std::end(x));
  ymin = *std::min_element(std::begin(y), std::end(y));
  xmax = *std::max_element(std::begin(x), std::end(x));
  ymax = *std::max_element(std::begin(y), std::end(y));
  Point center;
  Point half_res;
  center.x = cx;
  center.y = cy;
  half_res.x = (xmax-xmin)/2;
  half_res.y = (ymax-ymin)/2;
  this->bbox = BoundingBox(center, half_res);

  //printf("(%lf %lf) (%lf %lf) (%lf %lf) (%lf %lf)", A.x, A.y, B.x, B.y, C.x, C.y, D.x, D.y);
}

bool OrientedRectangle::contains(const Point& p)
{
  if ( (B.x - A.x) * (p.y - A.y) - (p.x - A.x) * (B.y - A.y) < 0)
    return false;

  if ( (C.x - B.x) * (p.y - B.y) - (p.x - B.x) * (C.y - B.y) < 0)
    return false;

  if ( (D.x - C.x) * (p.y - C.y) - (p.x - C.x) * (D.y - C.y) < 0)
    return false;

  if ( (A.x - D.x) * (p.y - D.y) - (p.x - D.x) * (A.y - D.y) < 0)
    return false;

  return true;
}

Circle::Circle(double xcenter, double ycenter, double radius) : Shape(xcenter, ycenter, radius)
{
  center.x = xcenter;
  center.y = ycenter;
  this->radius = radius;
}

bool Circle::contains(const Point& p)
{
  double A = center.x - p.x;
  double B = center.y - p.y;
  double d = A*A + B*B;
  return d <= radius*radius + EPSILON;
}

Triangle::Triangle(Point& A, Point& B, Point& C) : Shape()
{
  double xmin = min(A.x, B.x, C.x);
  double ymin = min(A.y, B.y, C.y);
  double xmax = max(A.x, B.x, C.x);
  double ymax = max(A.y, B.y, C.y);
  Shape s(xmin,xmax, ymin, ymax);
  this->A = A;
  this->B = B;
  this->C = C;
  this->bbox = s.bbox;
}

bool Triangle::contains(const Point& p)
{
  if (!this->bbox.contains(p, EPSILON))
    return false;

  double denominator = (A.x*(B.y - C.y) + A.y*(C.x - B.x) + B.x*C.y - B.y*C.x);
  double t1 = (p.x*(C.y - A.y) + p.y*(A.x - C.x) - A.x*C.y + A.y*C.x) / denominator;
  double t2 = (p.x*(B.y - A.y) + p.y*(A.x - B.x) - A.x*B.y + A.y*B.x) / -denominator;
  double s = t1 + t2;

  if (0 <= t1 && t1 <= 1 && 0 <= t2 && t2 <= 1 && s <= 1)
    return true;

  // see http://totologic.blogspot.com/2014/01/accurate-point-in-triangle-test.html

  if (distanceSquarePointToSegment(A, B, p) <= EPSILON)
    return true;
  if (distanceSquarePointToSegment(B, C, p) <= EPSILON)
    return true;
  if (distanceSquarePointToSegment(C, A, p) <= EPSILON)
    return true;

  return false;
}

Sphere::Sphere(double xcenter, double ycenter, double zcenter, double radius) : Shape(xcenter, ycenter, radius)
{
  center.x = xcenter;
  center.y = ycenter;
  center.z = zcenter;
  this->radius = radius;
}

bool Sphere::contains(const PointXYZ& p)
{
  double A = center.x - p.x;
  double B = center.y - p.y;
  double C = center.z - p.z;
  double d = A*A + B*B + C*C;
  return(d <= radius*radius + EPSILON);
}
