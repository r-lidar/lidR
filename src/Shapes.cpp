#include "Shapes.h"

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
  return(p.x >= bbox.center.x - bbox.half_res.x && p.x <= bbox.center.x + bbox.half_res.x && p.y >= bbox.center.y - bbox.half_res.y && p.y <= bbox.center.y + bbox.half_res.y);
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
  return(p.x >= A.x && p.x <= B.x && p.y >= A.y && p.y <= B.y);
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
  return d <= radius*radius;
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
  #define buffer 0.001*0.001

  if (!this->bbox.contains(p, buffer))
    return false;

  double denominator = (A.x*(B.y - C.y) + A.y*(C.x - B.x) + B.x*C.y - B.y*C.x);
  double t1 = (p.x*(C.y - A.y) + p.y*(A.x - C.x) - A.x*C.y + A.y*C.x) / denominator;
  double t2 = (p.x*(B.y - A.y) + p.y*(A.x - B.x) - A.x*B.y + A.y*B.x) / -denominator;
  double s = t1 + t2;

  if (0 <= t1 && t1 <= 1 && 0 <= t2 && t2 <= 1 && s <= 1)
    return true;

  // see http://totologic.blogspot.com/2014/01/accurate-point-in-triangle-test.html

  if (distanceSquarePointToSegment(A, B, p) <= buffer)
    return true;
  if (distanceSquarePointToSegment(B, C, p) <= buffer)
    return true;
  if (distanceSquarePointToSegment(C, A, p) <= buffer)
    return true;

  return false;
}

Cuboid::Cuboid(double xmin, double ymin, double zmin, double xmax, double ymax, double zmax) : Shape(xmin, xmax, ymin, ymax)
{
  A.x = xmin;
  A.y = ymin;
  A.z = zmin;
  B.x = xmax;
  B.y = ymax;
  B.z = zmax;
}

bool Cuboid::contains(const PointXYZ& p)
{
  return(p.x >= A.x && p.x <= B.x && p.y >= A.y && p.y <= B.y && p.z >= A.z && p.z <= B.z);
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
  return(d <= radius*radius);
}
