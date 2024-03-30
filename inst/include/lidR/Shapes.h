#ifndef SHAPES_H
#define SHAPES_H

#include "Point.h"

namespace lidR
{

#define EPSILON 1e-8
#define XYINF 99999999999;
#define ZINF 2147483640;
#define MAX(a,b,c) ((a>b)?((a>c)?a:c):((b>c)?b:c));
#define MIN(a,b,c) ((a>b)?((b>c)?c:b):((a>c)?c:a));

struct Shape
{
  double xmin;
  double xmax;
  double ymin;
  double ymax;
  double zmin;
  double zmax;

  Shape();
  Shape(double xmin, double xmax, double ymin, double ymax);
  Shape(double xmin, double xmax, double ymin, double ymax, double zmin, double zmax);
  template<typename T> bool contains(const T&);
};

inline Shape::Shape()
{
  this->xmin = -XYINF;
  this->xmax = XYINF;
  this->ymin = -XYINF;
  this->ymax = XYINF;
  this->zmin = -ZINF;
  this->zmax = ZINF;
}

inline Shape::Shape(double xmin, double xmax, double ymin, double ymax)
{
  this->xmin = xmin;
  this->xmax = xmax;
  this->ymin = ymin;
  this->ymax = ymax;
  this->zmin = -ZINF;
  this->zmax = ZINF;
}

inline Shape::Shape(double xmin, double xmax, double ymin, double ymax, double zmin, double zmax)
{
  this->xmin = xmin;
  this->xmax = xmax;
  this->ymin = ymin;
  this->ymax = ymax;
  this->zmin = zmin;
  this->zmax = zmax;
}

template<typename T>
bool Shape::contains(const T& p)
{
  return(p.x >= xmin - EPSILON &&
         p.x <= xmax + EPSILON &&
         p.y >= ymin - EPSILON &&
         p.y <= ymax + EPSILON &&
         p.z >= zmin - EPSILON &&
         p.z <= zmax + EPSILON);
}

struct Rectangle: public Shape
{
  Rectangle(double xmin, double xmax, double ymin, double ymax);
  template<typename T> bool contains(const T&);
};

inline Rectangle::Rectangle(double xmin, double xmax, double ymin, double ymax) : Shape(xmin, xmax, ymin, ymax) { }

template<typename T>
bool Rectangle::contains(const T& p)
{
  return(p.x >= xmin - EPSILON &&
         p.x <= xmax + EPSILON &&
         p.y >= ymin - EPSILON &&
         p.y <= ymax + EPSILON);
}

struct OrientedRectangle: public Shape
{
  OrientedRectangle(double xmin, double xmax, double ymin, double ymax, double angle);
  template<typename T> bool contains(const T&);
  Point A;
  Point B;
  Point C;
  Point D;
};

inline OrientedRectangle::OrientedRectangle(double xmin, double xmax, double ymin, double ymax, double angle) : Shape()
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
  this->xmin = *std::min_element(std::begin(x), std::end(x));
  this->ymin = *std::min_element(std::begin(y), std::end(y));
  this->xmax = *std::max_element(std::begin(x), std::end(x));
  this->ymax = *std::max_element(std::begin(y), std::end(y));

  //printf("(%lf %lf) (%lf %lf) (%lf %lf) (%lf %lf)", A.x, A.y, B.x, B.y, C.x, C.y, D.x, D.y);
}

template<typename T>
bool OrientedRectangle::contains(const T& p)
{
  if ( (B.x - A.x) * (p.y - A.y) - (p.x - A.x) * (B.y - A.y) < EPSILON)
    return false;

  if ( (C.x - B.x) * (p.y - B.y) - (p.x - B.x) * (C.y - B.y) < EPSILON)
    return false;

  if ( (D.x - C.x) * (p.y - C.y) - (p.x - C.x) * (D.y - C.y) < EPSILON)
    return false;

  if ( (A.x - D.x) * (p.y - D.y) - (p.x - D.x) * (A.y - D.y) < EPSILON)
    return false;

  return true;
}

struct Circle: public Shape
{
  Circle(double xcenter, double ycenter, double radius);
  template<typename T> bool contains(const T&);
  Point center;
  double radius;
};

inline Circle::Circle(double xcenter, double ycenter, double radius) : Shape(xcenter-radius, xcenter+radius, ycenter-radius, ycenter+radius)
{
  center.x = xcenter;
  center.y = ycenter;
  this->radius = radius;
}

template<typename T>
bool Circle::contains(const T& p)
{
  double A = center.x - p.x;
  double B = center.y - p.y;
  double d = A*A + B*B;
  return d <= radius*radius + EPSILON;
}

struct Triangle: public Shape
{
  Triangle(Point& A, Point& B, Point& C);
  template<typename T> bool contains(const T&);
  template<typename T> double distanceSquarePointToSegment(const Point& p1, const Point& p2, const T& p);
  Point A;
  Point B;
  Point C;
};

inline Triangle::Triangle(Point& A, Point& B, Point& C)
{
  xmin = MIN(A.x, B.x, C.x);
  ymin = MIN(A.y, B.y, C.y);
  xmax = MAX(A.x, B.x, C.x);
  ymax = MAX(A.y, B.y, C.y);

  this->A = A;
  this->B = B;
  this->C = C;
}

template<typename T>
bool Triangle::contains(const T& P)
{
  if (P.x < xmin - EPSILON || P.x > xmax + EPSILON || P.y < ymin - EPSILON || P.y > ymax + EPSILON)
    return false;

  // move to (0,0) to gain arithmetic precision
  double x_offset = xmin;
  double y_offset = ymin;
  PointXY a(A.x - x_offset, A.y - y_offset);
  PointXY b(B.x - x_offset, B.y - y_offset);
  PointXY c(C.x - x_offset, C.y - y_offset);
  PointXY p(P.x - x_offset, P.y - y_offset);

  double denominator = (a.x*(b.y - c.y) + a.y*(c.x - b.x) + b.x*c.y - b.y*c.x);
  double t1 = (p.x*(c.y - a.y) + p.y*(a.x - c.x) - a.x*c.y + a.y*c.x) / denominator;
  double t2 = (p.x*(b.y - a.y) + p.y*(a.x - b.x) - a.x*b.y + a.y*b.x) / -denominator;
  double s = t1 + t2;

  if (0 <= t1 && t1 <= 1 && 0 <= t2 && t2 <= 1 && s <= 1)
    return true;

  // see http://totologic.blogspot.com/2014/01/accurate-point-in-triangle-test.html
  if (distanceSquarePointToSegment(a, b, p) <= EPSILON) return true;
  if (distanceSquarePointToSegment(b, c, p) <= EPSILON) return true;
  if (distanceSquarePointToSegment(c, a, p) <= EPSILON) return true;

  return false;
}

template<typename T>
double Triangle::distanceSquarePointToSegment(const Point& p1, const Point& p2, const T& p)
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

struct Sphere: public Shape
{
  Sphere(double xcenter, double ycenter, double zcenter, double radius);
  template<typename T> bool contains(const T&);
  PointXYZ center;
  double radius;
};

inline Sphere::Sphere(double xcenter, double ycenter, double zcenter, double radius) : Shape(xcenter-radius, xcenter+radius, ycenter-radius, ycenter+radius, zcenter-radius, zcenter+radius)
{
  center.x = xcenter;
  center.y = ycenter;
  center.z = zcenter;
  this->radius = radius;
}

template<typename T>
bool Sphere::contains(const T& p)
{
  double A = center.x - p.x;
  double B = center.y - p.y;
  double C = center.z - p.z;
  double d = A*A + B*B + C*C;
  return(d <= radius*radius + EPSILON);
}

}

#endif //SHAPES_H
