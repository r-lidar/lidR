#ifndef SHAPES_H
#define SHAPES_H

#include "Point.h"
#include "BoundingBox.h"

struct Shape
{
  Shape();
  Shape(double xmin, double xmax, double ymin, double ymax);
  Shape(double xcenter, double ycenter, double radius);
  bool contains(const Point&);
  BoundingBox bbox;
};

struct Rectangle: public Shape
{
  Rectangle(double xmin, double xmax, double ymin, double ymax);
  bool contains(const Point&);
  Point A;
  Point B;
};

struct OrientedRectangle: public Shape
{
  OrientedRectangle(double xmin, double xmax, double ymin, double ymax, double angle);
  bool contains(const Point&);
  Point A;
  Point B;
  Point C;
  Point D;
};

struct Circle: public Shape
{
  Circle(double xcenter, double ycenter, double radius);
  bool contains(const Point&);
  Point center;
  double radius;
};

struct Triangle: public Shape
{
  Triangle(Point& A, Point& B, Point& C);
  bool contains(const Point&);
  Point A;
  Point B;
  Point C;
};

struct Sphere: public Shape
{
  Sphere(double xcenter, double ycenter, double zcenter, double radius);
  bool contains(const PointXYZ&);
  PointXYZ center;
  double radius;
};

#endif //SHAPES_H
