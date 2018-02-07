#ifndef POINT_H
#define POINT_H

#include <vector>

struct Point
{
	double x, y;
  int id;

  Point(){}
  Point(const double _x, const double _y) : x(_x), y(_y), id(0) {}
  Point(const double _x, const double _y, const int _id) : x(_x), y(_y), id(_id) {}
};

template<class T>
struct Pixel
{
  int i, j;
  T val;

  Pixel() {}
  Pixel(const int _i, const int _j, const T _val) : i(_i), j(_j), val(_val) {}
};

std::vector<double> sqdistance(std::vector<Point*>& pts, Point& u);

double dist(const Point& lhs, const Point& rhs);

struct DistanceFunc
{
  DistanceFunc(const Point& _p) : p(_p) {}

  bool operator()(const Point* lhs, const Point* rhs) const
  {
    return dist(p, *lhs) < dist(p, *rhs);
  }

private:
  Point p;
};

#endif //POINT_H

