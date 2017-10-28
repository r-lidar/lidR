#ifndef POINT_H
#define POINT_H

#include <vector>

struct Point
{
	double x, y;
  int id;

	Point();
	Point(const double, const double);
	Point(const double, const double, const long);
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