#ifndef POINT_H
#define POINT_H

#include <vector>

struct Point
{
	double x, y;
  int id;

  Point() {}
  Point(const double _x, const double _y) : x(_x), y(_y), id(0) {}
  Point(double _x, double _y, int _id) : x(_x), y(_y), id(_id) {}
};

struct PointXYZ
{
  double x, y, z;
  int id;

  PointXYZ() {}
  PointXYZ(double _x, double _y, double _z, int _id) : x(_x), y(_y), z(_z), id(_id) {}
};

template<class T>
struct Pixel
{
  int i, j;
  T val;

  Pixel() {}
  Pixel(const int _i, const int _j, const T _val) : i(_i), j(_j), val(_val) {}
};

template<typename T> std::vector<double> sqdistance(std::vector<T*>& pts, T& u);
template<typename T> std::vector<double> sqdistance(std::vector<T*>& pts, T& u)
{
  int n = pts.size();
  std::vector<double> y(n);
  std::vector<double>::iterator iy, endy;
  typename std::vector<T*>::iterator ip, endp;

  for(ip = pts.begin(), iy = y.begin(), endp = pts.end(), endy = y.end() ; iy < endy && ip < endp ; ++iy, ++ip)
  {
    double dx = (*ip)->x - u.x;
    double dy = (*ip)->y - u.y;
    *iy = dx * dx + dy * dy;
  }

  return y;
}

template<typename T> double dist(const T& lhs, const T& rhs);
template<typename T> double dist(const T& lhs, const T& rhs)
{
  double dx = lhs.x - rhs.x;
  double dy = lhs.y - rhs.y;
  return dx * dx + dy * dy;
}

struct ZSortPoint
{
  bool operator()(const PointXYZ* lhs, const PointXYZ* rhs) const { return lhs->z > rhs->z; }
};


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

