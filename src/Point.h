#ifndef POINT_H
#define POINT_H

#include <vector>
#include <math.h>

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
  PointXYZ(double _x, double _y) : x(_x), y(_y), z(0), id(0) {}
  PointXYZ(double _x, double _y, double _z) : x(_x), y(_y), z(_z), id(0) {}
  PointXYZ(double _x, double _y, double _z, int _id) : x(_x), y(_y), z(_z), id(_id) {}
};

struct PointXYZR
{
  double x, y, z, r;
  int id;

  PointXYZR() {}
  PointXYZR(double _x, double _y) : x(_x), y(_y), z(0), id(0), r(0) {}
  PointXYZR(double _x, double _y, double _z, int _id, double _r) : x(_x), y(_y), z(_z), id(_id), r(_r) {}
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

struct ZSortPoint
{
  bool operator()(const PointXYZ* lhs, const PointXYZ* rhs) const { return lhs->z > rhs->z; }
};

struct ZSortPoint_increasing
{
  bool operator()(const PointXYZ* lhs, const PointXYZ* rhs) const { return lhs->z < rhs->z; }
};

struct ZRSortPoint_increasing
{
  bool operator()(const PointXYZR* lhs, const PointXYZR* rhs) const { return lhs->z < rhs->z; }
};

struct RSortPoint
{
  bool operator()(const PointXYZR* lhs, const PointXYZR* rhs) const { return lhs->r < rhs->r; }
};

template<class T>
struct distance_to
{
  distance_to(const T& _p) : p(_p) {}

  bool operator()(const T* lhs, const T* rhs) const
  {
    double dx1 = p.x - rhs->x;
    double dy1 = p.y - rhs->y;
    double d1  = dx1 * dx1 + dy1 * dy1;

    double dx2 = p.x - lhs->x;
    double dy2 = p.y - lhs->y;
    double d2  = dx2 * dx2 + dy2 * dy2;

    return d2 < d1;
  }

private:
  T p;
};


#endif //POINT_H

