#ifndef POINT_H
#define POINT_H

#include <boost/geometry.hpp>
#include <math.h>
#include <vector>

template<typename T1, typename T2, typename T3> struct Point2D
{
  T1 x;
  T2 y;
  T3 id;

  Point2D() {}
  Point2D(const T1 _x, const T2 _y) : x(_x), y(_y), id(0) {}
  Point2D(const T1 _x, const T2 _y, const T3 _id) : x(_x), y(_y), id(_id) {}
};

template<typename T1, typename T2, typename T3, typename T4> struct Point3D
{
  T1 x;
  T2 y;
  T3 z;
  T4 id;

  Point3D() {}
  Point3D(const T1 _x, const T2 _y) : x(_x), y(_y), z(0), id(0) {}
  Point3D(const T1 _x, const T2 _y, const T3 _z) : x(_x), y(_y), z(_z), id(0) {}
  Point3D(const T1 _x, const T2 _y, const T3 _z, const T4 _id) : x(_x), y(_y), z(_z), id(_id) {}
};


template<typename T1, typename T2, typename T3, typename T4, typename T5>struct Point4D
{
  T1 x;
  T2 y;
  T3 z;
  T4 r;
  T5 id;

  Point4D() {}
  Point4D(const T1 _x, const T2 _y) : x(_x), y(_y), z(0), id(0), r(0) {}
  Point4D(const T1 _x, const T2 _y, const T3 _z) : x(_x), y(_y), z(_z), id(0), r(0) {}
  Point4D(const T1 _x, const T2 _y, const T3 _z, const T5 _id) : x(_x), y(_y), z(_z), id(_id), r(0) {}
  Point4D(const T1 _x, const T2 _y, const T3 _z, const T5 _id, const T4 _r) : x(_x), y(_y), z(_z), id(_id), r(_r) {}
};

template<class T> struct Pixel
{
  int i, j;
  T val;
  Pixel() {}
  Pixel(const int _i, const int _j, const T _val) : i(_i), j(_j), val(_val) {}
};

typedef Point2D<double, double, int> Point;
typedef Point3D<double, double, double, int> PointXYZ;
typedef Point4D<double, double, double, double, int> PointXYZR;

// Used to sort points std::sort

template<typename T> struct ZSortPoint
{
  bool operator()(const T* lhs, const T* rhs) const { return lhs->z > rhs->z; }
  bool operator()(const T lhs, const T rhs) const { return lhs.z > rhs.z; }
};

template<typename T> struct RSortPoint
{
  bool operator()(const T* lhs, const T* rhs) const { return lhs->r < rhs->r; }
  bool operator()(const T lhs, const T rhs) const { return lhs.r < rhs.r ; }
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

template <class T>
struct EuclidianDistance {
  double operator() (const T a, const T b) const
  {
    double dx = b->x - a->x;
    double dy = b->y - a->y;
    return sqrt(dx*dx + dy*dy);
  }
};

// Used in QuadTree3D
template<class T> struct EuclidianDistance3DSort
{
  EuclidianDistance3DSort(const T& _p) : p(_p) {}

  bool operator()(const T lhs, const T rhs) const
  {
    double dx1 = p.x - rhs.x;
    double dy1 = p.y - rhs.y;
    double dz1 = p.z - rhs.z;
    double d1  = sqrt(dx1 * dx1 + dy1 * dy1 + dz1 * dz1);

    double dx2 = p.x - lhs.x;
    double dy2 = p.y - lhs.y;
    double dz2 = p.z - lhs.z;
    double d2  = sqrt(dx2 * dx2 + dy2 * dy2 + dz2 * dz2);

    return d2 < d1;
  }

private:
  T p;
};

// Used in QuadTree
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


// used in TreeSegmentManager
template <class T> double euclidianDistance2D_inZ( T &refPoint, T &point )
{
  double dx = refPoint.x - point.x;
  double dy = refPoint.y - point.y;
  return sqrt(dx*dx + dy*dy);
}

#endif //POINT_H

