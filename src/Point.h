#ifndef POINT_H
#define POINT_H

#include <cmath>
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
  T4 id;
  T5 r;

  Point4D() {}
  Point4D(const T1 _x, const T2 _y) : x(_x), y(_y), z(0), id(0), r(0) {}
  Point4D(const T1 _x, const T2 _y, const T3 _z) : x(_x), y(_y), z(_z), id(0), r(0) {}
  Point4D(const T1 _x, const T2 _y, const T3 _z, const T4 _id) : x(_x), y(_y), z(_z), id(_id), r(0) {}
  Point4D(const T1 _x, const T2 _y, const T3 _z, const T4 _id, const T5 _r) : x(_x), y(_y), z(_z), id(_id), r(_r) {}
};

typedef Point2D<double, double, int> Point;
typedef Point3D<double, double, double, int> PointXYZ;
typedef Point4D<double, double, double, int, double> PointXYZR;


/*
 * Used to sort points with std::sort
 */

// Sort point by Z attributes
template<typename T> struct ZSort
{
  bool operator()(const T* lhs, const T* rhs) const { return lhs->z > rhs->z; }
  bool operator()(const T lhs, const T rhs) const { return lhs.z > rhs.z; }
};

// Sort points by Z attributes
template<typename T> struct RSort
{
  bool operator()(const T* lhs, const T* rhs) const { return lhs->r < rhs->r; }
  bool operator()(const T lhs, const T rhs) const { return lhs.r < rhs.r ; }
};

// Sort points according to their distance to a reference point (2D)
template<class T> struct DSort2D
{
  DSort2D(const T& _p) : p(_p) {}

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

  bool operator()(const T lhs, const T rhs) const
  {
    double dx1 = p.x - rhs.x;
    double dy1 = p.y - rhs.y;
    double d1  = dx1 * dx1 + dy1 * dy1;

    double dx2 = p.x - lhs.x;
    double dy2 = p.y - lhs.y;
    double d2  = dx2 * dx2 + dy2 * dy2;

    return d2 < d1;
  }

  private:
    T p;
};

// Used in spatial index for knn
template<class T> struct DSort3D
{
  DSort3D(const T& _p) : p(_p) {}

  bool operator()(const T lhs, const T rhs) const
  {
    double dx1 = p.x - rhs.x;
    double dy1 = p.y - rhs.y;
    double dz1 = p.z - rhs.z;
    double d1  = dx1 * dx1 + dy1 * dy1 + dz1 * dz1;

    double dx2 = p.x - lhs.x;
    double dy2 = p.y - lhs.y;
    double dz2 = p.z - lhs.z;
    double d2  = dx2 * dx2 + dy2 * dy2 + dz2 * dz2;

    return d2 < d1;
  }

  bool operator()(const T* lhs, const T* rhs) const
  {
    double dx1 = p.x - rhs->x;
    double dy1 = p.y - rhs->y;
    double dz1 = p.z - rhs->z;
    double d1  = dx1 * dx1 + dy1 * dy1 + dz1 * dz1;

    double dx2 = p.x - lhs->x;
    double dy2 = p.y - lhs->y;
    double dz2 = p.z - lhs->z;
    double d2  = dx2 * dx2 + dy2 * dy2 + dz2 * dz2;

    return d2 < d1;
  }

  private:
    T p;
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

static inline double distanceSquarePointToSegment(const Point& p1, const Point& p2, const Point& p)
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

#endif //POINT_H

