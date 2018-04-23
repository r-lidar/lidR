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
  PointXYZ(double _x, double _y) : x(_x), y(_y), z(0), id(0) {}
  PointXYZ(double _x, double _y, double _z) : x(_x), y(_y), z(_z), id(0) {}
  PointXYZ(double _x, double _y, double _z, int _id) : x(_x), y(_y), z(_z), id(_id) {}
};

struct PointRTZ
{
  double r, t, z;
  int id;

  PointRTZ() {}
  PointRTZ(double _r, double _t) : r(_r), t(_t), z(0), id(0) {}
  PointRTZ(double _r, double _t, double _z) : r(_r), t(_t), z(_z), id(0) {}
  PointRTZ(double _r, double _t, double _z, int _id) : r(_r), t(_t), z(_z), id(_id) {}
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

template <class T> struct EuclidianDistance : std::binary_function <T,T,T> {
  double operator() (const T a, const T b) const
  {
    return std::sqrt(pow(b->x - a->x, 2) + pow(b->y - a->y, 2));
  }
};

template <class T> struct SlopeInCylindricalReferenceSystem : std::binary_function <T,T,T> {
  double operator() (const T a, const T b) const
  {
    return (b->z - a->z) / (b->r - a->r);
  }
};

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


template<typename T1, typename T2>
void cart2pol_vec( const std::vector<T1*> &points, const T2* &center, std::vector<PointRTZ*> &result )
{
  double x = 0, y = 0, z = 0, r = 0, t = 0;
  int ind = 0;
  for ( int i = 0; i < points.size(); i++ )
  {
    x = points[i]->x - center->x;
    y = points[i]->y - center->y;
    r = sqrt(pow(x,2) + pow(y,2));
    t = atan(y/x);
    z = points[i]->z;
    ind = points[i]->id;
    result[i] = new PointRTZ( r, t, z, ind );
  }
}

//----------------------------------------------------------------------------------------//
template<typename T1, typename T2>
void pol2cart_vec( const std::vector<T1*> &points, const T2* &center, std::vector<PointXYZ*> &result )
{
  int Xsign[6] = {1, 1, -1, -1, 1, 1};
  double x = 0, y = 0, z = 0;
  int ind = 0;
  for ( int i = 0; i < points.size(); i++ )
  {
    x = Xsign[i] * points[i]->r * cos( points[i]->t ) + center->x;
    y = Xsign[i] * points[i]->r * sin( points[i]->t ) + center->y;
    z = points[i]->z;
    ind = points[i]->id;
    result[i] = new PointXYZ( x, y, z, ind );
  }
}


#endif //POINT_H

