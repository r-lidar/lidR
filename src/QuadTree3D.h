#ifndef QT3D_H
#define QT3D_H

#include <vector>
#include "BoundingBox3D.h"
#include "Point.h"
#include <math.h>

template<typename T> class QuadTree3D
{
  public:
    QuadTree3D( const T point, const T rangePoint );
    ~QuadTree3D();
    bool insert(const T& p);
    void sphere_lookup(const T &point, const T &rangePoint, std::vector<T>& res);
    void knn_lookup3D(const T &point, const int k, std::vector<T>& res);
    int count();
    BoundingBox3D<T> bbox();
    std::vector<T> points;
    std::vector<double> tmp;
    QuadTree3D<T>* NW;
    int depth;

  private:
    int MAX_DEPTH;
    double EPSILON;
    double EPSILONSQ;
    //int depth;
    int npoints;
    BoundingBox3D<T> boundary;
    //std::vector<T> points;
    QuadTree3D<T>* NE;
    //QuadTree3D* NW;
    QuadTree3D<T>* SE;
    QuadTree3D<T>* SW;
    QuadTree3D<T>* parent;
    QuadTree3D(const BoundingBox3D<T> boundary, const QuadTree3D* parent);
    void subdivide();
    void range_lookup(const BoundingBox3D<T> &bb, std::vector<T>& res, const int method);
    void getPointsSphere(const BoundingBox3D<T> &bb, std::vector<T>& points, std::vector<T>& res);
    void getPointsCube(const BoundingBox3D<T> &bb, std::vector<T>& points, std::vector<T>& res);
    bool in_sphere(const T& p1, const T& p2, const double r);
    bool in_cube(const BoundingBox3D<T>& bb, const T& p);
};

template<typename T> QuadTree3D<T>* QuadTreeCreate(const std::vector<T> &pts);
static inline double max (double a, double b, double c);
static inline double min (double a, double b, double c);

template<typename T> QuadTree3D<T>* QuadTreeCreate(const std::vector<T> &pts)
{
  int n = pts.size();

  double xmin = pts[0].x;
  double ymin = pts[0].y;
  double zmin = pts[0].z;
  double xmax = pts[0].x;
  double ymax = pts[0].y;
  double zmax = pts[0].z;

  for(int i = 0 ; i < n ; i++)
  {
    if(pts[i].x < xmin)
      xmin = pts[i].x;
    else if(pts[i].x > xmax)
      xmax = pts[i].x;
    if(pts[i].y < ymin)
      ymin = pts[i].y;
    else if(pts[i].y > ymax)
      ymax = pts[i].y;
    if(pts[i].z < zmin)
      zmin = pts[i].z;
    else if(pts[i].z > zmax)
      zmax = pts[i].z;
  }

  double xrange = xmax - xmin;
  double yrange = ymax - ymin;
  double zrange = zmax - zmin;
  double range = xrange > yrange ? xrange/2 : yrange/2;
  //double range2 = range > zrange ? range/2 : zrange/2;  //si utilisation en 3D

  T point ((xmin+xmax)/2, (ymin+ymax)/2, (zmin+zmax)/2);
  T rangePoint (range+0.01, range+0.01, range+0.01);

  QuadTree3D<T> *tree = new QuadTree3D<T>( point, rangePoint);

  for(int i = 0 ; i < n ; i++)
  {
    T p(pts[i].x, pts[i].y, pts[i].z, pts[i].id);
    tree->insert(p);
  }

  return tree;
}

template<typename T> QuadTree3D<T>::QuadTree3D( const T point, const T rangePoint )
{
  MAX_DEPTH = 6;
  EPSILON = 0.001;
  EPSILONSQ = EPSILON*EPSILON;
  npoints = 0;

  boundary = BoundingBox3D<T> (point, rangePoint);

  depth = 1;

  NE = 0;
  NW = 0;
  SE = 0;
  SW = 0;
}

template<typename T> QuadTree3D<T>::~QuadTree3D()
{
  delete NE;
  delete NW;
  delete SE;
  delete SW;
}

template<typename T> bool QuadTree3D<T>::insert(const T& p)
{

  if(!boundary.contains_2D(p))
    return false;

  npoints++;

  if(depth == MAX_DEPTH)
  {
    points.push_back(p);
    return true;
  }

  if(NW == 0)
    subdivide();


  if(NW->insert(p))
    return true;
  if(NE->insert(p))
    return true;
  if(SW->insert(p))
    return true;
  if(SE->insert(p))
    return true;

  return false;
}

template<typename T> void QuadTree3D<T>::sphere_lookup(const T &point, const T &rangePoint, std::vector<T>& res)
{
  tmp.push_back(rangePoint.x);
  range_lookup(BoundingBox3D<T>(point, rangePoint), res, 2);
  return;
}

template<typename T> void QuadTree3D<T>::knn_lookup3D(const T &point, const int k, std::vector<T>& res)
{
  double area = 4 * boundary.half_res.x * boundary.half_res.y ; // Dimension of the Quadtree
  double density = npoints / area;                              // Approx point density

  // Radius of the first circle lookup. Computed based on point density to reduce lookup iterations
  double radius = sqrt((double)k / (density * 3.14));

  std::vector<T> pts;
  T rangePoint;

  // Get at least k point within a sphere
  int n = 0;
  while (n < k)
  {
    pts.clear();
    rangePoint = T (radius, radius, radius);
    sphere_lookup( point, rangePoint, pts );
    n = pts.size();
    radius *= 1.5;
  }

  sort(pts.begin(), pts.end(), EuclidianDistance3DSort<T>(point) );

  res.clear();
  for (int i = 0 ; i < k ; i++)
  res.push_back(pts[i]);

  return;
}

template<typename T> QuadTree3D<T>::QuadTree3D(const BoundingBox3D<T> boundary, const QuadTree3D<T>* parent) : boundary(boundary)
{
  MAX_DEPTH = 6;
  EPSILON = 0.001;
  EPSILONSQ = EPSILON*EPSILON;
  npoints = 0;

  depth = parent->depth + 1;

  NE = 0;
  NW = 0;
  SE = 0;
  SW = 0;
}

template<typename T> void QuadTree3D<T>::subdivide()
{
  double half_res_half = boundary.half_res.x * 0.5;

  T p(half_res_half+EPSILONSQ, half_res_half+EPSILONSQ);
  T pNE(boundary.center.x + half_res_half, boundary.center.y + half_res_half);
  T pNW(boundary.center.x - half_res_half, boundary.center.y + half_res_half);
  T pSE(boundary.center.x + half_res_half, boundary.center.y - half_res_half);
  T pSW(boundary.center.x - half_res_half, boundary.center.y - half_res_half);

  NE = new QuadTree3D<T>(BoundingBox3D<T>(pNE, p), this);
  NW = new QuadTree3D<T>(BoundingBox3D<T>(pNW, p), this);
  SE = new QuadTree3D<T>(BoundingBox3D<T>(pSE, p), this);
  SW = new QuadTree3D<T>(BoundingBox3D<T>(pSW, p), this);

}

template<typename T> void QuadTree3D<T>::range_lookup(const BoundingBox3D<T> &bb, std::vector<T>& res, const int method)
{
  if(!boundary.intersects_2D(bb))
    return;

  if(depth == MAX_DEPTH)
  {
    tmp.push_back(3);
    switch(method)
    {
    case 1: getPointsCube(bb, points, res);
      break;

    case 2: getPointsSphere(bb, points, res);
      break;
    }
  }

  if(NW == 0)
    return;

  NE->range_lookup(bb, res, method);
  NW->range_lookup(bb, res, method);
  SE->range_lookup(bb, res, method);
  SW->range_lookup(bb, res, method);

  return;
}

template<typename T> void QuadTree3D<T>::getPointsSphere(const BoundingBox3D<T> &bb, std::vector<T>& points, std::vector<T>& res)
{
  for(typename std::vector<T>::iterator it = points.begin(); it != points.end(); it++)
  {
    if(in_sphere(bb.center, (*it), bb.half_res.x))
      res.push_back((*it));
  }

  return;
}

template <typename T> void QuadTree3D<T>::getPointsCube(const BoundingBox3D<T> &bb, std::vector<T>& points, std::vector<T>& res)
{
  for(typename std::vector<T>::iterator it = points.begin(); it != points.end(); it++)
  {
    if(in_cube(bb, *it))
      res.push_back((*it));
  }
  return;
}

template<typename T> bool QuadTree3D<T>::in_sphere(const T& p1, const T& p2, const double r)
{
  double A = p1.x - p2.x;
  double B = p1.y - p2.y;
  double C = p1.z - p2.z;
  double d = sqrt(A*A + B*B  + C*C);

  return(d <= r);
}

template <typename T> bool QuadTree3D<T>::in_cube(const BoundingBox3D<T>& bb, const T& p)
{
  double dx = bb.center.x - p.x;
  double dy = bb.center.y - p.y;
  double dz = bb.center.z - p.z;
  dx = dx < 0 ? -dx : dx;
  dy = dy < 0 ? -dy : dy;
  dz = dz < 0 ? -dz : dz;

  return(dx <= bb.half_res.x && dy <= bb.half_res.y && dz <= bb.half_res.z);
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

template <typename T> int QuadTree3D<T>::count()
{
  return npoints;
}


#endif //QT3D_H