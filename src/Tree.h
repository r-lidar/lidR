#ifndef TREE
#define TREE

#include <boost/geometry.hpp>
//#include <boost/geometry/geometries/point_xy.hpp>
#include <cmath>
#include "Point.h"

//typedef boost::geometry::model::point<double, 3, boost::geometry::cs::cartesian> polygonPoint;
typedef boost::geometry::model::point<double, 2, boost::geometry::cs::cartesian> point_t;
typedef boost::geometry::model::multi_point<point_t> mpoint_t;
//typedef boost::geometry::model::d2::point_xy<double> point2D;
typedef boost::geometry::model::polygon<point_t> polygonNew;
//typedef boost::geometry::model::polygon<polygonPoint, false, true> polygonCCW;
//typedef boost::geometry::model::polygon<point2D, false, true> polygon;

template<typename T> class Tree
{
public:
  Tree();
  Tree( T &pt );
  ~Tree();

  void addPoint( T &pt );
  void addPoint( T &pt, double &newArea );
  void addPoint_dist( T &pt, double &newDist );
  void calculateNewArea( T &pt );
  double testArea( T & pt );
  double testDist( T &pt );
  double findZMax();
  double findZMin();

//private:
  int nbPoints;
  double area;
  double dist;
  std::vector<T> points;

};

template<typename T> Tree<T>::Tree()
{
  nbPoints = 0;
  area = 0;
  dist = 0;
}

template<typename T> Tree<T>::~Tree() {}

template<typename T> Tree<T>::Tree( T &pt )
{
  nbPoints = 1;
  area = 0;
  dist = 0;
  points.push_back(pt);
}
/*
struct ZSort_increasing
{
  bool operator()(const polygonPoint lhs, const polygonPoint rhs) const { return lhs.get<2>() < rhs.get<2>(); }
};*/

template<typename T> void Tree<T>::calculateNewArea( T &pt )
{
  if ( nbPoints > 2)
  {
    mpoint_t pointsForPoly;

    for ( int i = 0 ; i < points.size() ; i++ )
      boost::geometry::append( pointsForPoly, point_t(points[i].x, points[i].y) );

    boost::geometry::model::ring<point_t> hull;
    boost::geometry::convex_hull(pointsForPoly, hull);

    double areaWithoutPt = boost::geometry::area( hull );

    point_t newPt ( pt.x, pt.y );
    pointsForPoly.push_back( newPt );   //Ajout du nouveau point
    boost::geometry::model::ring<point_t> hull2;
    boost::geometry::convex_hull(pointsForPoly, hull2);

    double areaWithPt = boost::geometry::area( hull2 );
    area = areaWithPt - areaWithoutPt;
  }
  else if ( nbPoints == 2 )
  {
    dist = sqrt( (points[0].x - pt.x)*(points[0].x - pt.x) + (points[0].y - pt.y)*(points[0].y - pt.y) );
  }
}


template<typename T> void Tree<T>::addPoint( T &pt )
{
  nbPoints++;
  points.push_back(pt);
  calculateNewArea(pt);
}

template<typename T> void Tree<T>::addPoint( T &pt, double &newArea )
{
  nbPoints++;
  points.push_back(pt);
  area = newArea;
}

template<typename T> void Tree<T>::addPoint_dist( T &pt, double &newDist )
{
  nbPoints++;
  points.push_back(pt);
  dist = newDist;
}

template<typename T> double Tree<T>::testArea( T &pt )
{
  mpoint_t pointsForPoly;

  for ( int i = 0 ; i < points.size() ; i++ )
    boost::geometry::append( pointsForPoly, point_t(points[i].x, points[i].y) );

  boost::geometry::model::ring<point_t> hull;
  boost::geometry::convex_hull(pointsForPoly, hull);

  double areaWithoutPt = boost::geometry::area( hull );

  point_t newPt ( pt.x, pt.y );
  pointsForPoly.push_back( newPt );   //Ajout du nouveau point
  boost::geometry::model::ring<point_t> hull2;
  boost::geometry::convex_hull(pointsForPoly, hull2);

  double areaWithPt = boost::geometry::area( hull2 );
  double calculatedArea = areaWithPt - areaWithoutPt;
  return calculatedArea;
}

template<typename T> double Tree<T>::testDist( T &pt )
{
  if ( points.size() == 1 )
  {
    double calculatedDist = sqrt( (points[0].x - pt.x)*(points[0].x - pt.x) + (points[0].y - pt.y)*(points[0].y - pt.y) );
    return calculatedDist;
  }
  else
  {
    //If more than one point in points --> Before distance calculation, search for closest point to pt
    double valRef = 0, val = 0;
    int keep = 0;
    valRef = euclidianDistance2D_inZ( pt, points[0] );
    for ( int i = 0; i < points.size(); i++ )
    {
      val = euclidianDistance2D_inZ( pt, points[i] );
      if ( valRef > val )
      {
        valRef = val;
        keep = i;
      }
    }
    double calculatedDist = sqrt( (points[keep].x - pt.x)*(points[keep].x - pt.x) + (points[keep].y - pt.y)*(points[keep].y - pt.y) );
    return calculatedDist;
  }
}


template<typename T> double Tree<T>::findZMax()
{
  sort( points.begin(), points.end(), ZSortPointBis<T>() );
  double Zmax = points[0].z;
  return (Zmax);
}

template<typename T> double Tree<T>::findZMin()
{
  sort( points.begin(), points.end(), ZSortPointBis_increasing<T>() );
  double Zmin = points[0].z;
  return (Zmin);
}


#endif //TREE_H
