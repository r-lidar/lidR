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
//typedef boost::geometry::model::polygon<point2D> polygonNew;
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

    point_t newPt ( pt.x, pt.y );

    boost::geometry::model::ring<point_t> hull;
    boost::geometry::convex_hull(pointsForPoly, hull);

    area = boost::geometry::area( hull );
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

//template<typename T> double Tree<T>::testArea( T &pt )
template<typename T> double Tree<T>::testArea( T &pt )
{
  //Copie du vecteur de points pour le transformer en boostPoint et y intégrer plus tard les valeurs d'angle ( Z non utile pour la définition de polygon en 2D)
  mpoint_t pointsForPoly;
  //polygonPoint zero = polygonPoint(0,0,0);
  //pointsForPoly.assign( points.size(), zero);

  for ( int i = 0 ; i < points.size() ; i++ )
  {
    boost::geometry::append( pointsForPoly, point_t(points[i].x, points[i].y) );
    //pointsForPoly[i] = polygonPoint(points[i].x, points[i].y, points[i].z);
  }

  point_t newPt ( pt.x, pt.y );
  //polygonPoint newPt (pt.x, pt.y, pt.z);
  //pointsForPoly.push_back( newPt );   //Ajout du nouveau point

  /*
  //Pour l'instant les points ne sont pas triés par angle. Le Z contient les valeurs de hauteur.
  //calcul barycentre pour avoir point de référence pour le calcul des angles
  double dx = pointsForPoly[0].get<0>();
  double dy = pointsForPoly[0].get<1>();
  for (int i = 1 ; i < points.size()+1 ; i++ )
  {
    dx = (dx + pointsForPoly[i].get<0>()) / 2;
    dy = (dy + pointsForPoly[i].get<1>()) / 2;
  }
  T B (dx, dy);

    // Pour etre dans l ordre trigo, la valeur des angles est calculé par rapport au barycentre et stocké dans Z
  for (int i = 0 ; i < points.size()+1 ; i++ )
  {
    //In this case Z stores the angle value and not the height!!
    pointsForPoly[i].set<2>( atan2(pointsForPoly[i].get<1>() - B.y, pointsForPoly[i].get<0>() - B.x) );
  }

  std::sort( pointsForPoly.begin(), pointsForPoly.end(), ZSort_increasing() );

  polygonCCW poly;

 //First point is added again at the end to close polygon
 pointsForPoly.push_back( polygonPoint(pointsForPoly[0]) );

 //Assign points to polygons
 boost::geometry::assign_points(poly, pointsForPoly);

 std::vector<point2D> points2DForPoly(points.size());
 for (int i = 0 ; i < points.size()+1 ; i++ )
 {
 //In this case Z stores the angle value and not the height!!
 points2DForPoly[i] = point2D( pointsForPoly[i].get<0>(), pointsForPoly[i].get<1>() );
 }

 //Assign points to polygons for 2D
 polygonNew poly2D;
 boost::geometry::assign_points(poly2D, points2DForPoly);
*/
  /*
  polygonNew poly2D;
  boost::geometry::assign_points(poly2D, pointsForPoly);
  boost::geometry::correct(poly2D);*/

 boost::geometry::model::ring<point_t> hull;
 boost::geometry::convex_hull(pointsForPoly, hull);

 double area = boost::geometry::area( hull );
  return area;

}

template<typename T> double Tree<T>::testDist( T &pt )
{
  if ( points.size() == 1 )
  {
    double dist = sqrt( (points[0].x - pt.x)*(points[0].x - pt.x) + (points[0].y - pt.y)*(points[0].y - pt.y) );
    return dist;
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
    double dist = sqrt( (points[keep].x - pt.x)*(points[keep].x - pt.x) + (points[keep].y - pt.y)*(points[keep].y - pt.y) );
    return dist;
  }
}


template<typename T> double Tree<T>::findZMax()
{
  sort( points.begin(), points.end(), ZSortPointBis<T>() );
  double Zmax = points[0].z;
  return (Zmax);
}


#endif //TREE_H
