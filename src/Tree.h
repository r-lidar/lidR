#ifndef TREE
#define TREE

#include <boost/assign.hpp>
#include <boost/geometry.hpp>
#include <cmath>
#include "Point.h"

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

typedef boost::geometry::model::point<double, 2, boost::geometry::cs::cartesian> point_t;
typedef boost::geometry::model::multi_point<point_t> mpoint_t;
typedef boost::geometry::model::polygon<point_t> polygon;

using namespace arma;
using boost::assign::tuple_list_of;

Rcpp::NumericVector findEllipseParameters(boost::geometry::model::ring<point_t> &points);

template<typename T> class Tree
{
  public:
    Tree();
    Tree( T &pt );
    Tree(const Tree<T> &t);
    ~Tree();

    void calculateNewArea( T &pt );
    double testArea( T &pt, double &area_Pt, boost::geometry::model::ring<point_t> &hull_out );
    double testDist( T &pt );
    void addPoint( T &pt );
    void addPoint( T &pt, double &newArea, boost::geometry::model::ring<point_t> &hull );
    void addPoint_dist( T &pt, double &newDist );
    void updateArea();

    double findZMax();
    double findZMin();
    void getZMax();
    void getZMax( T &pt);

    void editIdResult (std::vector<int> &idResult, int &index);


    //Functions for tree score calculation
    void getSize(int k);
    void getOrientation();
    void getRegularity();
    void getCircularity();
    double getScore( int k );

    //Attributes
  //private:
    int nbPoints;
    double area;
    double diff_area;
    double dist;
    std::vector<T> points;
    T Zmax;
    boost::geometry::model::ring<point_t> pointsCH;
    double scoreS;
    double scoreO;
    double scoreR;
    double scoreC;
    double scoreGlobal;
};

//========================================================================================
//                              CONSTRUCTORS
//========================================================================================

template<typename T> Tree<T>::Tree()
{
  nbPoints = 0;
  area = 0;
  diff_area = 0;
  dist = 0;
  scoreS = 0, scoreO = 0, scoreC = 0, scoreR = 0, scoreGlobal = 0;
}

template<typename T> Tree<T>::Tree( T &pt )
{
  nbPoints = 1;
  area = 0;
  diff_area = 0;
  dist = 0;
  points.push_back(pt);
  scoreS = 0, scoreO = 0, scoreC = 0, scoreR = 0, scoreGlobal = 0;
}

template<typename T> Tree<T>::Tree(const Tree<T> &t)
{
  nbPoints = t.nbPoints;
  points.reserve(t.points.size());
  points.assign(t.points.begin(), t.points.end());
  area = t.area;
  diff_area = t.diff_area;
  dist = t.dist;
  scoreS = 0, scoreO = 0, scoreC = 0, scoreR = 0, scoreGlobal = 0;
}

//========================================================================================
//                              DESTRUCTOR
//========================================================================================
template<typename T> Tree<T>::~Tree() {}

//========================================================================================
//                              CALCULATE NEW AREA
//========================================================================================
//Given a new point pt, this function calculates:
// - new distance between points if there was no point or only one point in the initial tree
// - new triangular area if the initial tree contains two points
// - new area using boost::polygon function if the initial tree contains more than two points (update of associated convex hull)
template<typename T> void Tree<T>::calculateNewArea( T &pt )
{
  if ( nbPoints > 2)
  {
    //Conversion from PointXYZ to point_t from boost library use
    mpoint_t pointsForPoly;
    for ( int i = 0 ; i < points.size() ; i++ )
      boost::geometry::append( pointsForPoly, point_t(points[i].x, points[i].y) );

    //Add of new Point + calculation of associated area
    point_t newPt (pt.x, pt.y);
    boost::geometry::append( pointsForPoly, newPt );

    //Assign boost points to polygon
    polygon poly2D;
    boost::geometry::assign_points(poly2D, pointsForPoly);
    boost::geometry::correct(poly2D);

    //Search for convex hull using previous polygon definition
    boost::geometry::model::ring<point_t> hull;
    boost::geometry::convex_hull(poly2D, hull);

    //Aera value without new point (previously stored)
    double area_noPt = area;
    //Aera value including new point Pt + update of 'aera', 'diff_aera' and 'pointCH' attributes
    double area_Pt = boost::geometry::area( hull );
    //Rcpp::Rcout<< "area_boost=" << area_Pt <<  std::endl;
    diff_area = fabs(area_Pt - area_noPt);
    area = area_Pt;
    pointsCH = hull;
    dist = 0;
  }
  else if ( nbPoints == 1 )   //calculate distance
  {
    dist = sqrt( (points[0].x - pt.x)*(points[0].x - pt.x) + (points[0].y - pt.y)*(points[0].y - pt.y) );
  }
  else if ( nbPoints == 2 )    //calculate aera of triangle
  {
    area = calculateTriangleArea( points[0], points[1], pt );
    //these three points define the new convex hull
    boost::geometry::model::ring<point_t> hull;
    boost::geometry::append( hull, point_t(points[0].x, points[0].y) );
    boost::geometry::append( hull, point_t(points[1].x, points[1].y) );
    boost::geometry::append( hull, point_t(pt.x, pt.y) );
    pointsCH = hull;
    dist = 0;
  }
}

//========================================================================================
//                              TEST AREA
//========================================================================================
//Given a new point pt, this function calculates:
// - area of new triangular area including pt if the initial tree contains two points
//   (--> becomes difference because without pt area = 0)
// - difference between old and new area (including pt) using boost::polygon function
//   if the initial tree contains more than two points (update of associated convex hull)
template<typename T> double Tree<T>::testArea( T &pt, double &area_Pt, boost::geometry::model::ring<point_t> &hull_out )
{
  if ( nbPoints == 2 )    //calculate aera of triangle
  {
    area_Pt = calculateTriangleArea( points[0], points[1], pt );
    double area_noPt = area;

    //these three points define the new convex hull
    boost::geometry::model::ring<point_t> hull;
    boost::geometry::append( hull, point_t(points[0].x, points[0].y) );
    boost::geometry::append( hull, point_t(points[1].x, points[1].y) );
    boost::geometry::append( hull, point_t(pt.x, pt.y) );
    pointsCH = hull;
    hull_out = hull;

    double calculatedDiffArea = fabs(area_Pt - area_noPt);
    return calculatedDiffArea;
  }
  else
  {
    //Conversion from PointXYZ to point_t from boost library use
    mpoint_t pointsForPoly;
    for (unsigned int i = 0 ; i < points.size() ; i++ )
      boost::geometry::append( pointsForPoly, point_t(points[i].x, points[i].y) );

    //Add of new Point + calculation of associated area
    point_t newPt (pt.x, pt.y);
    boost::geometry::append( pointsForPoly, newPt );

    //Assign boost points to polygon
    polygon poly2D;
    boost::geometry::assign_points(poly2D, pointsForPoly);
    boost::geometry::correct(poly2D);

    //Search for convex hull using previous polygon definition
    boost::geometry::model::ring<point_t> hull;
    boost::geometry::convex_hull(poly2D, hull);
    area_Pt = boost::geometry::area( hull );
    hull_out = hull;

    double area_noPt = area;
    double calculatedDiffArea = fabs(area_Pt - area_noPt);
    return calculatedDiffArea;
  }
}

//========================================================================================
//                              TEST DISTANCE
//========================================================================================
//Given a new point pt, this function calculates:
// - distance if only one point in initial tree
// - minimum distance between pt and all points of initial tree if there is more than one point
template<typename T> double Tree<T>::testDist( T &pt )
{
  if ( nbPoints == 1 )
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

//========================================================================================
//                              ADD POINT FUNCTIONS
//========================================================================================

// Functions that insert a new point pt into intial tree and update all its parameters (dist, area, convex hull...)
// The two last functions avoid recalculation of previously calculated area, convex hull or dist during "testArea" function
// (always called before)
template<typename T> void Tree<T>::addPoint( T &pt )
{
  nbPoints++;
  points.push_back(pt);
  calculateNewArea(pt);
}

template<typename T> void Tree<T>::addPoint( T &pt, double &newArea, boost::geometry::model::ring<point_t> &hull )
{
  nbPoints++;
  points.push_back(pt);
  area = newArea;
  pointsCH = hull;
  dist = 0;
}

template<typename T> void Tree<T>::addPoint_dist( T &pt, double &newDist )
{
  nbPoints++;
  points.push_back(pt);
  dist = newDist;
}


//========================================================================================
//                              UPDATE AREA
//========================================================================================

template<typename T> void Tree<T>::updateArea()
{
  if ( nbPoints > 3)
  {
    //Conversion from PointXYZ to point_t from boost library use
    mpoint_t pointsForPoly;
    for (unsigned int i = 0 ; i < points.size() ; i++ )
      boost::geometry::append( pointsForPoly, point_t(points[i].x, points[i].y) );

    //Assign boost points to polygon
    polygon poly2D;
    boost::geometry::assign_points(poly2D, pointsForPoly);
    boost::geometry::correct(poly2D);

    //Search for convex hull using previous polygon definition
    boost::geometry::model::ring<point_t> hull;
    boost::geometry::convex_hull(poly2D, hull);

    double area_Pt = boost::geometry::area( hull );
    //diff_area = fabs(area_Pt - area_noPt);
    area = area_Pt;
    pointsCH = hull;
    dist = 0;
  }
  else if ( nbPoints == 2 )   //calculate distance
  {
    dist = sqrt( (points[0].x - points[1].x)*(points[0].x - points[1].x) + (points[0].y - points[1].y)*(points[0].y - points[1].y) );
  }
  else if ( nbPoints == 3 )    //calculate aera of triangle
  {
    area = calculateTriangleArea( points[0], points[1], points[2] );
    boost::geometry::model::ring<point_t> hull;
    boost::geometry::append( hull, point_t(points[0].x, points[0].y) );
    boost::geometry::append( hull, point_t(points[1].x, points[1].y) );
    boost::geometry::append( hull, point_t(points[2].x, points[2].y) );
    pointsCH = hull;
    dist = 0;
  }
}

//========================================================================================
//                              Z-MIN AND -MAX FUNCTIONS
//========================================================================================

//Function that returns highest Z value in points of tree
template<typename T> double Tree<T>::findZMax()
{
  sort( points.begin(), points.end(), ZSortPointBis<T>() );
  double ZmaxValue = points[0].z;
  return (ZmaxValue);
}

//Function that return lowest Z value in points of tree
template<typename T> double Tree<T>::findZMin()
{
  sort( points.begin(), points.end(), ZSortPointBis_increasing<T>() );
  double Zmin = points[0].z;
  return (Zmin);
}

//Function that stores point coordinates of highest Z value in tree definition
template<typename T> void Tree<T>::getZMax()
{
  sort( points.begin(), points.end(), ZSortPointBis<T>() );
  Zmax = points[0];
}

//Function that returns point coordinates of highest Z value in points of tree
template<typename T> void Tree<T>::getZMax( T &pt )
{
  sort( points.begin(), points.end(), ZSortPointBis<T>() );
  pt = points[0];
}

//========================================================================================
//                              EDIT ID RESULT
//========================================================================================
//Function that stores ID of tree in a reference vector of int --> idResult
template<typename T> void Tree<T>::editIdResult (std::vector<int> &idResult, int &index)
{
  for (int i = 0; i < nbPoints ; i++)
  {
    if ( idResult[points[i].id] == 0)
      idResult[points[i].id] = index;
    /*else
      idResult[points[i].id] = INT16_MAX;      //pour tester si on remplace des IDs d'arbres précédemment trouvés*/
  }
  index++;
}





//========================================================================================
//                              SIZE CRITERION
//========================================================================================

//Function to improve detection and removal of small false positive depending on minimal number of point per tree segment
//trees: number of detected trees
//k: number of nearest neighbours
//D: local density of the 3D points in the horizontal plane within the tree segment (page 101 after Eq.5)
template<typename T> void Tree<T>::getSize(int k)
{
  //Search for Zmax point in tree segment  --> H
  double H = findZMax();     //page 101 after Eq.5
  //Point density calculation --> D
  double D = 0;
  if (area != 0 )
    D = nbPoints / area;
  else
    D = nbPoints / dist;     //TODO:que faire pour la distance????
  //Treshold calculation (page 101 Eq.5)
  double threshold = k * D * log(H);

  //Comparison N and threshold (page 101 Eq.4)
  if (nbPoints > threshold)
    scoreS = 1;
  else
    scoreS = (nbPoints/threshold);
}

//========================================================================================
//                              ORIENTATION CRITERION
//========================================================================================
//Function that evaluates the eccentrictity of M (highest point of tree segment) to the gravity center G
//of the point cloud associated with the tree segment -->page 101
template<typename T> void Tree<T>::getOrientation()
{
  scoreO = 0;

  if ( area != 0 && points.size() > 2 && pointsCH.size() > 2 )
  {
    //searching for highest point of tree segment
    getZMax();
    T M = Zmax;

    //searching for gravity center G of the point cloud
    double dx = 0, dy = 0;
    for (int j = 0 ; j < nbPoints; j++ )
    {
      dx += points[j].x;
      dy += points[j].y;
    }
    double xG = dx/nbPoints;
    double yG = dy/nbPoints;
    T G = M;
    G.x = xG; G.y = yG; //--> TODO:faire la moyenne des coordonees de tous les points ou seulement ceux du convex hull??

    //Calculation of planimetric distance between M and G
    double dist_MG = euclidianDistance2D_inZ( M, G );

    //Calculation of average distance of G to the borders of the convex hull
    T pt (boost::geometry::get<0>(pointsCH[0]), boost::geometry::get<1>(pointsCH[0]));
    double dist_GCH = euclidianDistance2D_inZ( M, pt );
    int ind = 0;
    for (int j = 1; j < pointsCH.size(); j++ )
    {
      ind++;
      pt.x = boost::geometry::get<0>(pointsCH[j]);
      pt.y = boost::geometry::get<1>(pointsCH[j]);
      dist_GCH += euclidianDistance2D_inZ( M, pt );
    }
    dist_GCH /= pointsCH.size();

    //Comparison dist_MG and dist_GCH (page 101 Eq. 7)
    if ( dist_MG <= dist_GCH/2.0 )
    {
      scoreO += (1.0 - 2.0*(dist_MG/dist_GCH));
    }
  }
}

//========================================================================================
//                              REGULARITY CRITERION
//========================================================================================
template<typename T> void Tree<T>::getRegularity()
{
  std::vector<double> planimetricDist_MCH;

  scoreR = 0;
  if ( area != 0 && nbPoints > 2 && pointsCH.size() > 2 )
  {
    //Area value retrieval from trees
    double area_CH = area;

    planimetricDist_MCH.clear();
    //searching for highest point of tree segment
    getZMax();
    T M = Zmax;
    T pt (boost::geometry::get<0>(pointsCH[0]), boost::geometry::get<1>(pointsCH[0]));
    planimetricDist_MCH.push_back( euclidianDistance2D_inZ( M, pt ) );
    for (int j = 1; j < pointsCH.size(); j++ )
    {
      pt.x = boost::geometry::get<0>(pointsCH[j]);
      pt.y = boost::geometry::get<1>(pointsCH[j]);
      planimetricDist_MCH.push_back( euclidianDistance2D_inZ( M, pt ) );
    }
    sort(planimetricDist_MCH.begin(), planimetricDist_MCH.end());
    int index_percentile95 = round(planimetricDist_MCH.size() * 0.95);

    double radius= planimetricDist_MCH[index_percentile95-1];

    scoreR = (area_CH / (PI*radius*radius )); //page 101 Eq. 8
  }


}

//========================================================================================
//                              CIRCULARITY CRITERION
//========================================================================================

//http://nicky.vanforeest.com/misc/fitEllipse/fitEllipse.html
Rcpp::NumericVector findEllipseParameters(boost::geometry::model::ring<point_t> &points)
{

  int nbPoint = points.size();
  dmat data(2,nbPoint);

  dmat center = zeros(1,2);
  for (int i = 0; i < nbPoint ; i++ )
  {
    data(0,i) = boost::geometry::get<0>(points[i]);
    data(1,i) = boost::geometry::get<1>(points[i]);
  }

  unique(data);

  //means of eachcoordinates --> ellipse center
  for (int i = 0; i < data.n_rows ; i++ )
  {
    center(0,0) += data(0,i);
    center(0,1) += data(1,i);
  }
  center(0,0) /= data.n_rows;
  center(0,1) /= data.n_rows;

  //Covariance C of
  dmat C = (data*data.t()) - center.t()*center;

  mat PC = princomp(C);
  mat data_PCA_2d = data.t() * PC;

  double max_x = max(data_PCA_2d.col(0));
  double max_y = max(data_PCA_2d.col(1));

  double min_x = min(data_PCA_2d.col(0));
  double min_y = min(data_PCA_2d.col(1));

  double half_axis_length1 = fabs(max_x - min_x);
  double half_axis_length2 = fabs(max_y - min_y);

  Rcpp::NumericVector L = Rcpp::NumericVector::create(half_axis_length1,half_axis_length2);

  return (L);
}

//Hypothesis --> each tree segment should be an approximate circle
//This function evaluates each tree circularity (ratio between major and minor axes of the smallest 2D projection ellipse) (page 101)
template<typename T> void Tree<T>::getCircularity()
{
  scoreC = 0;

  if ( area != 0 && points.size() > 2 && pointsCH.size() > 2 )
  {
    //calculate major and minor axes (A and B)
    Rcpp::NumericVector E = findEllipseParameters(pointsCH);
    double A = E(0) > E(1)? E(0): E(1);
    double B = E(0) < E(1)? E(0): E(1);
    if (B != 0)
      scoreC += (A/B);              //page 101 Eq.6
  }
  scoreGlobal = (scoreS + scoreO + scoreR + scoreC) / 4.0;   //Faire fonction spécifique éventuellement(plus propre)

}

template<typename T> double Tree<T>::getScore( int k )
{
  //enlever plus tard les 4 scores individuels et ne conserver que le score moyen
  getSize(k);
  getOrientation();
  getRegularity();
  getCircularity();   //scoreGlobal calculé ici --> bof
}


#endif //TREE_H
