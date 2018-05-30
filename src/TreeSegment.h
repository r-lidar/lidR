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

class TreeSegment
{
  public:
    TreeSegment();
    TreeSegment( PointXYZ &pt );
    TreeSegment(const TreeSegment &t);
    ~TreeSegment();

    void getZMax();
    void getZMax(PointXYZ &pt);
    void calculateArea();
    void addPoint( PointXYZ &pt );
    void updateArea();

    double testArea(PointXYZ &pt);
    double testDist(PointXYZ &pt);
    double findZMax();
    double findZMin();

    polygon get_convex_hull();
    point_t get_apex();

    void editIdResult (std::vector<int> &idResult, int &index);

    // Functions for tree score calculation
    void getSize(int k);
    void getOrientation();
    void getRegularity();
    void getCircularity();
    double getScore( int k );

  public:
    int nbPoints;
    double area;
    double diff_area;
    double dist;
    double scoreS;
    double scoreO;
    double scoreR;
    double scoreC;
    double scoreGlobal;
    double zmax;
    double zmin;
    std::vector<PointXYZ> points;
    PointXYZ Zmax;
    point_t apex;
    polygon convex_hull;
    boost::geometry::model::ring<point_t> pointsCH;
};

#endif //TREE_H
