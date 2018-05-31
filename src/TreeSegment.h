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
    TreeSegment(PointXYZ &pt);
    ~TreeSegment();

    void addPoint(PointXYZ &pt);
    void calculateArea();
    void compute_all_score(int);

    static void apply2DFilter(std::vector<PointXYZ> &, std::vector<PointXYZ> &);

    double testArea(PointXYZ &pt);
    double testDist(PointXYZ &pt);

    point_t get_apex();

    TreeSegment merge(TreeSegment&, int);

  private:
    void compute_size_score(int);
    void compute_orientation_score();
    void compute_circularity_score();
    void compute_regularity_score();

  public:
    int nbPoints;
    double area;
    double scoreS;
    double scoreO;
    double scoreR;
    double scoreC;
    double scoreGlobal;
    PointXYZ Zmax;
    PointXYZ Zmin;
    polygon convex_hull;
};

#endif //TREE_H
