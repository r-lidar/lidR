#ifndef TREE
#define TREE

// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppArmadillo)]]

#include <boost/geometry.hpp>
#include <cmath>
#include "Point.h"
#include <RcppArmadillo.h>

typedef boost::geometry::model::point<double, 2, boost::geometry::cs::cartesian> point_t;
typedef boost::geometry::model::polygon<point_t> polygon;

class TreeSegment
{
  public:
public:
    TreeSegment();
    TreeSegment(int);
    TreeSegment(PointXYZ &pt, int);
    ~TreeSegment();

    void add_point(PointXYZ &pt);
    void compute_area();
    void compute_all_score();

    static void apply2DFilter(std::vector<PointXYZ> &, std::vector<PointXYZ> &);

    double compute_area_increment(PointXYZ &pt);
    double compute_distance_to(PointXYZ &pt);

    point_t get_apex();

    TreeSegment merge(TreeSegment&);

  private:
    void compute_size_score();
    void compute_orientation_score();
    void compute_circularity_score();
    void compute_regularity_score();
    std::pair<double, double> findEllipseParameters(polygon &);

  public:
    int nbPoints;
    int k;
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
