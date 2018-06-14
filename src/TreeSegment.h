#ifndef TREESEGMENT_H
#define TREESEGMENT_H

// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppArmadillo)]]

#include <boost/geometry.hpp>
#include <cmath>
#include "Point.h"
#include <RcppArmadillo.h>

typedef boost::geometry::model::point<double, 2, boost::geometry::cs::cartesian> point_t;
typedef boost::geometry::model::polygon<point_t> polygon;

namespace ptrees
{
  class TreeSegment
  {
    public:
      TreeSegment();
      TreeSegment(int);
      TreeSegment(PointXYZ &, int);
      ~TreeSegment();

    public:
      double get_zmax();
      double get_zmin();
      double get_score();

    private:
      bool add_point(PointXYZ &, double);
      void compute_area();
      void compute_all_score();

      double compute_area_increment(PointXYZ &);
      double compute_distance_to(PointXYZ &);

      point_t get_apex();

      TreeSegment merge(TreeSegment&);

      void compute_size_score();
      void compute_orientation_score();
      void compute_circularity_score();
      void compute_regularity_score();
      std::pair<double, double> findEllipseParameters(polygon &);

    private:
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

      friend class TreeSegmentManager;
  };
} //end Vega

#endif //TREESEGMENT_H
