#include <Rcpp.h>
using namespace Rcpp;

#include <boost/geometry.hpp>
#include <boost/geometry/geometries/polygon.hpp>
#include <boost/geometry/geometries/adapted/boost_tuple.hpp>

#include <boost/foreach.hpp>

BOOST_GEOMETRY_REGISTER_BOOST_TUPLE_CS(cs::cartesian)

typedef boost::tuple<double, double> point;
typedef boost::geometry::model::polygon<point, true, true> polygon;

namespace Rcpp
{
  // as<>() converter from R to Boost.Geometry's polygon type
  template <> polygon as(SEXP pointsMatrixSEXP)
  {
    // the coordinates are the rows of the (n x 2) matrix
    NumericMatrix pointsMatrix(pointsMatrixSEXP);
    polygon poly;

    for (int i = 0; i < pointsMatrix.nrow(); ++i)
    {
      double x = pointsMatrix(i,0);
      double y = pointsMatrix(i,1);
      point p(x,y);
      poly.outer().push_back(p);
    }
    return (poly);
  }

  // wrap() converter from Boost.Geometry's polygon to an R(cpp) matrix
  // The Rcpp NumericMatrix can be converted to/from a SEXP
  template <> SEXP wrap(const polygon& poly)
  {
    const std::vector<point>& points = poly.outer();
    NumericMatrix rmat(points.size(), 2);

    for (unsigned int i = 0; i < points.size(); ++i)
    {
      const point& p = points[i];
      rmat(i,0) = p.get<0>();
      rmat(i,1) = p.get<1>();
    }
    return Rcpp::wrap(rmat);
  }
}

// [[Rcpp::export]]
NumericMatrix polygon_intersection(SEXP pointsMatrix1, SEXP pointsMatrix2)
{
  polygon poly1 = as<polygon>(pointsMatrix1);
  polygon poly2 = as<polygon>(pointsMatrix2);

  boost::geometry::correct(poly1);
  boost::geometry::correct(poly2);

  std::deque<polygon> output;
  boost::geometry::intersection(poly2, poly1, output);

  return wrap(output[0]);
}