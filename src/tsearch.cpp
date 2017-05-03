// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <Rcpp.h>
#include "QuadTree.h"

using namespace Rcpp;

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

bool PointInTriangle(Point p, Point p0, Point p1, Point p2)
{
    double s = p0.y * p2.x - p0.x * p2.y + (p2.y - p0.y) * p.x + (p0.x - p2.x) * p.y;
    double t = p0.x * p1.y - p0.y * p1.x + (p0.y - p1.y) * p.x + (p1.x - p0.x) * p.y;

    if ((s < 0) != (t < 0))
        return false;

    double  A = -p1.y * p2.x + p0.y * (p2.x - p1.x) + p0.x * (p1.y - p2.y) + p1.x * p2.y;

    if (A < 0)
    {
        s = -s;
        t = -t;
        A = -A;
    }

    return s > 0 && t > 0 && (s + t) <= A;
}

// [[Rcpp::export]]
IntegerVector tsearch(NumericVector x,  NumericVector y, IntegerMatrix elem, NumericVector xi, NumericVector yi)
{
  QuadTree *tree = QuadTree::create(as< std::vector<double> >(xi),as< std::vector<double> >(yi));

  int nelem = elem.nrow();
  int np = xi.size();

  Progress p(nelem, true);

  IntegerVector output(np);
  std::fill(output.begin(), output.end(), NA_INTEGER);

  // Loop over each triangle
  for (int k = 0; k < nelem; k++)
  {
    if (Progress::check_abort() )
      return output;
    else
      p.update(k);

    // Retrieve triangle A B C coordinates

    int iA = elem(k, 0) - 1;
    int iB = elem(k, 1) - 1;
    int iC = elem(k, 2) - 1;

    Point A(x(iA), y(iA));
    Point B(x(iB), y(iB));
    Point C(x(iC), y(iC));

    // Boundingbox of A B C

    double rminx = min(A.x, B.x, C.x);
    double rmaxx = max(A.x, B.x, C.x);
    double rminy = min(A.y, B.y, C.y);
    double rmaxy = max(A.y, B.y, C.y);

    double xcenter = (rminx + rmaxx)/2;
    double ycenter = (rminy + rmaxy)/2;
    double half_width = (rmaxx - rminx)/2;
    double half_height = (rmaxy - rminy )/2;

    // QuadTree search of points in enclosing boundingbox

    std::vector<Point*> points;
    tree->rect_lookup(xcenter, ycenter, half_width, half_height, points);

    // Compute if the points are in A B C

    for (int i = 0 ; i < points.size() ; i++)
    {
      if (PointInTriangle(*points[i], A, B, C))
      {
        int id = points[i]->id;
        output(id) = k + 1;
      }
    }
  }

  delete tree;

  return(output);
}