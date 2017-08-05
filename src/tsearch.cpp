// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <Rcpp.h>
#include "QuadTree.h"

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector tsearch(NumericVector x,  NumericVector y, IntegerMatrix elem, NumericVector xi, NumericVector yi, bool diplaybar = false)
{
  // Shift the point cloud to the origin to avoid computer precision error
  // The shift is done by reference to save memory. The original data is shift back at the end

  double minx = mean(x);
  double miny = mean(y);
  x = x - minx;
  y = y - miny;
  xi = xi - minx;
  yi = yi - miny;

  // Algorithm

  QuadTree *tree = QuadTree::create(as< std::vector<double> >(xi), as< std::vector<double> >(yi));

  int nelem = elem.nrow();
  int np = xi.size();

  Progress p(nelem, diplaybar);

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

    // QuadTree search of points in the triangle
    std::vector<Point*> points;
    tree->triangle_lookup(A, B, C, points);

    // Return the id of the triangle
    for(std::vector<Point*>::iterator it = points.begin(); it != points.end(); it++)
    {
        int id = (*it)->id;
        output(id) = k + 1;
    }
  }

  delete tree;

  // Shift back the data
  x = x + minx;
  y = y + miny;
  xi = xi + minx;
  yi = yi + miny;

  return(output);
}