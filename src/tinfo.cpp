/*
 ===============================================================================

 PROGRAMMERS:

jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR

COPYRIGHT:

Copyright 2016 Jean-Romain Roussel

This file is part of lidR R package.

lidR is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>

===============================================================================
*/

#include <Rcpp.h>
using namespace Rcpp;

// @param M a matrix n x 4 describing a delaunay triangulation. Each row a set of indices to the points + a triangle id
// This matrix is expected to be pruned of useless triangles in attempt to reduce the computation times
// @param X a matrix m x 3 with the points coordinates
// @param size the number of triangle in original dataset
// @return n x 6 matrix with 3 coord of normal vector, intercept, area, projected area, max edge size
// [[Rcpp::export]]
NumericMatrix tinfo(IntegerMatrix M, NumericMatrix X)
{
  NumericMatrix N(M.nrow(), 7);
  std::fill(N.begin(), N.end(), NA_REAL);

  for (int i = 0, end = M.nrow() ; i < end ; i++)
  {
    int p1 = M(i,0)-1;
    int p2 = M(i,1)-1;
    int p3 = M(i,2)-1;

    NumericVector A = NumericVector::create(X(p1,0), X(p1,1), X(p1,2));
    NumericVector B = NumericVector::create(X(p2,0), X(p2,1), X(p2,2));
    NumericVector C = NumericVector::create(X(p3,0), X(p3,1), X(p3,2));

    NumericVector u = A - B;
    NumericVector v = A - C;
    NumericVector w = B - C;

    // Cross product
    NumericVector n(3);
    n(0) = u(1)*v(2)-u(2)*v(1);
    n(1) = u(2)*v(0)-u(0)*v(2);
    n(2) = u(0)*v(1)-u(1)*v(0);

    // normal vector
    N(i,0) = n(0);
    N(i,1) = n(1);
    N(i,2) = n(2);

    // intercept
    N(i,3) = sum(-n*C);

    // area and projected area
    N(i,4) = std::fabs(0.5 * sqrt(n(0) * n(0) + n(1) * n(1) + n(2) * n(2)));
    N(i,5) = std::fabs(0.5 * n(2));

    // max edge length
    u.erase(2);
    v.erase(2);
    w.erase(2);
    NumericVector e = NumericVector::create(sqrt(sum(pow(u, 2))), sqrt(sum(pow(v, 2))), sqrt(sum(pow(w, 2))));
    N(i,6) = max(e);
  }

  colnames(N) = CharacterVector::create("nx", "ny", "nz", "intercept", "xyzarea", "xyarea", "maxedge");

  return N;
}
