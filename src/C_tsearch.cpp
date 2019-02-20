/*
 ===============================================================================

 PROGRAMMERS:

 jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR

 COPYRIGHT:

 Copyright 2017-2019 Jean-Romain Roussel

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
#include "QuadTree.h"
#include "Progress.h"
#include "myomp.h"

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector C_tsearch(NumericVector x, NumericVector y, IntegerMatrix elem, NumericVector xi, NumericVector yi, int ncpu)
{
  QuadTree tree(xi, yi);

  int nelem = elem.nrow();
  int np = xi.size();

  bool abort = false;

  Progress pb(nelem, "Searching in TIN: ");

  IntegerVector output(np);
  std::fill(output.begin(), output.end(), NA_INTEGER);

  // Loop over each triangle
  #pragma omp parallel for num_threads(ncpu)
  for (int k = 0; k < nelem; k++)
  {
    if (abort) continue;
    if (pb.check_interrupt()) abort = true; // No data race here because only thread 0 can actually write
    pb.increment();

    // Retrieve triangle A B C coordinates
    int iA = elem(k, 0) - 1;
    int iB = elem(k, 1) - 1;
    int iC = elem(k, 2) - 1;

    Point A(x(iA), y(iA));
    Point B(x(iB), y(iB));
    Point C(x(iC), y(iC));

    Triangle triangle(A,B,C);
    std::vector<Point*> points;
    tree.lookup(triangle, points);

    // Return the id of the triangle
    #pragma omp critical
    {
      for(std::vector<Point*>::iterator it = points.begin(); it != points.end(); it++)
      {
        int id = (*it)->id;
        output(id) = k + 1;
      }
    }
  }

  if (abort) throw Rcpp::internal::InterruptedException();

  return(output);
}
