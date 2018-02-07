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

// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <Rcpp.h>
#include "Point.h"

using namespace Rcpp;

struct SortPoint
{
  SortPoint(const NumericVector _Z) : Z(_Z) {}

  bool operator()(const Point* lhs, const Point* rhs) const
  {
    return Z(lhs->id) > Z(rhs->id);
  }

  private:
    NumericVector Z;
};

// [[Rcpp::export]]
IntegerVector algo_li2012(S4 las, double dt1, double dt2, double th_tree, double R, bool progressbar = false)
{
  /* *********************
   * INITALISATION STUFF *
   ***********************/

  // DataFrame data = las.slot("data");
  DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));

  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  S4 header = las.slot("header");
  List phb  = header.slot("PHB");
  double xmax = phb["Max X"];
  double xmin = phb["Min X"];
  double ymax = phb["Max Y"];
  double ymin = phb["Min Y"];

  int ni = X.length();            // Number of points
  int n  = ni;                    // Number of remaining points
  int k  = 1;                     // Current tree ID
  IntegerVector idtree(ni);       // The ID of each point (returned object)
  std::fill(idtree.begin(), idtree.end(), NA_INTEGER);
  Progress p(ni, progressbar);    // A progress bar and script abort options
  Point* dummy = new Point(xmin-100,ymin-100,-1);
  std::vector<Point*> P,N;        // Store the point in N or P group (see Li et al.)

  // Reserve memory for N et P group
  // (will statistically reduce the number of dynamic reallocation)
  int alloc = 3*R*10;
  P.reserve(alloc);
  N.reserve(alloc);

  // Square distance to speed up computation (dont need sqrt)
  R = R * R;
  dt1 = dt1 * dt1;
  dt2 = dt2 * dt2;

  // Convert the R data into STL containers of points
  std::vector<Point*> points(ni);

  for (int i = 0 ; i < ni ; ++i)
    points[i] = new Point(X[i], Y[i], i);

  /* *********************
   * LI ET AL ALGORITHHM *
   ***********************/

  std::sort(points.begin(), points.end(), SortPoint(Z));

  while(n > 0)
  {
    Point* u = points[0];
    std::vector<bool> inN(n);

    // Stop the algo is the highest point u, which is the tree top, is below a threshold
    // Addition from original algo
    if (Z[u->id] < th_tree)
    {
      p.update(ni);
    }
    else
    {
      // Initial step no point in P or N
      P.clear();
      N.clear();

      if (Progress::check_abort() )
        return  IntegerVector::create(0);
      else
        p.update(ni-n);

      // element 0 is the current highest points and is in P
      P.push_back(u);
      idtree[u->id] = k;

      // Add dummy point in N
      N.push_back(dummy);

      // Compute the distance between the local max u and all the other point
      std::vector<double> d = sqdistance(points, *u);

      for (int i = 1 ; i < n ; ++i)
      {
        u = points[i];

        if(d[i] > R)            // If d > R those points are not the current segmented tree
        {
          inN[i] = true;
        }
        else                    // If d <= R classify point base on Li et al. rules
        {
          std::vector<double> dP = sqdistance(P, *u);
          std::vector<double> dN = sqdistance(N, *u);

          double dmin1 = *std::min_element(dP.begin(), dP.end());
          double dmin2 = *std::min_element(dN.begin(), dN.end());

          double dt    = (Z[u->id] > 15) ? dt2 : dt1;

          if ( (dmin1 > dt) || (dmin1 <= dt & dmin1 > dmin2) )
          {
            inN[i] = true;
            N.push_back(u);
          }
          else if (dmin1 <= dt & dmin1 <= dmin2)
          {
            P.push_back(u);
            idtree[u->id] = k;
          }
        }
      }
    }

    // Keep the point in N and redo the loop with remining points
    std::vector<Point*> temp;
    temp.reserve(N.size()-1);

    for(int i = 0 ; i < n ; i++)
    {
      if(inN[i])
        temp.push_back(points[i]);
      else
        delete points[i];
    }

    points.swap(temp);
    n = points.size();
    k++;                        // Increase current tree id
  }

  delete dummy;

  return idtree;
}
