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
#include "Point.h"
#include "Progress.h"

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector C_lastrees_li(S4 las, double dt1, double dt2, double Zu, double th_tree, double R, bool progressbar = false)
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
  double xmin = phb["Min X"];
  double ymin = phb["Min Y"];

  unsigned int ni = X.length();            // Number of points
  unsigned int n  = ni;                    // Number of remaining points
  unsigned int k  = 1;                     // Current tree ID
  IntegerVector idtree(ni);       // The ID of each point (returned object)
  std::fill(idtree.begin(), idtree.end(), NA_INTEGER);
  Progress p(ni, progressbar);    // A progress bar and script abort options
  PointXYZ* dummy = new PointXYZ(xmin-100,ymin-100,0,-1);
  std::vector<PointXYZ*> P,N;     // Store the point in N or P group (see Li et al.)

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
  std::vector<PointXYZ*> points(ni);

  for (unsigned int i = 0 ; i < ni ; ++i)
    points[i] = new PointXYZ(X[i], Y[i], Z[i], i);

  /* *********************
   * LI ET AL ALGORITHHM *
   ***********************/

  std::sort(points.begin(), points.end(), ZSortPoint());

  while(n > 0)
  {
    PointXYZ* u = points[0];
    std::vector<bool> inN(n);

    // Stop the algo is the highest point u, which is the tree top, is below a threshold
    // Addition from original algo
    if (u->z < th_tree)
    {
      p.update(ni);
    }
    else
    {
      // Initial step no point in P or N
      P.clear();
      N.clear();

      if (p.check_abort())
      {
        for (unsigned int i = 0 ; i < points.size() ; i++) delete points[i];
        delete dummy;
        p.exit();
      }
      else
        p.update(ni-n);

      // element 0 is the current highest points and is in P
      P.push_back(u);
      idtree[u->id] = k;

      // Add dummy point in N
      N.push_back(dummy);

      // Compute the distance between the local max u and all the other point
      std::vector<double> d = sqdistance(points, *u);

      for (unsigned int i = 1 ; i < n ; ++i)
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
          double dt    = (u->z > Zu) ? dt2 : dt1;

          if ( (dmin1 > dt) || ((dmin1 <= dt) && (dmin1 > dmin2)) )
          {
            inN[i] = true;
            N.push_back(u);
          }
          else if ((dmin1 <= dt) && (dmin1 <= dmin2))
          {
            P.push_back(u);
            idtree[u->id] = k;
          }
        }
      }
    }

    // Keep the point in N and redo the loop with remining points
    std::vector<PointXYZ*> temp;
    temp.reserve(N.size()-1);

    for(unsigned int i = 0 ; i < n ; i++)
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
