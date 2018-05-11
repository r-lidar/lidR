/*
 ===============================================================================

PROGRAMMERS:

jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR

COPYRIGHT:

Copyright 2018 Jean-Romain Roussel

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

LogicalVector C_LocalMaximaPoints(S4 las, double ws, double min_height);

// [[Rcpp::export]]
IntegerVector C_lastrees_li2(S4 las, double dt1, double dt2, double Zu, double R, double th_tree, double radius, bool progressbar = false)
{
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

  // The ID of each point (returned object)
  IntegerVector idtree(ni);
  std::fill(idtree.begin(), idtree.end(), NA_INTEGER);

  // A progress bar and script abort options
  Progress p(ni, progressbar);

  // Square distance to speed up computation (dont need sqrt)
  radius = radius * radius;
  dt1 = dt1 * dt1;
  dt2 = dt2 * dt2;

  /* =====================
  * LI ET AL ALGORITHHM *
  ======================*/

  // Li, W., Guo, Q., Jakubowski, M. K., & Kelly, M. (2012). A New Method for Segmenting Individual
  // Trees from the Lidar Point Cloud. Photogrammetric Engineering & Remote Sensing, 78(1), 75–84.
  // https://doi.org/10.14358/PERS.78.1.75

  // Find if a point is a local maxima within an R windows
  LogicalVector is_lm = C_LocalMaximaPoints(las, R, 0);

  // U the points to be segmented (see Li et al. page 78)
  std::vector<PointXYZ*> U(ni);
  for (unsigned int i = 0 ; i < ni ; ++i)
    U[i] = new PointXYZ(X[i], Y[i], Z[i], i);

  // N and P groups (see Li et al. page 78)
  std::vector<PointXYZ*> P,N;
  P.reserve(100);
  N.reserve(100);

  // A dummy point out of the dataset (see Li et al. page 79)
  PointXYZ* dummy = new PointXYZ(xmin-100,ymin-100,0,-1);

  // Z-sort the point cloud U
  std::sort(U.begin(), U.end(), ZSortPoint());

  while(n > 0)
  {
    PointXYZ* u = U[0];
    std::vector<bool> inN(n);

    // Stop the algo is the highest point u, which is the target tree top, is below a threshold
    // Addition from original algo to limit over segmentaton
    if (u->z < th_tree)
    {
      p.update(ni);
    }
    else
    {
      if (p.check_abort())
      {
        for (unsigned int i = 0 ; i < U.size() ; i++) delete U[i];
        delete dummy;
        p.exit();
      }

      p.update(ni-n);

      // Initial step no point in P or N
      P.clear();
      N.clear();

      // element 0 is the current highest point and is in P (target tree)
      P.push_back(u);
      idtree[u->id] = k;

      // Add the dummy point in N
      N.push_back(dummy);

      // Compute the distance between the current point u and all the other points of U
      // This is not in the original algo. This is an optimisation to reduce the computation
      // time (see line 136).
      std::vector<double> d = sqdistance(U, *u);

      // Loop over each point of U (but the global maximum that is alredy in P)
      for (unsigned int i = 1 ; i < n ; ++i)
      {
        u = U[i];

        // If d > radius this point u is far and thus it is not in the current segmented tree
        // We don't need to apply the li et al. rules. This speed up a lot the computations
        if(d[i] > radius)
        {
          inN[i] = true;
        }
        // If d <= radius classify the point u based on Li et al. rules
        else
        {
          std::vector<double> dP = sqdistance(P, *u);
          std::vector<double> dN = sqdistance(N, *u);

          double dmin1 = *std::min_element(dP.begin(), dP.end());
          double dmin2 = *std::min_element(dN.begin(), dN.end());
          double dt    = (u->z > Zu) ? dt2 : dt1;

          if(is_lm[u->id] && dmin1 > dt) // if u is a local maximum
          {
              inN[i] = true;
              N.push_back(u);
          }
          else // if u is not a local maximum
          {
            if (dmin1 <= dmin2)
            {
              P.push_back(u);
              idtree[u->id] = k;
            }
            else
            {
              inN[i] = true;
              N.push_back(u);
            }
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
        temp.push_back(U[i]);
      else
        delete U[i];
    }

    U.swap(temp);
    n = U.size();
    k++;                        // Increase current tree id
  }

  delete dummy;

  return idtree;
}
