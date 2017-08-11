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
#include <algorithm>
#include "QuadTree.h"

using namespace Rcpp;

// Defined in cxx_utils.cpp
NumericVector sqdistance(NumericVector, NumericVector, double, double);

// [[Rcpp::export]]
IntegerVector algo_li2012(NumericVector X, NumericVector Y, const NumericVector Z, double dt1, double dt2, double R, bool displaybar = false)
{
  bool end = false;

  int ni = X.length();
  int n  = ni;
  int k  = 1;

  Progress p(ni, displaybar);

  IntegerVector idpoint = seq_len(ni)-1;
  IntegerVector idtree(ni);

  R = R * R;
  dt1 = dt1 * dt1;
  dt2 = dt2 * dt2;

  while(!end)
  {
    // Intial step not point in P or N
    LogicalVector N(n);
    NumericVector XP,XN,YP,YN;

    if (Progress::check_abort() )
      return  IntegerVector::create(0);
    else
      p.update(ni-n);

    // element 0 is the current highest points and is in P
    XP.push_back(X(0));
    YP.push_back(Y(0));
    idtree[idpoint[0]] = k;

    // Add dummy point in N
    XN.push_back(X(0)+100);
    YN.push_back(Y(0)+100);

    // Compute the distance between the local max u and all the other point
    NumericVector d = sqdistance(X, Y, X(0), Y(0));

    for (int i = 1 ; i < n ; ++i)
    {
      if(d[i] > R)            // If d > R those points are not the current segmented tree
      {
        N[i] = true;
      }
      else                    // If d <= R classify point base on Li et al. rules
      {
        double dmin1 = min(sqdistance(XP, YP, X[i], Y[i]));
        double dmin2 = min(sqdistance(XN, YN, X[i], Y[i]));
        double dt    = (Z[idpoint[i]] > 15) ? dt2 : dt1;

        if ( (dmin1 > dt) || (dmin1 <= dt & dmin1 > dmin2) )
        {
          N[i] = true;
          XN.push_back(X(i));
          YN.push_back(Y(i));
        }
        else if (dmin1 <= dt & dmin1 <= dmin2)
        {
          XP.push_back(X(i));
          YP.push_back(Y(i));
          idtree[idpoint[i]] = k;
        }
      }
    }

    // Increase current tree id
    k++;

    // Keep the point in N and redo the loop with remining points
    X = X[N];
    Y = Y[N];
    idpoint = idpoint[N];

    n = X.length();

    if(n == 0)
      end = true;
  }

  return idtree;
}

// =======================================================================================
// This a v2 version of the function with the aim to speed-up the algorithm
// Currently the computation speed is equal with v1
//========================================================================================

std::vector<double> sqdistance(std::vector<Point*>& pts, Point u);

struct SortFunc
{
  SortFunc(const NumericVector _Z) : Z(_Z) {}

  bool operator()(const Point* lhs, const Point* rhs) const
  {
    return Z(lhs->id) > Z(rhs->id);
  }

private:
  NumericVector Z;
};

// [[Rcpp::export]]
IntegerVector algo_li2012_v2(NumericVector X, NumericVector Y, const NumericVector Z, double dt1, double dt2, double R, bool displaybar = false)
{
  int ni = X.length();
  int n = ni;
  int k = 1;
  int j = 0;

  Progress p(ni, displaybar);

  IntegerVector treeID(ni);

  QuadTree *tree = QuadTree::create(as< std::vector<double> >(X),as< std::vector<double> >(Y));

  dt1 = dt1 * dt1;
  dt2 = dt2 * dt2;

  // The higthest point (point were ordered befor to enter the function)
  Point u(X(0), Y(0), 0);

  while(n > 0)
  {
    if (Progress::check_abort() )
      return  IntegerVector::create(0);
    else
      p.update(ni-n);

    // Intial step not point in P or N
    std::vector<Point*> P, N;

    // u is the highest points and is in P
    P.push_back(&u);
    treeID(u.id) = k;

    // Add a dummy point in N
    Point dummy(u.x + 100, u.y + 100);
    N.push_back(&dummy);

    // Get points within a circle C(u, R). This enable to reduce heavily the amount of computations
    std::vector<Point*> pts;
    tree->circle_lookup(u.x, u.y, R, pts);
    std::sort(pts.begin(), pts.end(), SortFunc(Z));

    // For each point in the circle C classify based on Li et al. rules
    for(std::vector<Point*>::iterator p = pts.begin(); p != pts.end(); p++)
    {
      std::vector<double> dP = sqdistance(P, **p);
      std::vector<double> dN = sqdistance(N, **p);

      double dmin1 = *std::min_element(dP.begin(), dP.end());
      double dmin2 = *std::min_element(dN.begin(), dN.end());
      double dt    = (Z[(*p)->id] > 15) ? dt2 : dt1;

      if ( (dmin1 > dt) || (dmin1 <= dt && dmin1 > dmin2) )
        N.push_back(*p);
      else
      {
        P.push_back(*p);
        treeID((*p)->id) = k;
        //tree->remove(*(*p));
        n--;
      }
    }

    // Find the new highest point which is the next non classified point
    while (treeID(j) != 0 && j < ni) j++;

    // update u and redo the loop
    u.x  = X(j);
    u.y  = Y(j);
    u.id = j;

    k++;
  }

  delete tree;

  return treeID;
}
