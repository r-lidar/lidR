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
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>

using namespace Rcpp;

// Defined in cxx_utils.cpp
int which_max(NumericVector);
IntegerVector which_true(LogicalVector);
NumericVector distance(NumericVector, NumericVector, double, double);

// [[Rcpp::export]]
IntegerVector algo_li2012(NumericVector X, NumericVector Y, NumericVector Z, NumericVector dt, double R)
{
  bool finish = false;

  int ni = X.length();
  int k = 1;

  Progress p(ni, true);

  IntegerVector idpoint = seq_len(ni)-1;
  IntegerVector idtree(ni);

  NumericVector d;

  while(!finish)
  {
    int n = X.length();
    LogicalVector P(n);
    LogicalVector N(n);

    if (Progress::check_abort() )
      return  IntegerVector::create(0);
    else
      p.update(ni-n);

    // element 0 is the current highest points and is in P
    P(0) = true;
    d = distance(X, Y, X(0), Y(0));

    // exit if no point in N
    if(max(d) < dt(1))
    {
      finish = true;

      for (IntegerVector::iterator it = idpoint.begin(), end = idpoint.end() ; it != end ; ++it)
        idtree[*it] = k;
    }
    else
    {
      // the farthest point is in N
      N[which_max(d)] = true;

      // Save a lot of time by do not testing too far points
      LogicalVector too_far = d >= R;
      IntegerVector non_too_far_id = which_true(!too_far);
      IntegerVector too_far_id = which_true(too_far);

      for (IntegerVector::iterator i = too_far_id.begin(), end = too_far_id.end() ; i != end ; ++i)
      {
        P[*i] = false;
        N[*i] = true;
      }

      // loop over all the points which are not too far
      for (IntegerVector::iterator i = non_too_far_id.begin(), end = non_too_far_id.end() ; i != end ; ++i)
      {
        double dmin1 = min(distance(X[P], Y[P], X(*i), Y(*i)));
        double dmin2 = min(distance(X[N], Y[N], X(*i), Y(*i)));

        double ddt = (Z(*i) > 15) ? dt(1) : dt(0);

        if (dmin1 > ddt)
        {
          N[*i] = true;
          P[*i] = false;
        }
        else if (dmin1 <= ddt & dmin1 <= dmin2)
        {
          N[*i] = false;
          P[*i] = true;
        }
        else if (dmin1 <= ddt & dmin1 > dmin2)
        {
          N[*i] = true;
          P[*i] = false;
        }
      }

      IntegerVector id = idpoint[P];

      for (IntegerVector::iterator i = id.begin(), end = id.end() ; i != end ; ++i)
        idtree[*i] = k;

      k++;

      X = X[N];
      Y = Y[N];
      Z = Z[N];
      idpoint = idpoint[N];

    }
  }

  Rcout << std::endl;

  return idtree;
}
