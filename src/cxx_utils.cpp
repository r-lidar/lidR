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

// [[Rcpp::export]]
IntegerVector fast_table(IntegerVector x, int size = 5)
{
  IntegerVector tbl(size);

  for (IntegerVector::iterator it = x.begin(), end = x.end() ; it != end ; ++it)
  {
    if (*it <= size)
      tbl(*it-1)++;
  }

  return tbl;
}

// [[Rcpp::export]]
int fast_countequal(NumericVector x, double t)
{
  int n = 0;

  for (NumericVector::iterator it = x.begin(), end = x.end() ; it != end ; ++it)
  {
    if (*it == t)
      n++;
  }

  return n;
}

// [[Rcpp::export]]
int fast_countbelow(NumericVector x, double t)
{
  int n = 0;

  for (NumericVector::iterator it = x.begin(), end = x.end() ; it != end ; ++it)
  {
    if (*it < t)
      n++;
  }

  return n;
}

// [[Rcpp::export]]
int fast_countover(NumericVector x, double t)
{
  int n = 0;

  for (NumericVector::iterator it = x.begin(), end = x.end() ; it != end ; ++it)
  {
    if (*it > t)
      n++;
  }

  return n;
}


