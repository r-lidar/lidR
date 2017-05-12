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
using namespace Rcpp;

// [[Rcpp::plugins("cpp0x")]]

// Does a point fall in a given polygon?
//
// Verifies for one point whether it falls in a given polygon
//
// @param vertx  numerical array of x-coordinates of polygon
// @param verty  numerical array of y-coordinates of polygon
// @param pointx numeric. x-coordinate of a point
// @param pointy numeric. y-coordinate of a point
// @return Logical. FALSE, point outside the polygon, TRUE, point is inside the polygon
// @references Adaptation of the C function written by W. Randolph Franklin
// @export
// [[Rcpp::export]]
bool point_in_polygon(NumericVector vertx, NumericVector verty, float pointx, float pointy)
{
  bool c = false;
  int nvert = vertx.length();

  for (int i = 0, j = nvert-1 ; i < nvert ; j = i++)
  {
    if( ((verty[i] > pointy) != (verty[j] > pointy)) && (pointx < (vertx[j]-vertx[i]) * (pointy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
      c = !c;
  }

  return c;
}

// Do points fall in a given polygon?
//
// Verifies for a set of points whether they fall in a given polygon
//
// @param vertx  numerical array of x-coordinates of polygon
// @param verty  numerical array of y-coordinates of polygon
// @param pointx numerical array of x-coordinates of points
// @param pointy numerical array of y-coordinates of points
// @return Logical array. FALSE, points are outside the polygon, TRUE, points are outside the polygon
// @export
// [[Rcpp::export]]
LogicalVector points_in_polygon(NumericVector vertx, NumericVector verty, NumericVector pointx, NumericVector pointy)
{
  int i;
  int npoints = pointx.length();
  LogicalVector c(npoints);

  for (i = 0 ; i < npoints ; i++)
    c[i] = point_in_polygon(vertx, verty, pointx[i], pointy[i]);

  return c;
}

// Do points fall inside a given polygon?
//
// Verifies for a set of points whether they fall inside a given polygon
//
// @param vertx  numerical list of array of x-coordinates of polygon
// @param verty  numerical list of array of y-coordinates of polygon
// @param pointx numerical array of x-coordinates of points
// @param pointy numerical array of y-coordinates of points
// @return numerical array. 0 if the points are in any polygon or the number of the polygon if points fall in a given polygon
// @export
// [[Rcpp::export]]
IntegerVector points_in_polygons(Rcpp::List vertx, Rcpp::List verty, NumericVector pointx, NumericVector pointy, bool displaybar = false)
{
  // Since lidR incoporate a QuadTree this algo could be rewritten with a QuadTree.

  int npoints = pointx.length();
  int nvert   = vertx.length();
  IntegerVector id(npoints);

  Progress p(npoints, displaybar);

  // find bouding boxes to fast the agorithm
  NumericVector bbxmin(nvert), bbxmax(nvert), bbymin(nvert), bbymax(nvert);
  for(int i = 0 ; i < nvert ; i ++)
  {
    bbxmin[i] = min(as<NumericVector>(vertx[i]));
    bbxmax[i] = max(as<NumericVector>(vertx[i]));
    bbymin[i] = min(as<NumericVector>(verty[i]));
    bbymax[i] = max(as<NumericVector>(verty[i]));
  }

  for (int i = 0 ; i < npoints ; i++)
  {
    bool found = false;
    int j = 0;

    if (Progress::check_abort() )
      return  id;
    else
      p.update(i);

    while (!found && j < nvert)
    {
      // if point is inside the bounding box run point_in_polygon else it's useless (slightly faster)
      if(bbxmin[j] <= pointx[i] || bbxmax[j] >= pointx[i] || bbymin[j] <= pointy[i] || bbymax[j] >= pointy[i])
      {
        if(point_in_polygon(vertx[j], verty[j], pointx[i], pointy[i]))
        {
          id[i] = j+1;
          found = true;
        }
      }

      j++;
    }
  }

  return id;
}
