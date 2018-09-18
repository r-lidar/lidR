/*
 ===============================================================================

 PROGRAMMERS:

 jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR

 COPYRIGHT:

 Copyright 2017 Jean-Romain Roussel

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

#include "RasterProcessors.h"

// [[Rcpp::export]]
NumericMatrix C_grid_canopy(S4 las, NumericMatrix bbox, double res, double subcircle = 0)
{
  S4 header = las.slot("header");
  List phb  = header.slot("PHB");
  DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));
  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  double xmin = bbox(0,0);
  double xmax = bbox(0,1);
  double ymin = bbox(1,0);
  double ymax = bbox(1,1);

  try
  {
    PointToRasterProcessor processor(xmin, ymin, xmax, ymax, res);

    if (subcircle > 0)
    {
      double angle[8] = {0, 2*M_PI/8, 4*M_PI/8, 6*M_PI/8, M_PI, 10*M_PI/8, 12*M_PI/8, 14*M_PI/8};

      for (int i = 0 ; i < X.length() ; i++)
      {
        for (int j = 0 ; j < 8 ; j++)
        {
          double x = X[i] + subcircle * cos(angle[j]);
          double y = Y[i] + subcircle * sin(angle[j]);
          processor.max(x, y, Z[i]);
        }
      }
    }
    else
    {
      for (int i = 0 ; i < X.length() ; i++)
      {
        processor.max(X[i], Y[i], Z[i]);
      }
    }

    return processor.getmatrix();
  }
  catch (std::exception const& e)
  {
    stop(e.what());
    return(NumericMatrix(0));
  }
}

