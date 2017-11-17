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
List Cpp_grid_canopy(S4 las, double res, double subcircle = 0)
{
  S4 header = las.slot("header");
  List phb  = header.slot("PHB");
  // DataFrame data = las.slot("data");
  DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));

  double xmax = phb["Max X"];
  double xmin = phb["Min X"];
  double ymax = phb["Max Y"];
  double ymin = phb["Min Y"];

  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];

  try
  {
    PointToRasterProcessor processor(xmin - subcircle,
                                     ymin - subcircle,
                                     xmax + subcircle,
                                     ymax + subcircle,
                                     res);

    if (subcircle > 0)
    {
      double angle[8] = {0, 2*PI/8, 4*PI/8, 6*PI/8, PI, 10*PI/8, 12*PI/8, 14*PI/8};

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

    return processor.expend();
  }
  catch (std::exception const& e)
  {
    stop(e.what());
    return(List(0));
  }
}

