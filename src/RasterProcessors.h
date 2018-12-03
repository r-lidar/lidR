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

#ifndef RASTERPROCESSOR_H
#define RASTERPROCESSOR_H

#include <Rcpp.h>

using namespace Rcpp;

class RasterProcessor
{
  public:
    RasterProcessor(NumericMatrix raster, double startx, double starty, double res);
    RasterProcessor(double minx, double miny, double maxx, double maxy, double res);
    ~RasterProcessor();
    NumericMatrix getmatrix();

  protected:
    int i;
    int j;
    int m_ncols;
    int m_nrows;
    double m_startx;
    double m_starty;
    double m_xmin;
    double m_ymin;
    double m_res;
    NumericMatrix m_raster;

  protected:
    bool xy2ij(double x, double y);
};

class PointToRasterProcessor : public RasterProcessor
{
  public:
    PointToRasterProcessor(double minx, double miny, double maxx, double maxy, double res);
    ~PointToRasterProcessor();
    void max(double x, double y, double z);
    //void min(double x, double y, double z);
    void count(double x, double y);
};

#endif //RASTERPROCESSOR_H
