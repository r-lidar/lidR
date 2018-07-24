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

/*********************
 *  RASTER PROCESSOR *
 *********************/

RasterProcessor::RasterProcessor(NumericMatrix raster, double startx, double starty, double resolution)
{
  i = 0;
  j = 0;
  m_raster = raster;
  m_nrows  = raster.nrow();
  m_ncols  = raster.nrow();
  m_startx = startx;
  m_starty = starty;
  m_res    = resolution;
}

RasterProcessor::RasterProcessor(double minx, double miny, double maxx, double maxy, double res)
{
  if (maxx < minx || maxy < miny)
    throw exception("in RasterProcessor(): bounding box.");

  i = 0;
  j = 0;

  m_res = res;
  m_startx = roundany(minx);
  m_starty = roundany(miny);
  m_xmin   = minx;
  m_ymin   = miny;

  double endx = roundany(maxx) + res;
  double endy = roundany(maxy) + res;

  m_ncols  = (endx - m_startx) / m_res + 1;
  m_nrows  = (endy - m_starty) / m_res + 1;

  m_raster = NumericMatrix(m_ncols, m_nrows);
  std::fill(m_raster.begin(), m_raster.end(), NA_REAL);

  /*Rcout << "nrow =  " << m_nrows << " ncol =  " << m_ncols << std::endl;
  Rcout << "startx =  " << m_startx << " starty =  " << m_starty << std::endl;*/
}

RasterProcessor::~RasterProcessor()
{
}

void RasterProcessor::xy2ij(double x, double y)
{
  i = (int)(std::abs((m_startx - x) / m_res) + 1)-1;
  j = m_nrows - (int)(std::abs((m_starty - y) / m_res))-1;

  if (i < 0 || i >= m_ncols || j < 0 || j >= m_nrows)
    throw exception("in xy2ij(): index out of bounds");

  return;
}

double RasterProcessor::roundany(double x)
{
  return round(x / m_res) * m_res;
}

List RasterProcessor::expend()
{
  double x;
  double y;
  std::vector<double> X, Y, Z;

  for (i = 0 ; i < m_ncols ; i++)
  {
    x = m_startx + i * m_res + 0.5 * m_res;

    for (j = 0 ; j < m_nrows ; j++)
    {
      y = m_starty + (m_nrows - j - 1) * m_res + 0.5 * m_res;

      if (!NumericVector::is_na(m_raster(i,j)))
      {
        X.push_back(x);
        Y.push_back(y);
        Z.push_back(m_raster(i,j));
      }
    }
  }

  return List::create(Named("X") = X, Named("Y") = Y, Named("Z") = Z);
}

NumericMatrix RasterProcessor::getmatrix()
{
  return m_raster;
}

/******************************
 *  POINT TO RASTER PROCESSOR *
 ******************************/

PointToRasterProcessor::PointToRasterProcessor(double minx, double miny, double maxx, double maxy, double res) : RasterProcessor(minx, miny, maxx, maxy, res)
{
}

PointToRasterProcessor::~PointToRasterProcessor()
{
}

void PointToRasterProcessor::max(double x, double y, double z)
{
  xy2ij(x,y);

  if (m_raster(i,j) < z || NumericVector::is_na(m_raster(i,j)))
    m_raster(i,j) = z;

  return;
}

void PointToRasterProcessor::min(double x, double y, double z)
{
  xy2ij(x,y);

  if (m_raster(i,j) > z || NumericVector::is_na(m_raster(i,j)))
    m_raster(i,j) = z;

  return;
}

void PointToRasterProcessor::count(double x, double y)
{
  xy2ij(x,y);

  if (NumericVector::is_na(m_raster(i,j)))
    m_raster(i,j) = 1;
  else
    m_raster(i,j)++;

  return;
}
