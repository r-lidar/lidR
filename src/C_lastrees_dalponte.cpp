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
#include <algorithm>
#include "Point.h"
using namespace Rcpp;

//[[Rcpp::export]]
IntegerMatrix C_lastrees_dalponte(NumericMatrix Image, IntegerMatrix Seeds, double th_seed, double th_crown, double th_tree, double DIST)
{
  bool grown = true;
  bool expend;
  int nrow  = Image.nrow();
  int ncol  = Image.ncol();

  std::vector< Pixel<double> > neighbours(4);

  IntegerMatrix Region     = clone(Seeds);
  IntegerMatrix Regiontemp = clone(Seeds);

  std::map<int, Pixel<int> > seeds;                                     // Stores all the seed as Pixel object
  std::map<int, double> sum_height;                                     // Stores the sum of the elevation of each pixel of a tree (to compute mean height)
  std::map<int, int> npixel;

  for (int i = 0 ; i < nrow ; i++)
  {
    for (int j = 0 ; j < ncol ; j++)
    {
      if (Seeds(i,j) != 0)
      {
        seeds[Seeds(i,j)] = Pixel<int>(i,j, Seeds(i,j));
        sum_height[Seeds(i,j)] = Image(i,j);
        npixel[Seeds(i,j)] = 1;
      }
    }
  }

  while (grown)
  {
    grown = false;

    for (int r = 1 ; r < nrow-1 ; r++)                                 // Loops across the entiere image
    {
      for(int k = 1 ; k < ncol-1 ; k++)
      {
        if(Region(r, k) != 0)                                          // If the pixel is already labeled
        {
          int id = Region(r, k);                                       // id of the crown for the current pixel

          Pixel<int>seed  = seeds[id];                                 // Get the seed with the label id
          double hSeed    = Image(seed.i, seed.j);                     // Seed height
          double mhCrown  = sum_height[id]/npixel[id];                 // Mean height of the crown

          // Elevation of the 4 neighbours
          neighbours[0] = Pixel<double>(r-1, k, Image(r-1, k));
          neighbours[1] = Pixel<double>(r, k-1, Image(r, k-1));
          neighbours[2] = Pixel<double>(r, k+1, Image(r, k+1));
          neighbours[3] = Pixel<double>(r+1, k, Image(r+1, k));

          for(unsigned int i = 0 ; i < neighbours.size() ; i++)                  // For each neighboring pixel
          {
            Pixel<double> px = neighbours[i];

            if (px.val > th_tree)                                       // The pixel is higher than the minimum value)
            {
              expend =
                px.val > hSeed*th_seed &&                               // La canopée est supérieure à un seuil pour ce pixel
                px.val > mhCrown*th_crown &&                            // La canopée est supérieure à un  autre seuil pour ce pixel
                px.val <= hSeed+hSeed*0.05 &&                           // La canopée est inférieure à un seuil pour ce pixel
                abs(seed.i-px.i) < DIST &&                              // Le pixel n'est pas trop loin du maximum local sur x
                abs(seed.j-px.j) < DIST &&                              // Le pixel n'est pas trop loin du maximum local sur y
                Region(px.i, px.j) == 0;

              if(expend)                                                // The pixel in part of the region
              {
                Regiontemp(px.i, px.j) = Region(r,k);                   // Add the pixel to the region
                npixel[id]++;
                sum_height[id] += Image(px.i, px.j);                    // Update the sum of the height of the region
                grown = true;
              }
            }
          }
        }
      }
    }

    std::copy( Regiontemp.begin(), Regiontemp.end(), Region.begin() );
  }

  return(Region);
}
