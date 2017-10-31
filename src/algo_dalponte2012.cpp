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
IntegerMatrix algo_dalponte(NumericMatrix Image, IntegerMatrix Seeds, double th_seed, double th_crown, double th_three, double DIST)
{
  bool expend;
  bool finished = false;

  int nrow = Image.nrow();
  int ncol = Image.ncol();

  std::vector< Pixel<double> > neighbour(8);

  IntegerMatrix OldRegion  = clone(Seeds);
  IntegerMatrix Region     = clone(Seeds);
  IntegerMatrix Regiontemp = clone(Seeds);
  IntegerMatrix Check(nrow, ncol);

  std::vector< Pixel<int> > seeds;
  std::vector<double> sum_height;

  for (int i = 0 ; i < nrow ; i++)
  {
    for (int j = 0 ; j < ncol ; j++)
    {
      if (Seeds(i,j) != 0)
      {
        seeds.push_back(Pixel<int>(i,j, Seeds(i,j)));
        sum_height.push_back(Image(i,j));
      }
    }
  }

  std::vector<int> npixel(seeds.size());
  std::fill(npixel.begin(), npixel.end(), 1);

  while (!finished)
  {
    finished = true;

    for (int r = 1 ; r < nrow-1 ; r++)
    {
      for(int k = 1 ; k < ncol-1 ; k++)
      {
        if(Check(r, k) == 0 && Region(r, k) != 0)                       // If the pixel is a crown and was not tested yet
        {
          int ind = Region(r, k)-1;                                     // ID of the crown for the current pixel

          Pixel<int>seed  = seeds[ind];                                 // Coordonnées du maximum local d'indice ind
          double rvSeed   = Image(seed.i, seed.j);                      // Seed height
          double rvCrown  = sum_height[ind]/npixel[ind];                // Mean height of the crown

          // Elevation of the 4 neighbours
          neighbour[0] = Pixel<double>(r-1, k, Image(r-1, k));
          neighbour[1] = Pixel<double>(r, k-1, Image(r, k-1));
          neighbour[2] = Pixel<double>(r, k+1, Image(r, k+1));
          neighbour[3] = Pixel<double>(r+1, k, Image(r+1, k));

          /*neighbour[4] = Pixel<double>(r-1, k-1, Image(r-1, k-1));
          neighbour[5] = Pixel<double>(r-1, k+1, Image(r-1, k+1));
          neighbour[6] = Pixel<double>(r+1, k+1, Image(r+1, k+1));
          neighbour[7] = Pixel<double>(r+1, k-1, Image(r+1, k-1));*/

          // Test les coordonnées pour trouver celles qui correspondend au test
          for(int i = 0 ; i < neighbour.size() ; i++)
          {
            Pixel<double> px = neighbour[i];

            if (px.val > th_three)                                  // La canopée ne vaut pas 0 pour ce pixel
            {
              expend =
                px.val > rvSeed*th_seed &&                          // La canopée est supérieure à un seuil pour ce pixel
                px.val > rvCrown*th_crown &&                        // La canopée est supérieure à un  autre seuil pour ce pixel
                px.val <= rvSeed+rvSeed*0.05 &&                     // La canopée est inférieure à un seuil pour ce pixel
                abs(seed.i-px.i) < DIST &&                          // Le pixel n'est pas trop loin du maximum local sur x
                abs(seed.j-px.j) < DIST &&                          // Le pixel n'est pas trop loin du maximum local sur y
                Region(px.i, px.j) == 0;

              if(expend)                                            // le pixel appartient à la courrone
              {
                Regiontemp(px.i, px.j) = Region(r, k);              // On ajoute le pixel
                finished = false;
              }
            }
          }
        }
      }
    }

    std::fill(npixel.begin(), npixel.end(), 0);
    std::fill(sum_height.begin(), sum_height.end(), 0);

    // To compute mean height of the crown
    for (int i = 0 ; i < nrow ; i++)
    {
      for (int j = 0 ; j < ncol ; j++)
      {
        if (Regiontemp(i,j) != 0)
        {
          npixel[Regiontemp(i,j) -1]++;
          sum_height[Regiontemp(i,j) -1] += Image(i,j);
        }
      }
    }

    std::copy( Regiontemp.begin(), Regiontemp.end(), Region.begin() );
    std::copy( OldRegion.begin(), OldRegion.end(), Check.begin() );
    std::copy( Regiontemp.begin(), Regiontemp.end(), OldRegion.begin() );
  }

  return(Region);
}

