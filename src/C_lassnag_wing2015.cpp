/*
 ===============================================================================

PROGRAMMERS:

andrew.sanchezmeador@nau.edu - https://github.com/bi0m3trics
jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR

COPYRIGHT:

Copyright 2018 Jean-Romain Roussel

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
#include <numeric>
#include "Progress.h"
#include "QuadTree.h"

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector C_Wing2015(S4 las, NumericVector neigh_radii, double low_int_thrsh, double uppr_int_thrsh, int pt_den_req, NumericMatrix BBPRthrsh_mat)
{
   DataFrame data = as<Rcpp::DataFrame>(las.slot("data"));

   NumericVector X = data["X"];
   NumericVector Y = data["Y"];
   NumericVector Z = data["Z"];
   IntegerVector I = data["Intensity"];

   unsigned int n = X.length();                         // Number of points

   NumericVector BBPr_sph(n);                           // vector to store the Branch-Bole point ratio (BBPr) for the sphere neighborhood object
   IntegerVector ptDen_sph(n);                          // vector to store the the sphere neighborhood point density for each focal point
   NumericVector meanBBPr_sph(n);                       // vector to store the mean BBPr for each focal point in its corresponding sphere neighborhood

   NumericVector BBPr_smcyl(n);                         // BBPr for the small cylinder neighborhood (which only includes points above focal point)
   IntegerVector ptDen_smcyl(n);                        // the small cylinder neighborhood point density for each focal point
   NumericVector meanBBPr_smcyl(n);                     // the mean BBPr for each focal point in its corresponding small cylinder neighborhood

   NumericVector BBPr_bigcyl(n);                        // BBPr for the big cylinder neighborhood
   IntegerVector ptDen_bigcyl(n);                       // the big cylinder neighborhood point density for each focal point
   NumericVector meanBBPr_bigcyl(n);                    // the mean BBPr for each focal point in its corresponding big cylinder neighborhood

   QuadTree qtree(las);                                 // the quadtree for the las object

   // Step 1 - First we have to build neighborhood objects (sphere, small and large cylinders) around each focal point and get
   // the BBPr counts, then we have to calculate the actual ratio of BBPr to neighborhood points for each focal point
   // ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

   double BBPr_cnt = 0;                                 // the count of BBPr points (based on thresholds) in the neighborhood

   for (unsigned int i = 0 ; i < n ; i++)
   {
      // Step 1.a Sphere neighborhood
      // ----------------------------

      std::vector<PointXYZ> sphpts;                     // creation of an STL container of points for the sphere neighborhood object
      Sphere sphere(X[i], Y[i], Z[i], neigh_radii[0]);  // creation of a sphere object
      qtree.lookup(sphere, sphpts);                     // lookup the points in the sphere neighborhood
      ptDen_sph[i] = sphpts.size();                     // count the points in the sphere neighborhood

      BBPr_cnt = 0;
      for (unsigned int j = 0 ; j < sphpts.size() ; j++)
      {
         if (I[sphpts[j].id] <= low_int_thrsh || I[sphpts[j].id] >= uppr_int_thrsh)
            BBPr_cnt++;
      }

      BBPr_sph[i] = BBPr_cnt/sphpts.size();             // Ratio of BBPr points in the neighborhood

      // Step 1.b Small cylinder neighborhood
      // ------------------------------------

      std::vector<Point*> smcylpts;                     // creation of an STL container of points for the small cylinder neighborhood object
      Circle smcircle(X[i], Y[i], neigh_radii[1]);      // creation of a small cylinder object
      qtree.lookup(smcircle, smcylpts);                 // lookup the points in the small cylinder neighborhood

      BBPr_cnt = 0;
      double ptZ = Z[i];                                // the height of the focal point (lower end of the small cylinder)
      for (unsigned int j = 0 ; j < smcylpts.size() ; j++)
      {
         if (Z[smcylpts[j]->id] >= ptZ)
         {
            ptDen_smcyl[i]++;
            if (I[smcylpts[j]->id] <= low_int_thrsh || I[smcylpts[j]->id] >= uppr_int_thrsh)
               BBPr_cnt++;
         }
      }

      BBPr_smcyl[i] = BBPr_cnt/ptDen_smcyl[i];          // Ratio of BBPr points in the neighborhood

      // Step 1.c Big cylinder neighborhood
      // ----------------------------------

      std::vector<Point*> bigcylpts;                    // creation of an STL container of points for the big cylinder neighborhood object
      Circle bigcircle(X[i], Y[i], neigh_radii[2]);     // creation of a big cylinder object
      qtree.lookup(bigcircle, bigcylpts);               // lookup the points in the big cylinder neighborhood
      ptDen_bigcyl[i] = bigcylpts.size();               // get the point density in the big cylinder neighborhood

      BBPr_cnt = 0;
      for (unsigned int j = 0; j < bigcylpts.size(); j++)
      {
         if (I[bigcylpts[j]->id] <= low_int_thrsh || I[bigcylpts[j]->id] >= uppr_int_thrsh)
            BBPr_cnt++;
      }

      BBPr_bigcyl[i] = BBPr_cnt/bigcylpts.size();       // Ratio of BBPr points in the neighborhood
   }

   // Step 2 - Next we have to calculate he mean BBPr value for points in the neighborhood object for each focal point
   // ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

   double sum_of_elements = 0;                          // sum the elements in the neighborhood

   for (unsigned int i = 0 ; i < n ; i++)
   {
     // Step 2.a Sphere neighborhood
     // ----------------------------

      std::vector<PointXYZ> sphpts;
      Sphere sphere(X[i], Y[i], Z[i], neigh_radii[0]);
      qtree.lookup(sphere, sphpts);

      sum_of_elements = 0;
      for(unsigned int j = 0; j < sphpts.size(); j++)
      {
         sum_of_elements += BBPr_sph[sphpts[j].id];
      }

      meanBBPr_sph[i] = sum_of_elements/ptDen_sph[i];   // calculate the mean

      // Step 2.b Small cylinder neighborhood
      // ------------------------------------

      std::vector<Point*> smcylpts;
      Circle smcircle(X[i], Y[i], neigh_radii[1]);
      qtree.lookup(smcircle, smcylpts);

      sum_of_elements = 0;
      double ptZ = Z[i];                                // the height of he focal point (lower end of the small cylinder)
      for (unsigned int j = 0 ; j < smcylpts.size() ; j++)
      {
         if (Z[smcylpts[j]->id]>=ptZ)
            sum_of_elements += BBPr_smcyl[smcylpts[j]->id];
      }

      meanBBPr_smcyl[i] = sum_of_elements/ptDen_smcyl[i]; // calculate the mean

      // Step 2.c Big cylinder neighborhood
      // ----------------------------------

      std::vector<Point*> bigcylpts;
      Circle bigcircle(X[i], Y[i], neigh_radii[2]);
      qtree.lookup(bigcircle, bigcylpts);

      sum_of_elements = 0;
      for(unsigned int j = 0 ; j < bigcylpts.size() ; j++)
      {
         sum_of_elements += BBPr_bigcyl[bigcylpts[j]->id];
      }

      meanBBPr_bigcyl[i] = sum_of_elements/ptDen_bigcyl[i]; // calculate the mean
   }

   // Step 3 - Finally classify each point based on point density requirements and mean BBPr values from on the lookup table
   // in Wing et al 2015 - Table 2 - pg. 172 - here, the values supplied/specified by user in BBPRthrsh_mat
   // ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

   IntegerVector output(n); // vector to store the snag (or tree) classification values

   for(unsigned int i = 0 ; i < n ; i++)
   {
      if (ptDen_sph[i] >= pt_den_req &&
          meanBBPr_sph[i] >= BBPRthrsh_mat(0,0) &&
          ptDen_smcyl[i] >= pt_den_req &&
          meanBBPr_smcyl[i] >= BBPRthrsh_mat(1,0) &&
          ptDen_bigcyl[i] >= pt_den_req &&
          meanBBPr_bigcyl[i] >= BBPRthrsh_mat(2,0))
      {
         output[i] = 1;  // General snag class
      }
      else if (ptDen_sph[i] >= 2 &&
               ptDen_sph[i] <= pt_den_req &&
               meanBBPr_sph[i] >= BBPRthrsh_mat(0,1) &&
               ptDen_smcyl[i] >= 2 &&
               ptDen_smcyl[i] <= pt_den_req &&
               meanBBPr_smcyl[i] >= BBPRthrsh_mat(1,1) &&
               ptDen_bigcyl[i] >= 2 &&
               ptDen_bigcyl[i] <= pt_den_req &&
               meanBBPr_bigcyl[i] >= BBPRthrsh_mat(2,1))
      {
         output[i] = 2; // Small snag class
      }
      else if (ptDen_sph[i] >= pt_den_req &&
               meanBBPr_sph[i] >= BBPRthrsh_mat(0,2) &&
               ptDen_smcyl[i] >= pt_den_req &&
               meanBBPr_smcyl[i] >= BBPRthrsh_mat(1,2) &&
               ptDen_bigcyl[i] >= pt_den_req*7 &&
               meanBBPr_bigcyl[i] >= BBPRthrsh_mat(2,2))
      {
         output[i] = 3; // Live crown edge snag class
      }
      else if (ptDen_sph[i] >= pt_den_req &&
               meanBBPr_sph[i] >= BBPRthrsh_mat(0,3) &&
               ptDen_smcyl[i] >= pt_den_req &&
               meanBBPr_smcyl[i] >= BBPRthrsh_mat(1,3) &&
               ptDen_bigcyl[i] >= pt_den_req*15 &&
               meanBBPr_bigcyl[i] >= BBPRthrsh_mat(2,3))
      {
         output[i] = 4; // High canopy cover snag class
      }
      else
      {
         output[i] = 0; // Remaining points assigned to live tree class
      }
   }

   return(output);
}
