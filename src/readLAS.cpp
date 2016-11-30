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

#include <time.h>
#include <stdio.h>
#include <Rcpp.h>

#include <Rcpp.h>

#include "lasreader.hpp"
#include "laswriter.hpp"

using namespace Rcpp;

//' Read data from a las and laz file with LASlib
//'
//' Read data from a las or laz file in format 1 to 4 according to LAS specification and return a list.
//'
//' This function musn't be used as is. It is an internal function. Please use \link[lidR:readLAS]{readLAS} abstraction.
//'
//' @param file character. The name of the file which the data are to be read from
//' @param Intensity logical. do you want to load Intensity field? default: TRUE
//' @param ReturnNumber logical. do you want to load ReturnNumber field? default: TRUE
//' @param NumberOfReturns logical. do you want to load NumberOfReturns field? default: TRUE
//' @param ScanDirectionFlag logical. do you want to load ScanDirectionFlag field? default: FALSE
//' @param EdgeofFlightline logical. do you want to load EdgeofFlightline field? default: FALSE
//' @param Classification logical. do you want to load Classification field? default: TRUE
//' @param ScanAngle logical. do you want to load intensity field? default: TRUE
//' @param UserData logical. do you want to load UserData field? default: FALSE
//' @param PointSourceID logical. do you want to load PointSourceID field? default: FALSE
//' @param RGB logical. do you want to load intensity R,G and B? default: TRUE
//'
//' @return list
// [[Rcpp::export]]
List readLASdata(CharacterVector file,
             bool Intensity = true,
             bool ReturnNumber = true,
             bool NumberOfReturns = true,
             bool ScanDirectionFlag = false,
             bool EdgeofFlightline = false,
             bool Classification = true,
             bool ScanAngle = true,
             bool UserData = false,
             bool PointSourceID = false,
             bool RGB = true)
{
  try
  {
    std::string filestd = as<std::string>(file);

    LASreadOpener lasreadopener;
    lasreadopener.set_file_name(filestd.c_str());

    LASreader* lasreader = lasreadopener.open();

    if(0 == lasreader | NULL == lasreader)
      throw std::runtime_error("LASlib internal error. See message above.");

    U8 point_type = lasreader->header.point_data_format;
    int format;
    int n = lasreader->header.number_of_point_records;

    switch (point_type)
    {
    case 0:
      format = 0;
      break;
    case 1:
      format = 1;
      break;
    case 2:
      format = 2;
      break;
    case 3:
      format = 3;
      break;
    case 4:
      throw std::runtime_error("LAS format not yet supported");
      break;
    case 5:
      throw std::runtime_error("LAS format not yet supported");
      break;
    case 6:
      throw std::runtime_error("LAS format not yet supported");
      break;
    case 7:
      throw std::runtime_error("LAS format not yet supported");
      break;
    case 8:
      throw std::runtime_error("LAS format not yet supported");
      break;
    case 9:
      throw std::runtime_error("LAS format not yet supported");
      break;
    case 10:
      throw std::runtime_error("LAS format not yet supported");
      break;
    default:
      throw std::runtime_error("LAS format not valid");
    }


    NumericVector X(n);
    NumericVector Y(n);
    NumericVector Z(n);
    IntegerVector I(n);
    IntegerVector RN(n);
    IntegerVector NoR(n);
    IntegerVector SDF(n);
    IntegerVector EoF(n);
    IntegerVector C(n);
    IntegerVector SA(n);
    IntegerVector UD(n);
    IntegerVector PSI(n);
    NumericVector T(n);
    IntegerVector R(n);
    IntegerVector G(n);
    IntegerVector B(n);

    unsigned int i = 0;
    while (lasreader->read_point())
    {
      X[i]   = lasreader->point.get_x();
      Y[i]   = lasreader->point.get_y();
      Z[i]   = lasreader->point.get_z();
      I[i]   = lasreader->point.get_intensity();
      RN[i]  = lasreader->point.get_return_number();
      NoR[i] = lasreader->point.get_number_of_returns();
      SDF[i] = lasreader->point.get_scan_direction_flag();
      EoF[i] = lasreader->point.get_edge_of_flight_line();
      C[i]   = lasreader->point.get_classification();
      SA[i]  = lasreader->point.get_scan_angle_rank();
      UD[i]  = lasreader->point.get_user_data();
      PSI[i] = lasreader->point.get_point_source_ID();
      T[i]   = lasreader->point.get_gps_time();
      R[i]   = lasreader->point.get_R();
      G[i]   = lasreader->point.get_G();
      B[i]   = lasreader->point.get_B();

      i++;
    }

    lasreader->close();
    delete lasreader;

    List lasdata = List::create(X,Y,Z);
    CharacterVector field(0);
    field.push_back("X");
    field.push_back("Y");
    field.push_back("Z");

    if(Intensity)
      lasdata.push_back(I), field.push_back("Intensity");
    if(ReturnNumber)
      lasdata.push_back(RN), field.push_back("ReturnNumber");
    if(NumberOfReturns)
      lasdata.push_back(NoR), field.push_back("NumberOfReturns");
    if(ScanDirectionFlag)
      lasdata.push_back(SDF), field.push_back("ScanDirectionFlag");
    if(EdgeofFlightline)
      lasdata.push_back(EoF), field.push_back("EdgeofFlightline");
    if(Classification)
      lasdata.push_back(C), field.push_back("Classification");
    if(ScanAngle)
      lasdata.push_back(SA), field.push_back("ScanAngle");
    if(UserData)
      lasdata.push_back(UD), field.push_back("UserData");
    if(PointSourceID)
      lasdata.push_back(PSI), field.push_back("PointSourceID");
    if(RGB && (format == 2 || format == 3))
    {
      lasdata.push_back(R), field.push_back("R");
      lasdata.push_back(G), field.push_back("G");
      lasdata.push_back(B), field.push_back("B");
    }
    if(format == 1 || format == 3)
      lasdata.push_back(T); field.push_back("gpstime");

    lasdata.names() = field;

    return(lasdata);
  }
  catch (std::exception const& e)
  {
    Rcerr << "Error: " << e.what() << std::endl;
    return(List(0));
  }
}

//' Read header in a las or laz file
//'
//' Read data from a las  or laz file in format 1 to 4 according to LAS specification and return a list.
//'
//' This function musn't be used as is. It is an internal function. Please use \link[lidR:readLAS]{readLAS} abstraction.
//'
//' @param file character. the name of the file which the data are to be read from
//' @return A list
//' @export readLASheader
// [[Rcpp::export]]
List readLASheader(CharacterVector file)
{
  try
  {
    std::string filestd = as<std::string>(file);

    LASreadOpener lasreadopener;
    lasreadopener.set_file_name(filestd.c_str());

    LASreader* lasreader = lasreadopener.open();

    if(0 == lasreader | NULL == lasreader)
      throw std::runtime_error("LASlib internal error. See message above.");

    List head(0);
    head.push_back(lasreader->header.file_signature);
    head.push_back(lasreader->header.file_source_ID);
    head.push_back(lasreader->header.global_encoding);
    head.push_back(0); //lasreader->header.GetProjectId();
    head.push_back(lasreader->header.version_major);
    head.push_back(lasreader->header.version_minor);
    head.push_back(lasreader->header.system_identifier);
    head.push_back(lasreader->header.generating_software);
    head.push_back(lasreader->header.file_creation_day);
    head.push_back(lasreader->header.file_creation_year);
    head.push_back(lasreader->header.header_size);
    head.push_back(lasreader->header.offset_to_point_data);
    head.push_back(lasreader->header.number_of_variable_length_records);
    head.push_back(lasreader->header.point_data_format);
    head.push_back(lasreader->header.point_data_record_length);
    head.push_back(lasreader->header.number_of_point_records);
    head.push_back(lasreader->header.number_of_points_by_return[0]);
    head.push_back(lasreader->header.number_of_points_by_return[1]);
    head.push_back(lasreader->header.number_of_points_by_return[2]);
    head.push_back(lasreader->header.number_of_points_by_return[3]);
    head.push_back(lasreader->header.number_of_points_by_return[4]);
    head.push_back(lasreader->header.x_scale_factor);
    head.push_back(lasreader->header.y_scale_factor);
    head.push_back(lasreader->header.z_scale_factor);
    head.push_back(lasreader->header.x_offset);
    head.push_back(lasreader->header.y_offset);
    head.push_back(lasreader->header.z_offset);
    head.push_back(lasreader->header.max_x);
    head.push_back(lasreader->header.min_x);
    head.push_back(lasreader->header.max_y);
    head.push_back(lasreader->header.min_y);
    head.push_back(lasreader->header.max_z);
    head.push_back(lasreader->header.min_z);

    lasreader->close();
    delete lasreader;

    CharacterVector names(0);
    names.push_back("File Signature");
    names.push_back("File Source ID");
    names.push_back("Global Encoding");
    names.push_back("Project ID - GUID");
    names.push_back("Version Major");
    names.push_back("Version Minor");
    names.push_back("System Identifier");
    names.push_back("Generating Software");
    names.push_back("File Creation Day of Year");
    names.push_back("File Creation Year");
    names.push_back("Header Size");
    names.push_back("Offset to point data");
    names.push_back("Number of variable length records");
    names.push_back("Point Data Format ID");
    names.push_back("Point Data Record Length");
    names.push_back("Number of point records");
    names.push_back("Number of 1st return");
    names.push_back("Number of 2nd return");
    names.push_back("Number of 3rd return");
    names.push_back("Number of 4th return");
    names.push_back("Number of 5th return");
    names.push_back("X scale factor");
    names.push_back("Y scale factor");
    names.push_back("Z scale factor");
    names.push_back("X offset");
    names.push_back("Y offset");
    names.push_back("Z offset");
    names.push_back("Max X");
    names.push_back("Min X");
    names.push_back("Max Y");
    names.push_back("Min Y");
    names.push_back("Max Z");
    names.push_back("Min Z");

    head.names() = names;

    return(head);
  }
  catch (std::exception const& e)
  {
    Rcerr << "Error: " << e.what() << std::endl;
    return(List(0));
  }
}
