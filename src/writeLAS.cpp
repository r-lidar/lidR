#include <Rcpp.h>

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "laswriter.hpp"

using namespace Rcpp;

//' Write a las file with LASlib
//'
//' Methods to write las files using LASlib
//'
//' This function musn't be used as is. It is an internal function. Please use \link[lidR:writeLAS]{writeLAS} abstraction.
//'
//' @param file character. filename of .las file
//' @param LASheader list a header from a LAS object
//' @param X numeric array X data
//' @param Y numeric array Y data
//' @param Z numeric array Z data
//' @param I integer array intensity data
//' @param RN integer array return number data
//' @param NoR integer array number of returns data
//' @param SDF integer array scan direction flag data
//' @param EoF integer array edge of flightline data
//' @param C integer array classification data
//' @param SA integer array scan angle data
//' @param UD integer array user data data
//' @param PSI integer array point source id data
//' @param T numeric array gpstime data
//' @param R integer array red data
//' @param G integer array green data
//' @param B integer array blue data
//' @return void
//' @export LASlibWrite
// [[Rcpp::export]]
void LASlibWrite(CharacterVector file,
             List LASheader,
             NumericVector X,
             NumericVector Y,
             NumericVector Z,
             IntegerVector I = IntegerVector(0),
             IntegerVector RN = IntegerVector(0),
             IntegerVector NoR = IntegerVector(0),
             IntegerVector SDF = IntegerVector(0),
             IntegerVector EoF = IntegerVector(0),
             IntegerVector C = IntegerVector(0),
             IntegerVector SA = IntegerVector(0),
             IntegerVector UD = IntegerVector(0),
             IntegerVector PSI = IntegerVector(0),
             NumericVector T = NumericVector(0),
             IntegerVector R  = IntegerVector(0),
             IntegerVector G = IntegerVector(0),
             IntegerVector B = IntegerVector(0))
{
  try
  {
    class LASheader header;

    header.file_source_ID = (int)LASheader["File Source ID"];
    header.version_major = (int)LASheader["Version Major"];
    header.version_minor = (int)LASheader["Version Minor"];
    header.file_creation_day =  (int)LASheader["File Creation Day of Year"];
    header.file_creation_year = (int)LASheader["File Creation Year"];
    header.point_data_format = (int)LASheader["Point Data Format ID"];
    header.x_scale_factor = (double)LASheader["X scale factor"];
    header.y_scale_factor = (double)LASheader["Y scale factor"];
    header.z_scale_factor = (double)LASheader["Z scale factor"];
    header.x_offset =  (double)LASheader["X offset"];
    header.y_offset =  (double)LASheader["Y offset"];
    header.z_offset =  (double)LASheader["Z offset"];
    header.point_data_record_length = (int)LASheader["Point Data Record Length"];
    strcpy(header.generating_software, "lidR R package");

    std::string filestd = as<std::string>(file);

    LASwriteOpener laswriteopener;
    laswriteopener.set_file_name(filestd.c_str());

    LASpoint p;
    p.init(&header, header.point_data_format, header.point_data_record_length, 0);

    LASwriter* laswriter = laswriteopener.open(&header);

    for(int i = 0 ; i < X.length() ; i++)
    {
      p.set_X((X[i]-header.x_offset)/header.x_scale_factor );
      p.set_Y((Y[i]-header.y_offset)/header.y_scale_factor );
      p.set_Z((Z[i]-header.z_offset)/header.z_scale_factor );
      p.set_intensity((U16)I[i]);

      if(I.length() > 0){ p.set_intensity((U16)I[i]); }
      if(RN.length() > 0){ p.set_return_number((U8)RN[i]); }
      if(NoR.length() > 0){ p.set_number_of_returns((U8)NoR[i]); }
      if(SDF.length() > 0){ p.set_scan_direction_flag((U8)SDF[i]); }
      if(EoF.length() > 0){ p.set_edge_of_flight_line((U8)EoF[i]); }
      if(C.length() > 0){ p.set_classification((U8)C[i]); }
      if(SA.length() > 0){ p.set_scan_angle_rank((I8)SA[i]); }
      if(UD.length() > 0){ p.set_user_data((U8)UD[i]); }
      if(PSI.length() > 0){ p.set_point_source_ID((U16)PSI[i]); }
      if(T.length() > 0){ p.set_gps_time((F64)T[i]); }
      if(R.length() > 0)
      {
        p.set_R((U16)R[i]);
        p.set_G((U16)G[i]);
        p.set_B((U16)B[i]);
      }

      laswriter->write_point(&p);
      laswriter->update_inventory(&p);
    }

    laswriter->update_header(&header, TRUE);
    I64 total_bytes = laswriter->close();
    delete laswriter;
  }
  catch (std::exception const& e)
  {
    Rcerr << "Error: " << e.what() << std::endl;
  }
}