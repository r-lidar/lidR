#include <Rcpp.h>

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "laswriter.hpp"

using namespace Rcpp;

//' Write a las file with liblas
//'
//' Methods to write las files using liblas
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
//' @export liblasWriteLAS
// [[Rcpp::export]]
void liblasWriteLAS(CharacterVector file,
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
    std::string filestd = as<std::string>(file);

    /*std::string FileSignature = LASheader["File Signature"];
    int FileSourceID = LASheader["File Source ID"];
    int GlobalEncoding = LASheader["Global Encoding"];
    std::string ProjectIDGUID = LASheader["Project ID - GUID"];*/
    int VersionMajor = LASheader["Version Major"];
    int VersionMinor = LASheader["Version Minor"];
    std::string SystemIdentifier = LASheader["System Identifier"];
    std::string GeneratingSoftware = LASheader["Generating Software"];
    int FileCreationDayofYear = LASheader["File Creation Day of Year"];
    int FileCreationYear = LASheader["File Creation Year"];
    /*int HeaderSize = LASheader["Header Size"];
    double Offsettopointdata = LASheader["Offset to point data"];
    double Numberofvariablelengthrecords = LASheader["Number of variable length records"];*/
    int PointDataFormatID = LASheader["Point Data Format ID"];
    int PointDataRecordLength = LASheader["Point Data Record Length"];
    //double Numberofpointrecords = LASheader["Number of point records"];
    //double Numberofpointsbyreturn = LASheader["Number of points by return"];
    double Xscalefactor = LASheader["X scale factor"];
    double Yscalefactor = LASheader["Y scale factor"];
    double Zscalefactor = LASheader["Z scale factor"];
    double Xoffset = LASheader["X offset"];
    double Yoffset = LASheader["Y offset"];
    double Zoffset = LASheader["Z offset"];
    /*double MaxX = LASheader["Max X"];
    double MinX = LASheader["Min X"];
    double MaxY = LASheader["Max Y"];
    double MinY = LASheader["Min Y"];
    double MaxZ = LASheader["Max Z"];
    double MinZ = LASheader["Min Z"];
    uint16_t offset = 227;*/

    class LASheader header;

    //header.SetFileSignature(as<std::string>(LASheader["File Signature"]));
    //header.file_source_ID = FileSourceID;
    //
    //header.SetProjectId();
    header.version_major = VersionMajor;
    header.version_minor = VersionMinor;
    //header.SetSystemId();
    //header.SetSoftwareId();
    header.file_creation_day =  FileCreationDayofYear;
    header.file_creation_year = FileCreationYear;
    //header.header_size = HeaderSize;
    //header.offset_to_point_data =  offset;
    //header.number_of_variable_length_records = Numberofvariablelengthrecords;
    header.point_data_format = PointDataFormatID;
    //header.SetDataRecordLength();
    //header.SetPointRecordsCount(numeric_cast<uint32_t>(X.length())); // WARNING
    //header.SetPointRecordsByReturnCount();
    header.x_scale_factor = Xscalefactor;
    header.y_scale_factor = Yscalefactor;
    header.z_scale_factor = Zscalefactor;
    header.x_offset =  Xoffset;
    header.y_offset =  Yoffset;
    header.z_offset =  Zoffset;
    header.point_data_record_length = PointDataRecordLength;
    /*header.max_x = MaxX;
    header.max_y = MaxY;
    header.max_z = MaxZ;
    header.min_x = MinX;
    header.min_y = MinY;
    header.min_z = MinZ;*/

    LASwriteOpener laswriteopener;
    laswriteopener.set_file_name(filestd.c_str());

    LASpoint p;
    p.init(&header, header.point_data_format, header.point_data_record_length, 0);

    LASwriter* laswriter = laswriteopener.open(&header);

    for(int i = 0 ; i < X.length() ; i++)
    {
      p.set_X((X[i]-Xoffset)/Xscalefactor);
      p.set_Y((Y[i]-Yoffset)/Yscalefactor);
      p.set_Z((Z[i]-Zoffset)/Zscalefactor);
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
      /*if(R.length() > 0)
      {
        liblas::Color color(numeric_cast<uint32_t>(R[i]), numeric_cast<uint32_t>(G[i]), numeric_cast<uint32_t>(B[i]));
        p.SetColor(color);
      }*/

      laswriter->write_point(&p);
      laswriter->update_inventory(&p);
    }

    laswriter->update_header(&header, TRUE);
    I64 total_bytes = laswriter->close();
  }
  catch (std::exception const& e)
  {
    Rcerr << "Error: " << e.what() << std::endl;
  }
}