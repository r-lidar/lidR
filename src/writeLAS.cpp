#include <Rcpp.h>
#include <liblas/writer.hpp>
#include <exception>
#include <fstream>

using namespace Rcpp;
using namespace boost;

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

    std::ofstream ofs(filestd.c_str(), std::ios::out | std::ios::binary);

    std::string FileSignature = LASheader["File Signature"];
    int FileSourceID = LASheader["File Source ID"];
    int GlobalEncoding = LASheader["Global Encoding"];
    std::string ProjectIDGUID = LASheader["Project ID - GUID"];
    int VersionMajor = LASheader["Version Major"];
    int VersionMinor = LASheader["Version Minor"];
    std::string SystemIdentifier = LASheader["System Identifier"];
    std::string GeneratingSoftware = LASheader["Generating Software"];
    int FileCreationDayofYear = LASheader["File Creation Day of Year"];
    int FileCreationYear = LASheader["File Creation Year"];
    int HeaderSize = LASheader["Header Size"];
    double Offsettopointdata = LASheader["Offset to point data"];
    double Numberofvariablelengthrecords = LASheader["Number of variable length records"];
    int PointDataFormatID = LASheader["Point Data Format ID"];
    int PointDataRecordLength = LASheader["Point Data Record Length"];
    double Numberofpointrecords = LASheader["Number of point records"];
    //double Numberofpointsbyreturn = LASheader["Number of points by return"];
    double Xscalefactor = LASheader["X scale factor"];
    double Yscalefactor = LASheader["Y scale factor"];
    double Zscalefactor = LASheader["Z scale factor"];
    double Xoffset = LASheader["X offset"];
    double Yoffset = LASheader["Y offset"];
    double Zoffset = LASheader["Z offset"];
    double MaxX = LASheader["Max X"];
    double MinX = LASheader["Min X"];
    double MaxY = LASheader["Max Y"];
    double MinY = LASheader["Min Y"];
    double MaxZ = LASheader["Max Z"];
    double MinZ = LASheader["Min Z"];
    uint16_t offset = 227;

    liblas::Header header;

    //header.SetFileSignature(as<std::string>(LASheader["File Signature"]));
    header.SetFileSourceId(numeric_cast<uint16_t>(FileSourceID));
    //
    //header.SetProjectId();
    header.SetVersionMajor(numeric_cast<uint8_t>(VersionMajor));
    header.SetVersionMinor(numeric_cast<uint8_t>(VersionMinor));
    //header.SetSystemId();
    //header.SetSoftwareId();
    header.SetCreationDOY(numeric_cast<uint16_t>(FileCreationDayofYear));
    header.SetCreationYear(numeric_cast<uint16_t>(FileCreationYear));
    header.SetHeaderSize(offset);
    header.SetDataOffset(offset);
    header.SetRecordsCount(numeric_cast<uint16_t>(Numberofvariablelengthrecords));
    header.SetDataFormatId(liblas::ePointFormat1);
    //header.SetDataRecordLength();
    header.SetPointRecordsCount(numeric_cast<uint32_t>(X.length())); // WARNING
    //header.SetPointRecordsByReturnCount();
    header.SetScale(Xscalefactor, Yscalefactor, Zscalefactor);
    header.SetOffset(Xoffset, Yoffset, Zoffset);
    header.SetMax(MaxX, MaxY, MaxZ);
    header.SetMin(MinX, MinY, MinZ);

    liblas::Writer writer(ofs, header);

    for(int i = 0 ; i < X.length() ; i++)
    {
      liblas::Point p;
      liblas::Classification cl;
      liblas::Color color;

      p.SetRawX((X[i]-Xoffset)/Xscalefactor);
      p.SetRawY((Y[i]-Yoffset)/Yscalefactor);
      p.SetRawZ((Z[i]-Zoffset)/Zscalefactor);

      if(I.length() > 0){ p.SetIntensity(numeric_cast<uint16_t>(I[i])); }
      if(RN.length() > 0){ p.SetReturnNumber(numeric_cast<uint16_t>(RN[i])); }
      if(NoR.length() > 0){ p.SetNumberOfReturns(numeric_cast<uint16_t>(NoR[i])); }
      if(SDF.length() > 0){ p.SetScanFlags(numeric_cast<uint8_t>(NoR[i])); }
      if(EoF.length() > 0){ p.SetFlightLineEdge(numeric_cast<uint8_t>(EoF[i])); }
      if(C.length() > 0){ p.SetClassification(numeric_cast<uint8_t>(C[i])); }
      if(SA.length() > 0){ p.SetScanAngleRank(numeric_cast<int8_t>(SA[i])); }
      if(UD.length() > 0){ p.SetUserData(numeric_cast<uint8_t>(UD[i])); }
      //if(PSI.length() > 0){ p.SetPointSourceID(numeric_cast<uint16_t>(PSI[i])); }
      if(T.length() > 0){ p.SetTime(T[i]); }
      if(R.length() > 0)
      {
        liblas::Color color(numeric_cast<uint32_t>(R[i]), numeric_cast<uint32_t>(G[i]), numeric_cast<uint32_t>(B[i]));
        p.SetColor(color);
      }

      writer.WritePoint(p);
    }

    ofs.close();
  }
  catch (std::exception const& e)
  {
    Rcerr << "Error: " << e.what() << std::endl;
  }
}