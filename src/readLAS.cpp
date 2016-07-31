#include <Rcpp.h>
#include <liblas/reader.hpp>
#include <exception>
#include <fstream>

using namespace Rcpp;

//' Read a .las file
//'
//' Methods to read .las files
//' @param LASfile character. filename of .las file
//' @param fields character. \code{"minimal"}, \code{"standard"}, \code{"all"}.
//' @return A data.table
//' @export readLAS
// [[Rcpp::export]]
List readLAS(CharacterVector file,
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

    std::ifstream ifs(filestd.c_str(), std::ios::in | std::ios::binary);

    liblas::Reader reader(ifs);
    liblas::Point p;

    boost::uint32_t n = reader.GetHeader().GetPointRecordsCount();
    liblas::Header header = reader.GetHeader();

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
    while (reader.ReadNextPoint())
    {
      p = reader.GetPoint();

      X[i]   = p.GetX();
      Y[i]   = p.GetY();
      Z[i]   = p.GetZ();
      I[i]   = p.GetIntensity();
      RN[i]  = p.GetReturnNumber();
      NoR[i] = p.GetNumberOfReturns();
      SDF[i] = p.GetScanFlags();
      EoF[i] = p.GetFlightLineEdge();
      C[i]   = p.GetClassification().GetClass();
      SA[i]  = p.GetScanAngleRank();
      UD[i]  = p.GetUserData();
      PSI[i] = p.GetPointSourceID();
      T[i]   = p.GetTime();
      R[i]   = p.GetColor().GetRed();
      G[i]   = p.GetColor().GetGreen();
      B[i]   = p.GetColor().GetBlue();

      i++;
    }

    ifs.close();

    List lasdata = List::create(X,Y,Z);
    CharacterVector field(0);
    field.push_back("X");
    field.push_back("Y");
    field.push_back("Z");

    if(Intensity)
      lasdata.push_back(I), field.push_back("Intensity");
    if(ReturnNumber)
      lasdata.push_back(RN), field.push_back("ReturnNumber");
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
    if(RGB && header.GetDataFormatId() == liblas::ePointFormat2 | header.GetDataFormatId() == liblas::ePointFormat3)
    {
      lasdata.push_back(R), field.push_back("R");
      lasdata.push_back(G), field.push_back("G");
      lasdata.push_back(B), field.push_back("B");
    }
    if(header.GetDataFormatId() == liblas::ePointFormat1 | header.GetDataFormatId() == liblas::ePointFormat3)
      lasdata.push_back(T); field.push_back("gpstime");

    lasdata.names() = field;

    List head(0);
    head.push_back(header.GetFileSignature());
    head.push_back(header.GetFileSourceId());
    head.push_back(0);
    head.push_back(header.GetProjectId().to_string());
    head.push_back(header.GetVersionMajor());
    head.push_back(header.GetVersionMinor());
    head.push_back(header.GetSystemId());
    head.push_back(header.GetSoftwareId());
    head.push_back(header.GetCreationDOY());
    head.push_back(header.GetCreationYear());
    head.push_back(header.GetHeaderSize());
    head.push_back(header.GetDataOffset());
    head.push_back(header.GetRecordsCount());
    head.push_back((int)header.GetDataFormatId());
    head.push_back(header.GetDataRecordLength());
    head.push_back(header.GetPointRecordsCount());
    head.push_back(header.GetPointRecordsByReturnCount());
    head.push_back(header.GetScaleX());
    head.push_back(header.GetScaleY());
    head.push_back(header.GetScaleZ());
    head.push_back(header.GetOffsetX());
    head.push_back(header.GetOffsetY());
    head.push_back(header.GetOffsetZ());
    head.push_back(header.GetMaxX());
    head.push_back(header.GetMinX());
    head.push_back(header.GetMaxY());
    head.push_back(header.GetMinX());
    head.push_back(header.GetMaxZ());
    head.push_back(header.GetMinZ());

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
    names.push_back("Number of points by return");
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

    return(List::create(head, lasdata));
  }
  catch (std::exception const& e)
  {
    Rcerr << "Error: " << e.what() << std::endl;
    return(List(0));
  }
}