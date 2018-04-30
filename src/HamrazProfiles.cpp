#include <math.h>
#include <memory>
#include "HamrazProfiles.h"

HZProfile::HZProfile(std::vector<PointXYZR*>& disc, PointXYZ Center, double Angle, double Radius, double Width, int Sensitivity,
                     double MDCW, double Epsilon, double CLc, double CLs, double Oc, double Os, double AngleRefCone,
                     double AngleRefSphere)
{
  angle = Angle * PI / 180;
  center = Center;
  width = Width;
  sensitivity = Sensitivity;
  mdcw = MDCW;
  epsilon = Epsilon;
  clc = CLc;
  cls = CLs;
  oc = Oc;
  os = Os;
  angleRefCone = AngleRefCone;
  angleRefSphere = AngleRefSphere;
  extremityPoint = PointXYZR( 0, 0, 0, 0, 0);

  extract_profile(disc);
  find_gap();
  find_local_minima();
  find_boundary();
  find_extremities();
}

HZProfile::~HZProfile()
{
}

void HZProfile::extract_profile(std::vector<PointXYZR*>& disc)
{
  double cosAngle = std::cos(angle);
  double sinAngle = std::sin(angle);
  for(int i = 0 ; i < disc.size() ; i++)
  {
    PointXYZR* p = disc[i];
    double rot_x = (p->x -center.x) * cosAngle - (center.y - p->y) * sinAngle;
    double rot_y = (center.y - p->y) * cosAngle + (p->x -center.x)  * sinAngle;

    if(rot_x >= 0 && rot_y >= - width && rot_y <= width)
      points.push_back(p);
  }

  std::sort( points.begin(), points.end(), RSortPoint() );
}

void HZProfile::find_gap()
{
  if (points.size() > 1)
  {
    //Euclidian distance calculation between center and other profile points (horiz. square dist)
    std::vector<double> distance( points.size() - 1, 0 );
    std::transform( points.begin(), points.end() - 1, points.begin() + 1, distance.begin(), EuclidianDistance<PointXYZR*>() );

    //Gap identification if dist > (sensitivity * interquantile range from Q3)
    double iqr = IQR(distance);

    int a = 0;
    while( distance[a] <= sensitivity * iqr )
    {
      a++;
      if (a == distance.size())
        break;
    }

    points_no_gaps.assign(points.begin(), points.begin() + a + 1);
  }
  else
  {
    points_no_gaps.assign(points.begin(), points.end());
  }
}

double HZProfile::IQR( std::vector<double> values)
{
  std::sort( values.begin(), values.end(), std::less<double>());

  int ind = 1;
  if ( values.size() % 2 == 0) { ind = 0; }

  std::vector<double> split_lo( values.begin(), values.begin() + (values.size()/2) + ind );
  std::vector<double> split_hi( values.begin() + values.size()/2, values.end() );

  double Q1 = median( split_lo );
  double Q3 = median( split_hi );
  double IQR = Q3 - Q1;

  return (IQR);
}

double HZProfile::median(std::vector<double> array)
{
  std::sort( array.begin(), array.end(), std::less<double>());
  int middle = array.size() / 2;

  if (array.size() % 2 == 0)
    return (array[middle-1] + array[middle]) / 2.0;
  else
    return array[middle];
}

//Function that detects local min by taking two consecutive points P1 and P2 (cylindrical coordinates)
//calculating their slope (P1(Z) - P2(Z)) / (P1(R) - P2(R)) and detecting index where slope becomes positive
void HZProfile::find_local_minima()
{
  if ( points_no_gaps.size() > 1 )
  {
    std::vector<PointRTZ> subProfile_cylind( points_no_gaps.size() );
    cart2pol_vec( points_no_gaps, center, subProfile_cylind );

    //Slope calculation
    std::vector<double> slope( subProfile_cylind.size() - 1, 0 );
    std::transform( subProfile_cylind.begin(), subProfile_cylind.end() - 1, subProfile_cylind.begin()+1, slope.begin(), SlopeInCylindricalReferenceSystem<PointRTZ>() );

    for (int i = 0; i < slope.size(); i++ )
    {
      if( slope[i] > 0 )
      {
        if( i >= 1 && slope[i-1] < 0 )
        {
          localMinimaIndex.push_back( i );
        }
        else if (i == 0)
        {
          localMinimaIndex.push_back( i );
        }
      }
    }

    if( localMinimaIndex.empty() == TRUE )
    {
      localMinimaIndex.push_back( INT16_MIN );
    }
  }
  else
  {
    localMinimaIndex.push_back(INT16_MIN);
  }
}

//search for all RTZpoints that have a R value under 'limit' value and storage into 'subProfileSubset'
void keepDataUnderDistanceValue( std::vector<PointRTZ> &subProfile, double limit, std::vector<PointRTZ> &subProfileSubset )
{
  int i = 1, keep = 0;
  while ( (i < subProfile.size()) && ((subProfile[i].r - subProfile[0].r) <= limit) )
  {
    keep = i++;
  }
  subProfileSubset.assign(subProfile.begin(), subProfile.begin() + keep + 1 );
}

void HZProfile::calculateSteepness( std::vector<PointRTZ> &subProfile, double &steepnessValue)
{
  std::vector<double> slope(subProfile.size(), 0 );
  std::transform( subProfile.begin(), subProfile.end() - 1, subProfile.begin()+1, slope.begin(), SlopeInCylindricalReferenceSystem<PointRTZ>() );
  steepnessValue = atan(median( slope )) * (180/PI);
}


void HZProfile::find_boundary()
{
  if ( points_no_gaps.size() > 1 )
  {
    //Initialisation
    std::vector<PointRTZ> subProfile_cylind( points_no_gaps.size() );
    double S_right_final, S_left_final, S_right;
    std::vector<PointRTZ> split_right_LocalMin, split_left_LocalMin, right_LocalMin_at_MDCW, right_LocalMin_at_wRD;
    if ( *localMinimaIndex.begin() != INT16_MIN)
    {
      //Conversion into cylindrical coordinates
      cart2pol_vec( points_no_gaps, center, subProfile_cylind );
      int p = 0;
      S_right_final = 0, S_left_final = 0, S_right = 0;
      do
      {
        //Storage of profile values from center (apex) to local minimum n°p
        split_left_LocalMin.assign(subProfile_cylind.begin(), subProfile_cylind.begin() + localMinimaIndex[p] + 1 );
        //Storage of profile values after local minimum n°p
        split_right_LocalMin.assign(subProfile_cylind.begin() + localMinimaIndex[p], subProfile_cylind.end());

        p++;

        //Suppression in this last vector of points at a distance greater than MDCW
        if ( split_right_LocalMin.size() > 1 )
        {
          keepDataUnderDistanceValue( split_right_LocalMin, mdcw, right_LocalMin_at_MDCW );
        }
        else { break; }

        //If only LM remains after filtering --> break
        if ( right_LocalMin_at_MDCW.size() == 1 ) { break; }

        //cone-shaped crown radius (crc)
        double h_ad = (subProfile_cylind[0].z + right_LocalMin_at_MDCW[0].z) / 2;      //mean value between center- and considered LM height
        double crc = ( (h_ad * clc) / (tan((angleRefCone-epsilon) * PI / 180.0)) ) * oc;
        //sphere-shaped crown radius
        double crs = ( (h_ad * cls) / 2 ) * os;

        //2 - Slope calculation --> steepness (S)
        calculateSteepness( right_LocalMin_at_MDCW, S_right);

        //size of the right window --> interpolation
        double wRD = crc * (1 - ( ((angleRefCone-epsilon)-S_right) / ((angleRefCone-epsilon)-angleRefSphere) ) ) + crs *  (((angleRefCone-epsilon)-S_right) / ((angleRefCone-epsilon)-angleRefSphere) );
        keepDataUnderDistanceValue( right_LocalMin_at_MDCW, wRD, right_LocalMin_at_wRD );

        //If only LM remains after filtering --> break
        if ( right_LocalMin_at_wRD.size() == 1 ) { break; }

        calculateSteepness( right_LocalMin_at_wRD, S_right_final);
        calculateSteepness( split_left_LocalMin, S_left_final);

        if (S_right_final > 0 && S_left_final < 0) { break; }

      } while ( p < localMinimaIndex.size() && (S_right_final <= 0 || S_left_final >= 0) );

      if ( (S_right_final > 0 && S_left_final < 0) )
      {
        points_no_boundaries.assign( points_no_gaps.begin(), points_no_gaps.begin() + localMinimaIndex[p - 1] + 1 );
      }
      //TODO: VERIFIER CETTE CONDITION!!!
      else if ( right_LocalMin_at_MDCW.size() == 1 || right_LocalMin_at_wRD.size() == 1 )
      {
        points_no_boundaries.assign( points_no_gaps.begin(), points_no_gaps.begin() + localMinimaIndex[p - 1]  );
      }
      else
      {
        points_no_boundaries.assign( points_no_gaps.begin(), points_no_gaps.end() );
      }
    }
    else
    {
      points_no_boundaries.assign( points_no_gaps.begin(), points_no_gaps.end() );
    }
  }
  else
  {
    points_no_boundaries.assign( points_no_gaps.begin(), points_no_gaps.end() );
  }
}



void HZProfile::find_extremities()
{
  extremityPoint =  *points_no_boundaries.back();
}


Rcpp::List HZProfile::to_R()
{
  Rcpp::List L;
  L["angle"] = angle*180/PI;

  Rcpp::NumericMatrix M(points.size(), 5);
  for ( int i = 0; i < points.size(); i++ )
  {
    M(i, 0) = points[i]->x;
    M(i, 1) = points[i]->y;
    M(i, 2) = points[i]->z;
    M(i, 3) = points[i]->id;
    M(i, 4) = points[i]->r;
  }

  L["points"] = M;

  Rcpp::NumericMatrix MM(points_no_gaps.size(), 5);
  for ( int i = 0; i < points_no_gaps.size(); i++ )
  {
    MM(i, 0) = points_no_gaps[i]->x;
    MM(i, 1) = points_no_gaps[i]->y;
    MM(i, 2) = points_no_gaps[i]->z;
    MM(i, 3) = points_no_gaps[i]->id;
    MM(i, 4) = points_no_gaps[i]->r;
  }

  L["points_no_gap"] = MM;

  Rcpp::NumericVector MMM(5);
  MMM(0) = extremityPoint.x;
  MMM(1) = extremityPoint.y;
  MMM(2) = extremityPoint.z;
  MMM(3) = extremityPoint.id;
  MMM(4) = extremityPoint.r;

  L["extremityPoint"] = MMM;

  return L;
}

bool operator<(HZProfile const &a, HZProfile const& b)
{
  return a.angle < b.angle;
}

// ================================================================================================


// ================================================================================================

//JS : Rq -> pour que l'algo concorde avec l'article, il faudrait mettre 8 profiles au lieu de 4 (alpha = 45)
HZProfiles::HZProfiles(std::vector<PointXYZR*>& data, PointXYZ Center, double Radius, double Width, int Sensitivity,
                       double MDCW, double Epsilon, double CLc, double CLs, double Oc, double Os, double AngleRefCone,
                       double AngleRefSphere)
{
  chord = Radius*1.414;
  alpha = 90;
  rmax = 0;
  radius = Radius;
  width = Width;
  sensitivity = Sensitivity;
  center = Center;
  mdcw = MDCW;
  epsilon = Epsilon;
  clc = CLc;
  cls = CLs;
  oc = Oc;
  os = Os;
  angleRefCone = AngleRefCone;
  angleRefSphere = AngleRefSphere;

  for( double a = 0; a < 360; a+=alpha )
  {
    profiles.push_back(HZProfile(data, center, a, radius, width, sensitivity,
                                 mdcw, epsilon, clc, cls, oc, os, angleRefCone,
                                 angleRefSphere));

    if ( profiles.back().extremityPoint.r > rmax )
      rmax = profiles.back().extremityPoint.r;
  }
}

HZProfiles::~HZProfiles()
{
}

void HZProfiles::add_next_profiles(std::vector<PointXYZR*>& data)
{

  size_t s = profiles.size();
  double a1;
  double a2;

  for (int i = 0 ; i < s ; i++)
  {
      a1 = profiles[i].angle * 180 / PI;

      if (i == s-1)
        a2 = 360;
      else
        a2 = profiles[i+1].angle * 180 / PI;

      double a = (a2 - a1)/2 + a1;

      profiles.push_back(HZProfile(data, center, a, radius, width, sensitivity, mdcw, epsilon,
                                   clc, cls, oc, os, angleRefCone, angleRefSphere));

      if ((profiles.back()).extremityPoint.r > rmax )
             rmax = (profiles.back()).extremityPoint.r;
  }

  std::sort(profiles.begin(), profiles.end());

  //JS : J'ai mis des parentheses et rajouté '/2' dans le calcul du chord
  double alpha = profiles[1].angle * (PI / 180);
  chord = 2 * rmax * std::sin(alpha/2);
}

Rcpp::List HZProfiles::to_R()
{
  Rcpp::List L;

  for(int i = 0 ; i < profiles.size() ; i++)
  {
    L.push_back(profiles[i].to_R());
  }

  return L;
}