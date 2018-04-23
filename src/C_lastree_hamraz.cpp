#include <boost/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/polygon.hpp>
#include <Rcpp.h>
#include "Point.h"


using namespace Rcpp;

typedef boost::geometry::model::d2::point_xy<double> polygonExtremityPoint, profileExtremities;
typedef boost::geometry::model::polygon<polygonExtremityPoint, false, true> polygonCCW;

//Function headers
void extractDataToVec2D( const S4 disc, std::vector<PointXYZR*> &points );
void createCornerCoordinates( const PointXYZ* &center, const double &radius, const double &widthProfile, std::vector<PointXYZ*> &coordRef );
void initialAngleValuesForProfileEvaluation( const int &nbProfiles, const double &init_alpha, std::vector<double> &angle );
void nextAngleValuesForProfileEvaluation( const int &generatedAngleValues, const int &nbProfiles, const double &init_alpha, std::vector<double> &angle );
void generateProfile_vec( const std::vector<PointXYZR*> &points, const std::vector<PointRTZ*> &coordCylindRef, const double &angleProfile, const PointXYZ* &center, std::vector<PointXYZR*> &subProfileRight, std::vector<PointXYZR*> &subProfileLeft );
double calculateMedian(std::vector<double> array);
double  calculateIQR( std::vector<double> values );
void findGap_vec( std::vector<PointXYZR*> &subProfile, std::vector<PointXYZR*> &output, double sensitivity = 6 );
void findLM_vec( std::vector<PointXYZR*> &subProfile, const PointXYZ* &center, std::vector<int> &localMinPosition );
void keepDataUnderDistanceValue( std::vector<PointRTZ*> &subProfile, double limit, std::vector<PointRTZ*> &subProfileSubset );
void calculateSteepness( std::vector<PointRTZ*> &subProfile, double &steepnessValue);
void findBoundaries_vec( std::vector<PointXYZR*> &subProfile, const PointXYZ* &center, double &CLc, double &CLs, double &Oc, double &Os, double &epsilon, double &angleRefCone, double &angleRefSphere, double &MDCW, std::vector<PointXYZR*> &profileSubset );
void findMaxDistToMax( const std::vector<PointXYZR*> &subProfile, PointXYZR* &maxVector );
void createPolygonFromExtremities( const std::vector<PointXYZR*> &points, std::vector<PointXYZR*> &extremityPoints, const PointXYZ* &center, std::vector<double> &pointsInsidePolygon );

//TODO: find better function than findMaxTo...



//----------------------------------------------------------------------------------------//
// [[Rcpp::export]]
std::vector<double> find_tree_polygon_vec ( S4 disc, double nbPoints, double nps, int SENSITIVITY, double MDCW, double epsilon,
                                      double CLc, double CLs, double Oc, double Os, double angleRefCone,
                                      double angleRefSphere, std::vector<double> centerRef, double radius )
{
  //Data import into vector of PointXYZR
  double widthProfile = 2 * nps;
  std::vector<PointXYZR*> points( nbPoints );
  extractDataToVec2D( disc, points );

  //Creation of profile reference corners for later angle rotation
  const PointXYZ* center = new PointXYZ( centerRef[0], centerRef[1], centerRef[2] );
  std::vector<PointXYZ*> coordRef( 6 );
  createCornerCoordinates( center, radius, widthProfile, coordRef );
  //Change into cylindrical coordinates
  std::vector<PointRTZ*> coordCylindRef( 6 );
  cart2pol_vec( coordRef, center, coordCylindRef );


  double chord = 1000;      //--> TODO: chercher à quelle valeur la fixer initialement??
  //mettre chord en argument de la fonction
  int nbProfiles = 2, generatedAngleValues = 0, angleProfile = 0;
  double alpha = 90, rmax = 0;
  std::vector<double> angle, rmaxSelection, pointsInsidePolygon;
  std::vector<PointXYZR*> subProfileRight, subProfileLeft;
  std::vector<PointXYZR*> subProfileRight_withoutGap, subProfileLeft_withoutGap;
  std::vector<PointXYZR*> subProfileRight_withoutBoundaries, subProfileLeft_withoutBoundaries;
  std::vector<PointXYZR*> profileExtremityStorage;
  PointXYZR *maxVectorRight, *maxVectorLeft;

  while ( chord > nps )
  {
    alpha /= 2;
    nbProfiles *= 2;
    angle.clear();

    if (nbProfiles == 4 )
    {
    initialAngleValuesForProfileEvaluation( nbProfiles, alpha, angle );
    generatedAngleValues = angle.size();
    }
    else
    {
    nextAngleValuesForProfileEvaluation( generatedAngleValues, nbProfiles, alpha, angle );
    generatedAngleValues += angle.size();
    }

    rmaxSelection.clear();
    for ( int a = 0; a < angle.size(); a++ )
    {
      //Storage of points in each profile
      subProfileRight.clear();
      subProfileLeft.clear();
      angleProfile = angle[a];
      //angleProfile = angle[0];
      generateProfile_vec( points, coordCylindRef, angleProfile, center, subProfileRight, subProfileLeft );

      //1 - Gap identification by statistical analysis --> horiz. square dist calculation
      subProfileRight_withoutGap.clear();
      subProfileLeft_withoutGap.clear();
      findGap_vec( subProfileRight, subProfileRight_withoutGap, SENSITIVITY );
      findGap_vec( subProfileLeft, subProfileLeft_withoutGap, SENSITIVITY );

      //2 - Boundaries identification
      subProfileRight_withoutBoundaries.clear();
      subProfileLeft_withoutBoundaries.clear();
      findBoundaries_vec( subProfileRight_withoutGap, center, CLc, CLs, Oc, Os, epsilon, angleRefCone, angleRefSphere, MDCW, subProfileRight_withoutBoundaries );
      findBoundaries_vec( subProfileLeft_withoutGap, center, CLc, CLs, Oc, Os, epsilon, angleRefCone, angleRefSphere, MDCW, subProfileLeft_withoutBoundaries );

      //If only apex point remains, set maximal radius at 0
      if (subProfileRight_withoutBoundaries.size() <= 1 && subProfileLeft_withoutBoundaries.size() <= 1 )
      {
        rmaxSelection.push_back( 0 );
      }
      else
      {
        //Storage of maximal radius in selection of profiles
        //search for extremities of each profile
        findMaxDistToMax( subProfileRight_withoutBoundaries, maxVectorRight );
        findMaxDistToMax( subProfileLeft_withoutBoundaries, maxVectorLeft );
        double maxValueRight = maxVectorRight->r, maxValueLeft = maxVectorLeft->r;
        rmaxSelection.push_back( maxValueRight > maxValueLeft ? maxValueRight : maxValueLeft );

        //Storage of extremities --> if only apex point in profile part, set to INT16_MIN
        if( maxValueRight != INT16_MIN )
        {
          profileExtremityStorage.push_back( maxVectorRight );
        }
        if( maxValueLeft != INT16_MIN )
        {
          profileExtremityStorage.push_back( maxVectorLeft );
        }
      }
    }

    //Selection of maximal radius
    rmax = *std::max_element( rmaxSelection.begin(), rmaxSelection.end() );

    //Calculation of chord value with selected maximal radius
    chord = 2 * rmax * sin((alpha * (PI / 180)) /2);
  }

  pointsInsidePolygon.clear();
  //Definition of polygon using extremity coordinates
  if(chord != 0)
  {
    createPolygonFromExtremities( points, profileExtremityStorage, center, pointsInsidePolygon );
  }
  else //If only apex point in profile
  {
    pointsInsidePolygon.push_back(centerRef[3]);
  }
  return ( pointsInsidePolygon );
}


//----------------------------------------------------------------------------------------//
//Function that converts data from LAS object into vector of PointXYZR
//disc = las object
//points = output vector of PointXYZR
void extractDataToVec2D( const S4 disc, std::vector<PointXYZR*> &points )
{
  DataFrame data = as<Rcpp::DataFrame>(disc.slot("data"));
  NumericVector X = data["X"];
  NumericVector Y = data["Y"];
  NumericVector Z = data["Z"];
  IntegerVector ID = data["pointID"];
  NumericVector Dist = data["distToMax"];

  for ( int i = 0 ; i < X.size() ; i++ )
    points[i] = new PointXYZR(X[i], Y[i], Z[i], ID[i], Dist[i]);
}


//----------------------------------------------------------------------------------------//
//Function that calculates coordinates of profile extremities around a given point
// (profile = rectangle with point as center, 2*radius as length and 2* widthProfile as width)
//center = profile center
//radius = profile length
//widthProfile = profile width
//coordRef = vector of PointXYZ containing profile extremity coordinates
void createCornerCoordinates( const PointXYZ* &center, const double &radius,
                              const double &widthProfile, std::vector<PointXYZ*> &coordRef )
{
  //Coordinates of limits in following order: RightUp, RightDown, LeftUp, LeftDown, MiddleUp, MiddleDown
  coordRef[0] = new PointXYZ(center->x + radius, center->y + (widthProfile/2));  //RU
  coordRef[1] = new PointXYZ(center->x + radius, center->y - (widthProfile/2));  //RD
  coordRef[2] = new PointXYZ(center->x - radius, center->y + (widthProfile/2));  //LU
  coordRef[3] = new PointXYZ(center->x - radius, center->y - (widthProfile/2));  //LD
  coordRef[4] = new PointXYZ(center->x, center->y + (widthProfile/2));           //MU
  coordRef[5] = new PointXYZ(center->x, center->y - (widthProfile/2));           //MD
}


//----------------------------------------------------------------------------------------//
//Increment structure to create list of increasing values starting with m_value
struct Increment {
  Increment() : m_value( 0 ) { }
  int operator()() { return m_value++; }
  int m_value;
};


//----------------------------------------------------------------------------------------//
//Function that calculates angle values to use for profile evaluation in the first loop
// Angles : 0 45 90 135
void initialAngleValuesForProfileEvaluation( const int &nbProfiles, const double &init_alpha, std::vector<double> &angle )
{
  angle.assign( nbProfiles, 0 );
  std::generate( angle.begin(), angle.end(), Increment() );
  std::transform ( angle.begin(), angle.end(), angle.begin(), std::bind1st(std::multiplies<double>(), init_alpha) );
}


//----------------------------------------------------------------------------------------//
//Function that calculates angle values to use for profile evaluation in the next loops
//The profile number is multiplied by two and alpha angle is divided by two
//Example for second loop : 22.5 67.5 112.5 157.5 (First loop: 0 45 90 135)

void nextAngleValuesForProfileEvaluation( const int &generatedAngleValues, const int &nbProfiles, const double &init_alpha, std::vector<double> &angle )
{
  angle.assign( nbProfiles - generatedAngleValues, init_alpha );
  std::vector<double> tmp ( nbProfiles - generatedAngleValues, 0 );
  std::generate( tmp.begin(), tmp.end(), Increment() );
  std::transform ( tmp.begin(), tmp.end(), tmp.begin(), std::bind1st(std::multiplies<double>(), init_alpha*2) );
  std::transform ( angle.begin(), angle.end(), tmp.begin(), angle.begin(), std::plus<double>() );
}


//----------------------------------------------------------------------------------------//
//Function that extract points located inside a profile
//(center specified by point coordinates and rotation defined by an angle)
//points = converted data from las object (vector of PointXYZR)
//coordCylindRef = coordinates of profile extremities at angle 0°
//angleProfile = rotation angle for profile around center
//center = point coordinates for center of profile
//subProfileRight = vector of points located in right part of profile
//subProfileLeft = vector of points located in left part of profile
void generateProfile_vec( const std::vector<PointXYZR*> &points,
                          const std::vector<PointRTZ*> &coordCylindRef,
                          const double &angleProfile, const PointXYZ* &center,
                          std::vector<PointXYZR*> &subProfileRight,
                          std::vector<PointXYZR*> &subProfileLeft )
{
  if ( points.size() > 1 )
  {
    //Addition of angle rotation to reference rectangle of profile
    std::vector<PointRTZ*> coordCylindLimit( coordCylindRef.size() );
    for ( int i = 0; i < coordCylindRef.size(); i++ )
    {
      coordCylindLimit[i] = new PointRTZ(coordCylindRef[i]->r,
                                         coordCylindRef[i]->t + (angleProfile * PI / 180));
    }
    std::vector<PointXYZ*> coordLimit( 6 );
    pol2cart_vec( coordCylindLimit, center, coordLimit );

    //Addition of angle value for trigonometric ordered points in following polygon definition
    for (int i = 0; i < coordLimit.size(); i++ )
    {
      //In this case Z in PointXYZ stores the angle value and not the height!!
      coordLimit[i]->z = atan2(coordLimit[i]->y - center->y, coordLimit[i]->x - center->x);
    }

    //Subdivision of profile coordinates into right and left profile (separation at center point)
    std::vector<PointXYZ*> coord_right( 4 ), coord_left( 4 );
    int indR = 0, indL = 0;
    for ( int i = 0; i < coordLimit.size(); i++ )
    {
      if( i < 4 )
      {
        if( i < 2 )
        {
          coord_right[i-indR] = coordLimit[i];
          indL++;
        }
        else
        {
          coord_left[i-indL] = coordLimit[i];
          indR++;
        }
      }
      else
      {
        coord_right[i-indR] = coordLimit[i];
        coord_left[i-indL] = coordLimit[i];
      }
    }

    //(To change if new Point definition with angle value)
    std::sort( coord_right.begin(), coord_right.end(), ZSortPoint_increasing() );
    std::sort( coord_left.begin(), coord_left.end(), ZSortPoint_increasing() );

    //attribution of coordinates for right and left part to two polygons
    polygonCCW polyR, polyL;
    std::vector<polygonExtremityPoint> pointsR(coordLimit.size()-1), pointsL(coordLimit.size()-1);

    for ( int i = 0 ; i < coord_right.size() ; i++ )
    {
      pointsR[i] = polygonExtremityPoint(coord_right[i]->x, coord_right[i]->y);
      pointsL[i] = polygonExtremityPoint(coord_left[i]->x, coord_left[i]->y);
    }
    //First point is added again at the end to close polygon
    pointsR.at(coord_right.size()) = polygonExtremityPoint(coord_right[0]->x, coord_right[0]->y);
    pointsL.at(coord_left.size()) = polygonExtremityPoint(coord_left[0]->x, coord_left[0]->y);
    //std::reverse(pointsR.begin(), pointsR.end());

    //Assign points to polygons
    boost::geometry::assign_points(polyR, pointsR);
    boost::geometry::assign_points(polyL, pointsL);

    //search for points of data inside each polygon
    //--> creation of vector of PointXYZZ for their storage (subProfileRight and Left)
    polygonExtremityPoint p;
    for ( int i = 0; i < points.size(); i++ )
    {
      boost::geometry::set<0>(p, points[i]->x);
      boost::geometry::set<1>(p, points[i]->y);
      if (boost::geometry::covered_by(p, polyR)  == TRUE)
      {
        subProfileRight.push_back(points[i]);
      }
      if( boost::geometry::covered_by(p, polyL) == TRUE)
      {
        subProfileLeft.push_back(points[i]);
      }
    }
  }
  else
  {
    subProfileLeft.assign(points.begin(), points.end() );
  }
}


//----------------------------------------------------------------------------------------//
//Function that returns median value of a double vector
double calculateMedian(std::vector<double> array)
{
  std::sort( array.begin(), array.end(), std::less<double>());
  double median;
  int middle = array.size() / 2;

  if (array.size() % 2 == 0)
  {
    median = (array[middle-1] + array[middle]) / 2.0;
  }
  else
  {
    median = array[middle];
  }
  return median;
}


//----------------------------------------------------------------------------------------//
//Interquartile value calculation of a vector of double
//1.Search for median value and subdivision in two parts (above and beyond median)
//2.Search of median value in both parts (--> Q1 and Q3)
//3. IQR = Q3-Q1
double  calculateIQR( std::vector<double> values )
{
  std::sort( values.begin(), values.end(), std::less<double>());

  int ind = 1;
  if ( values.size() % 2 == 0) { ind = 0; }

  std::vector<double> split_lo( values.begin(), values.begin() + (values.size()/2) + ind );
  std::vector<double> split_hi( values.begin() + values.size()/2, values.end() );

  double Q1 = calculateMedian( split_lo );
  double Q3 = calculateMedian( split_hi );
  double IQR = Q3 - Q1;
  return (IQR);
}


//----------------------------------------------------------------------------------------//
//Function that cut profile if gap is identified
//1.Horizontal distance calculation between each points
//2.Elimination of points with distance greater than sensitivity * IQR value
//subProfile = vector of PointXYZR (right or left part of profile)
//output = remaining points after distance calculation
void findGap_vec( std::vector<PointXYZR*> &subProfile, std::vector<PointXYZR*> &output, double sensitivity )
{
  if ( subProfile.size() > 1 )
  {
    //Euclidian distance calculation between center and other profile points (horiz. square dist)
    std::sort( subProfile.begin(), subProfile.end(), RSortPoint() );
    std::vector<double> distance( subProfile.size() - 1, 0 );
    std::transform( subProfile.begin(), subProfile.end() - 1, subProfile.begin()+1, distance.begin(), EuclidianDistance<PointXYZR*>() );


    //Gap identification if dist > (sensitivity * interquantile range from Q3)
    double IQR = calculateIQR( distance );
    int a = 0;
    while( distance[a] <= sensitivity * IQR )
    {
      a++;
      if ( a==distance.size() ){break;}
    }
    output.assign( subProfile.begin(), subProfile.begin() + a+1 );
  }
  else
  {
    output.assign( subProfile.begin(), subProfile.end() );
  }
}


//----------------------------------------------------------------------------------------//
//Function to find local minima for points located in subProfile
void findLM_vec( std::vector<PointXYZR*> &subProfile, const PointXYZ* &center, std::vector<int> &localMinPosition )
{
  if ( subProfile.size() > 1 )
  {
    std::sort( subProfile.begin(), subProfile.end(), RSortPoint() );
    std::vector<PointRTZ*> subProfile_cylind( subProfile.size() );
    cart2pol_vec( subProfile, center, subProfile_cylind );

    //Slope calculation
    std::vector<double> slope( subProfile_cylind.size() - 1, 0 );
    std::transform( subProfile_cylind.begin(), subProfile_cylind.end() - 1, subProfile_cylind.begin()+1, slope.begin(), SlopeInCylindricalReferenceSystem<PointRTZ*>() );

    for (int i = 0; i < slope.size(); i++ )
    {
      if( slope[i] > 0 )
      {
        if( i >= 1 && slope[i-1] < 0 )
        {
          localMinPosition.push_back( i );
        }
        else if (i == 0)
        {
          localMinPosition.push_back( i );
        }
      }
    }

    if( localMinPosition.empty() == TRUE )
    {
      localMinPosition.push_back( INT16_MIN );
    }
  }
  else
  {
    localMinPosition.push_back(INT16_MIN);
  }
}


//----------------------------------------------------------------------------------------//
void keepDataUnderDistanceValue( std::vector<PointRTZ*> &subProfile, double limit, std::vector<PointRTZ*> &subProfileSubset )
{
  int i = 1, keep = 0;
  while ( (i < subProfile.size()) && ((subProfile[i]->r - subProfile[0]->r) <= limit) )
  {
    keep = i++;
  }
  subProfileSubset.assign(subProfile.begin(), subProfile.begin() + keep + 1 );
}


//----------------------------------------------------------------------------------------//
//Steepness calculation
void calculateSteepness( std::vector<PointRTZ*> &subProfile, double &steepnessValue)
{
  std::vector<double> slope(subProfile.size(), 0 );
  std::transform( subProfile.begin(), subProfile.end() - 1, subProfile.begin()+1, slope.begin(), SlopeInCylindricalReferenceSystem<PointRTZ*>() );
  steepnessValue = atan(calculateMedian( slope )) * (180/PI);
}


//----------------------------------------------------------------------------------------//
void findBoundaries_vec( std::vector<PointXYZR*> &subProfile, const PointXYZ* &center, double &CLc, double &CLs, double &Oc, double &Os, double &epsilon, double &angleRefCone, double &angleRefSphere, double &MDCW, std::vector<PointXYZR*> &profileSubset )
{
  if ( subProfile.size() > 1 )
  {
    //Search for local minima in considered profile part
    std::vector<int> localMinPositions;
    std::sort( subProfile.begin(), subProfile.end(), RSortPoint() );
    findLM_vec( subProfile, center, localMinPositions );

    std::vector<PointRTZ*> subProfile_cylind( subProfile.size() );
    double S_right_final, S_left_final, S_right;
    std::vector<PointRTZ*> split_right_LocalMin, split_left_LocalMin, right_LocalMin_at_MDCW, right_LocalMin_at_wRD;
    if ( localMinPositions[0] != INT16_MIN)
    {
      //Conversion into cylindrical coordinates
      cart2pol_vec( subProfile, center, subProfile_cylind );
      int p = 0;
      S_right_final = 0, S_left_final = 0, S_right = 0;
      do
      {
        //Storage of profile values from center (apex) to local minimum n°p
        split_left_LocalMin.assign(subProfile_cylind.begin(), subProfile_cylind.begin() + localMinPositions[p] + 1 );
        //Storage of profile values after local minimum n°p
        split_right_LocalMin.assign(subProfile_cylind.begin() + localMinPositions[p], subProfile_cylind.end());

        p++;

        //Suppression in this last vector of points at a distance greater than MDCW
        if ( split_right_LocalMin.size() > 1 )
        {
          keepDataUnderDistanceValue( split_right_LocalMin, MDCW, right_LocalMin_at_MDCW );
        }
        else { break; }

        //If only LM remains after filtering --> break
        if ( right_LocalMin_at_MDCW.size() == 1 ) { break; }

        //cone-shaped crown radius (crc)
        double h_ad = (subProfile_cylind[0]->z + right_LocalMin_at_MDCW[0]->z) / 2;      //mean value between center- and considered LM height
        double crc = ( (h_ad * CLc) / (tan((angleRefCone-epsilon) * PI / 180.0)) ) * Oc;
        //sphere-shaped crown radius
        double crs = ( (h_ad * CLs) / 2 ) * Os;

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

      } while ( p < localMinPositions.size() && (S_right_final <= 0 || S_left_final >= 0) );

      if ( (S_right_final > 0 && S_left_final < 0) )
      {
        profileSubset.assign( subProfile.begin(), subProfile.begin() + localMinPositions[p - 1] + 1 );
      }
      //TODO: VERIFIER CETTE CONDITION!!!
      else if ( right_LocalMin_at_MDCW.size() == 1 || right_LocalMin_at_wRD.size() == 1 )
      {
        profileSubset.assign( subProfile.begin(), subProfile.begin() + localMinPositions[p - 1]  );
      }
      else
      {
        profileSubset.assign( subProfile.begin(), subProfile.end() );
      }
    }
    else
    {
      profileSubset.assign( subProfile.begin(), subProfile.end() );
    }
  }
  else
  {
    profileSubset.assign( subProfile.begin(), subProfile.end() );
  }
}


//----------------------------------------------------------------------------------------//
void findMaxDistToMax( const std::vector<PointXYZR*> &subProfile, PointXYZR* &maxVector )
{
  if ( subProfile.size() > 1 )
  {
    double tmp;
    double maxValue = subProfile[0]->r;
    for ( int i = 1; i < subProfile.size(); i++ )
    {
      tmp = subProfile[i]->r;
      if ( tmp > maxValue )
      {
        maxValue = tmp;
        maxVector = subProfile[i];
      }
    }
  }
  else
  {
    maxVector = new PointXYZR( INT16_MIN, INT16_MIN );
  }
}


//----------------------------------------------------------------------------------------//
void createPolygonFromExtremities( const std::vector<PointXYZR*> &points, std::vector<PointXYZR*> &extremityPoints, const PointXYZ* &center, std::vector<double> &pointsInsidePolygon )
{
  //pour éviter les doublons dans les extrémités
  sort( extremityPoints.begin(), extremityPoints.end() );
  extremityPoints.erase( unique( extremityPoints.begin(), extremityPoints.end() ), extremityPoints.end() );
  std::vector<PointXYZR*> filteredExtremityPoints;
  for (int i = 0; i < extremityPoints.size(); i++ )
  {
    if ( extremityPoints[i]->x != INT16_MIN )
    {
      filteredExtremityPoints.push_back( extremityPoints[i] );
    }
  }

  pointsInsidePolygon.clear();
  if ( filteredExtremityPoints.size() >  2 )
  {
    for (int i = 0; i < filteredExtremityPoints.size(); i++ )
    {
      //z of PointXYZR is used here to store angle values
      filteredExtremityPoints[i]->z = atan2(filteredExtremityPoints[i]->y - center->y, filteredExtremityPoints[i]->x - center->x);
    }
    std::sort( filteredExtremityPoints.begin(), filteredExtremityPoints.end(), ZRSortPoint_increasing() );

    std::vector<polygonExtremityPoint> pointsExtremityForPolygon(filteredExtremityPoints.size() + 1);

    for ( int i = 0; i < filteredExtremityPoints.size(); i++ )
    {
      pointsExtremityForPolygon[i] = polygonExtremityPoint(filteredExtremityPoints[i]->x, filteredExtremityPoints[i]->y);
    }
    pointsExtremityForPolygon[filteredExtremityPoints.size()] = polygonExtremityPoint(filteredExtremityPoints[0]->x, filteredExtremityPoints[0]->y);

    polygonCCW extremityPolygon;
    boost::geometry::assign_points(extremityPolygon, pointsExtremityForPolygon);

    polygonExtremityPoint p;
    for ( int i = 0; i < points.size(); i++ )
    {
      boost::geometry::set<0>(p, points[i]->x);
      boost::geometry::set<1>(p, points[i]->y);
      if (boost::geometry::covered_by(p, extremityPolygon)  == TRUE)
      {
        pointsInsidePolygon.push_back(points[i]->id);
      }
    }

  }
  else
  {
    for (int i = 0; i < filteredExtremityPoints.size(); i++ )
    {
      pointsInsidePolygon.push_back( filteredExtremityPoints[i]->id );
    }
  }
}