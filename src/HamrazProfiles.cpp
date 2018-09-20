#include <math.h>
#include <memory>
#include "HamrazProfiles.h"
#include <stdint.h>

namespace Hamraz
{
  Profile::Profile(std::vector<PointXYZR* >& disc, PointXYZR Center, double Angle, double Radius, double Width, int Sensitivity, double MDCW, double Epsilon, double CLc, double CLs, double Oc, double Os)
  {
    angle = Angle * M_PI / 180;
    center = Center;
    width = Width;
    sensitivity = Sensitivity;
    mdcw = MDCW;
    epsilon = Epsilon;
    clc = CLc;
    cls = CLs;
    oc = Oc;
    os = Os;
    extremityPoint = PointXYZR(0, 0, 0, 0, 0);

    extract_profile(disc);  // page 535 section 2.1
    find_inter_tree_gaps(); // page 535 section 2.2.1
    find_local_minima();    // page 535 section 2.2.2
    find_boundary();        // page 535 section 2.2.2
  }

  Profile::~Profile()
  {
  }

  void Profile::extract_profile(std::vector<PointXYZR*>& disc)
  {
    double cosAngle = std::cos(angle);
    double sinAngle = std::sin(angle);
    double hwidth = width/2;

    for(size_t i = 0 ; i < disc.size() ; i++)
    {
      PointXYZR* p = disc[i];
      double rot_x = (p->x -center.x) * cosAngle - (center.y - p->y) * sinAngle;
      double rot_y = (center.y - p->y) * cosAngle + (p->x -center.x)  * sinAngle;

      if(rot_x >= 0 && rot_y >= -hwidth && rot_y <= hwidth)
        points.push_back(p);
    }

    std::sort(points.begin(), points.end(), RSort<PointXYZR>());
  }

  // Section 2.2.1 page 535
  void Profile::find_inter_tree_gaps()
  {
    // If there are less than 4 points in the profile IQR is meaningless. Return the whole profile
    if (points.size() <= 4)
    {
      points_no_gaps.assign(points.begin(), points.end());
      return;
    }

    std::vector<double> distance(points.size()-1, 0);
    for (size_t i = 0 ; i < points.size()-1 ; i++)
      distance[i] = std::sqrt(points[i+1]->r - points[i]->r);

    // Gap identification if dist > (sensitivity * interquantile range from Q3)
    double iqr = IQR(distance);

    unsigned int a = 0;
    while(distance[a] <= sensitivity * iqr)
    {
      a++;
      if (a == distance.size())
        break;
    }

    points_no_gaps.assign(points.begin(), points.begin() + a + 1);
  }

  // Section 2.2.2 page 535
  void Profile::find_local_minima()
  {
    // If there are less than 3 points, LM search is meaningless.
    if (points_no_gaps.size() <= 3)
    {
      localMinimaIndex.push_back(INT32_MIN);
      return;
    }

    double slope = 0;
    double previous_slope = 0;

    for (size_t i = 0; i < points_no_gaps.size() - 1 ; i++ )
    {
      slope = (points[i+1]->z - points[i]->z) / (points[i+1]->r - points[i]->r);

      if( slope > 0 && previous_slope < 0)
        localMinimaIndex.push_back(i);

      previous_slope = slope;
    }

    if(localMinimaIndex.empty())
      localMinimaIndex.push_back(INT32_MIN);
  }

  // Section 2.2.2 page 535
  void Profile::find_boundary()
  {
    if (*localMinimaIndex.begin() == INT32_MIN)
    {
      points_no_boundaries.assign(points_no_gaps.begin(), points_no_gaps.end());
      return;
    }

    // Initialisation
    unsigned int p = 0;
    double S_left = 0;
    double S_right = 0;

    //std::vector<PointXYZR*> subProfile_cylind(points_no_gaps.size());
    std::vector<PointXYZR*> left_windows;
    std::vector<PointXYZR*> right_windows;
    std::vector<PointXYZR*> right_windows_prior_mdcw;
    std::vector<PointXYZR*> right_windows_prior_wrd;

    do
    {
      // Storage of profile values prior and after local minimum p (section 2.2.2 page 535)
      // (the current LM is added in both subsets)
      left_windows.assign(points_no_gaps.begin(), points_no_gaps.begin() + localMinimaIndex[p] + 1);
      right_windows.assign(points_no_gaps.begin() + localMinimaIndex[p], points_no_gaps.end());

      p++;

      // No points in the right section. The LM is the only one possibility. Skip other steps.
      if (right_windows.size() <= 1)
        break;

      // Suppression of points at a distance greater than MDCW from the LM (page 535 before eq. 2)
      extract_points_prior(right_windows, mdcw, right_windows_prior_mdcw);

      // If a single points remains, breaks;
      if (right_windows_prior_mdcw.size() == 1)
        break;

      // Stepness of LSP on the right (eq 2 page 535)
      S_right = steepness(right_windows_prior_mdcw);

      double theta = 90 - epsilon;

      // cone-shaped crown radius (eq 3)
      double h_ad = (points_no_gaps[0]->z + right_windows_prior_mdcw[0]->z) / 2;      //mean value between center - and considered LM height
      double crc = ((h_ad * clc) / (std::tan(theta * M_PI / 180.0))) * oc;

      //sphere-shaped crown radius (eq 4)
      double crs = ( (h_ad * cls) / 2 ) * os;

      //size of the right window --> interpolation (eq 5)
      double m1 = crc * (1 - ((theta - std::abs(S_right)) / (theta - 32.7)));
      double m2 = crs * ((theta - std::abs(S_right)) / (theta - 32.7));
      double wRD = m1 + m2;

      extract_points_prior(right_windows, wRD, right_windows_prior_wrd);

      // If only LM remains after filtering --> break
      if (right_windows_prior_wrd.size() == 1)
        break;

      S_right = steepness(right_windows_prior_wrd);
      S_left = steepness(left_windows);

      //if (angle > 255.9*PI/180 && angle < 256*PI/180)
        //Rcpp::Rcout << "LocalMax: " << p << " Sright = " << S_right << " Sleft = " << S_left << std::endl;

      // page 536 after eq. 5
      if (S_right > 0 && S_left < 0)
        break;

    } while ( p < localMinimaIndex.size() && (S_right <= 0 || S_left >= 0) );


    if (S_right > 0 && S_left < 0)
    {
      // The current LM is the LM
      points_no_boundaries.assign( points_no_gaps.begin(), points_no_gaps.begin() + localMinimaIndex[p - 1] + 1 );
    }
    else if (right_windows_prior_mdcw.size() == 1 || right_windows_prior_wrd.size() == 1)
    {
      // We exit the loop because there was not enought points on the right
      points_no_boundaries.assign( points_no_gaps.begin(), points_no_gaps.begin() + localMinimaIndex[p - 1] + 1 );
    }
    else
    {
      // No limit found -> The whole profile is the tree.
      points_no_boundaries.assign( points_no_gaps.begin(), points_no_gaps.end() );
    }

    extremityPoint = *points_no_boundaries.back();
  }

  double Profile::IQR( std::vector<double> values)
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

  double Profile::median(std::vector<double> array)
  {
    std::sort( array.begin(), array.end(), std::less<double>());
    int middle = array.size() / 2;

    if (array.size() % 2 == 0)
      return (array[middle-1] + array[middle]) / 2.0;
    else
      return array[middle];
  }

  void Profile::extract_points_prior( std::vector<PointXYZR*> &subProfile, double limit, std::vector<PointXYZR*> &subProfileSubset )
  {
    size_t i = 1, keep = 0;
    while ( (i < subProfile.size()) && ((subProfile[i]->r - subProfile[0]->r) <= limit) )
    {
      keep = i++;
    }
    subProfileSubset.assign(subProfile.begin(), subProfile.begin() + keep + 1 );
  }

  double Profile::steepness(std::vector<PointXYZR*> &subProfile)
  {
    std::vector<double> slope(subProfile.size()-1, 0);

    for(size_t i = 0 ; i < subProfile.size() -1 ; i++)
      slope[i] = (subProfile[i+1]->z - subProfile[i]->z) / (subProfile[i+1]->r - subProfile[i]->r);

    return std::atan(median(slope)) * (180/M_PI);
  }

  Rcpp::List Profile::to_R()
  {
    Rcpp::List L;
    L["angle"] = angle*180/M_PI;

    Rcpp::NumericMatrix M(points.size(), 5);
    for (size_t i = 0; i < points.size(); i++ )
    {
      M(i, 0) = points[i]->x;
      M(i, 1) = points[i]->y;
      M(i, 2) = points[i]->z;
      M(i, 3) = points[i]->id;
      M(i, 4) = points[i]->r;
    }

    L["points"] = M;

    Rcpp::NumericMatrix MM(points_no_gaps.size(), 5);
    for (size_t i = 0; i < points_no_gaps.size(); i++ )
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

  bool operator<(Profile const &a, Profile const& b)
  {
    return a.angle < b.angle;
  }

  // ================================================================================================


  // ================================================================================================

  ProfilesManager::ProfilesManager(std::vector<PointXYZR*>& data, PointXYZR Center, double Radius, double Width, int Sensitivity, double MDCW, double Epsilon, double CLc, double CLs, double Oc, double Os)
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

    // Loop over the angle to build the 8 initial profiles
    for(double a = 0 ; a < 360; a += alpha)
    {
      // When a profile is build, the boundary identification is done too
      profiles.push_back(Profile(data, center, a, radius, width, sensitivity, mdcw, epsilon, clc, cls, oc, os));

      // Store the maximum radius found to compute the chord
      if (profiles.back().extremityPoint.r > rmax ) rmax = profiles.back().extremityPoint.r;
    }
  }

  ProfilesManager::~ProfilesManager()
  {
  }

  void ProfilesManager::add_next_profiles(std::vector<PointXYZR*>& data)
  {

    size_t s = profiles.size();
    double a1;
    double a2;

    for (size_t i = 0 ; i < s ; i++)
    {
        a1 = profiles[i].angle * 180 / M_PI;

        if (i == s-1)
          a2 = 360;
        else
          a2 = profiles[i+1].angle * 180 / M_PI;

        double a = (a2 - a1)/2 + a1;

        profiles.push_back(Profile(data, center, a, radius, width, sensitivity, mdcw, epsilon, clc, cls, oc, os));

        if ((profiles.back()).extremityPoint.r > rmax )
          rmax = (profiles.back()).extremityPoint.r;
    }

    std::sort(profiles.begin(), profiles.end());

    //JS : J'ai mis des parentheses et rajouté '/2' dans le calcul du chord
    double alpha = profiles[1].angle;
    chord = 2 * rmax * std::sin(alpha/2);
  }

  std::vector<PointXYZ> ProfilesManager::get_polygon()
  {
    std::vector<PointXYZ> out(profiles.size());

    for (size_t i = 0 ; i < profiles.size() ; i++)
    {
      out[i].x = center.x + profiles[i].extremityPoint.r * std::cos(profiles[i].angle);
      out[i].y = center.y + profiles[i].extremityPoint.r * std::sin(profiles[i].angle);
      out[i].z = profiles[i].extremityPoint.z;
    }

    return out;
  }

  Rcpp::List ProfilesManager::to_R()
  {
    Rcpp::List L;
    Rcpp::List l;

    Rcpp::NumericVector X(profiles.size());
    Rcpp::NumericVector Y(profiles.size());
    Rcpp::NumericVector Z(profiles.size());
    Rcpp::NumericVector R(profiles.size());

    for (size_t i = 0 ; i < profiles.size() ; i++)
    {
      X(i) = center.x + (profiles[i].extremityPoint.r + 0.5*width/2) * std::cos(profiles[i].angle);
      Y(i) = center.y + (profiles[i].extremityPoint.r + 0.5*width/2) * std::sin(profiles[i].angle);
      Z(i) = profiles[i].extremityPoint.z;
      R(i) = profiles[i].extremityPoint.r;
    }

    L["polygon"] = Rcpp::DataFrame::create(Rcpp::Named("X") = X, Rcpp::Named("Y") = Y, Rcpp::Named("Z") = Z, Rcpp::Named("R") = R);

    for(size_t i = 0 ; i < profiles.size() ; i++)
    {
      l.push_back(profiles[i].to_R());
    }

    L["profile"] = l;

    return L;
  }
}