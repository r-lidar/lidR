#ifndef HAMRAZPROFILE_H
#define HAMRAZPROFILE_H

#include <Rcpp.h>
#include "Point.h"

namespace Hamraz
{
  class Profile
  {
    public:
      Profile(std::vector<PointXYZR*>& Data, PointXYZR Center, double Angle, double Radius, double Width, int Sensitivity, double MDCW, double Epsilon, double CLc, double CLs, double Oc, double Os);
      ~Profile();
      Rcpp::List to_R();

    public:
      double angle;
      double width;
      int sensitivity;
      double mdcw;
      double epsilon;
      double clc;
      double cls;
      double oc;
      double os;
      PointXYZR center;
      std::vector<PointXYZR*> points;
      std::vector<PointXYZR*> points_no_gaps;
      std::vector<PointXYZR*> points_no_boundaries;
      PointXYZR extremityPoint;
      std::vector<int> localMinimaIndex;

    private:
      void extract_profile(std::vector<PointXYZR*>& Data);
      void find_inter_tree_gaps();
      void find_boundary();
      void find_local_minima();
      double IQR(std::vector<double>);
      double median(std::vector<double>);
      double steepness(std::vector<PointXYZR*> &subProfile);
      void extract_points_prior(std::vector<PointXYZR*> &subProfile, double limit, std::vector<PointXYZR*> &subProfileSubset);

  };

  bool operator<(Profile const& a, Profile const& b);

  class ProfilesManager
  {
    public:
      ProfilesManager(std::vector<PointXYZR*>& data, PointXYZR Center, double Radius, double Width, int Sensitivity, double MDCW, double Epsilon, double CLc, double CLs, double Oc, double Os);
      ~ProfilesManager();
      void add_next_profiles(std::vector<PointXYZR*>& data);
      std::vector<PointXYZ> get_polygon();
      Rcpp::List to_R();

    public:
      double chord;

    private:
      int sensitivity;
      double alpha;
      double rmax;
      double radius;
      double width;
      double mdcw;
      double epsilon;
      double clc;
      double cls;
      double oc;
      double os;
      PointXYZR center;
      std::vector<Profile> profiles;
      std::vector<int> localMinimaIndex;
  };
}

#endif //HAMRAZPROFILE_H
