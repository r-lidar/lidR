#ifndef QT_KNN
#define QT_KNN

#include <limits>
#include "Point.h"

namespace lidR
{

namespace Bucket
{

struct KnnBucket
{
  KnnBucket(const PointXYZ& p, const unsigned int k, const double radius);
  KnnBucket(const PointXY& p, const unsigned int k, const double radius);
  void push(PointXYZ& p);

  bool XYonly = false;
  unsigned int k;
  unsigned int pos_max_dist;
  double max_dist;
  PointXYZ pref;
  std::vector<double> distance;
  std::vector<PointXYZ*> bucket;
};

inline KnnBucket::KnnBucket(const PointXYZ& p, const unsigned int k, const double radius)
{
  this->k = 0;
  pref = p;
  XYonly = false;
  pos_max_dist = 0;
  max_dist = (radius == 0) ? std::numeric_limits<double>::max() : radius;
  bucket.resize(k);
  distance.resize(k);
}

inline KnnBucket::KnnBucket(const PointXY& p, const unsigned int k, const double radius)
{
  this->k = 0;
  pref = PointXYZ(p.x, p.y, 0);
  XYonly = true;
  pos_max_dist = 0;
  max_dist = (radius == 0) ? std::numeric_limits<double>::max() : radius;
  bucket.resize(k);
  distance.resize(k);
}


inline void KnnBucket::push(PointXYZ& p)
{
  double dist = (pref.x-p.x)*(pref.x-p.x)+(pref.y-p.y)*(pref.y-p.y);
  if (!XYonly) dist += (pref.z-p.z)*(pref.z-p.z);
  dist = std::sqrt(dist);

  if (dist <= max_dist && k < bucket.size())
  {
    bucket[k] = &p;
    distance[k] = dist;
    k++;

    if (k == bucket.size())
    {
      std::vector<double>::const_iterator it = std::max_element(distance.begin(), distance.end());
      max_dist = *it;
      pos_max_dist = it - distance.begin();
    }
  }
  else
  {
    if (dist < max_dist)
    {
      bucket[pos_max_dist] = &p;
      distance[pos_max_dist] = dist;
      std::vector<double>::const_iterator it = std::max_element(distance.begin(), distance.end());
      max_dist = *it;
      pos_max_dist = it - distance.begin();
    }
  }

  return;
}

}
}

#endif
