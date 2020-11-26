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
  KnnBucket(PointXYZ& p, const unsigned int k, const double radius);
  void push(PointXYZ& p);

  unsigned int k;
  unsigned int pos_max_dist;
  double max_dist;
  PointXYZ pref;
  std::vector<double> distance;
  std::vector<PointXYZ*> bucket;
};

inline KnnBucket::KnnBucket(PointXYZ& _p, const unsigned int _k, const double radius)
{
  k = 0;
  pref = _p;
  pos_max_dist = 0;
  max_dist = (radius == 0) ? std::numeric_limits<double>::max() : radius;
  bucket.resize(_k);
  distance.resize(_k);
}

inline void KnnBucket::push(PointXYZ& p)
{
  double dist = std::sqrt((pref.x-p.x)*(pref.x-p.x)+(pref.y-p.y)*(pref.y-p.y)+(pref.z-p.z)*(pref.z-p.z));

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
