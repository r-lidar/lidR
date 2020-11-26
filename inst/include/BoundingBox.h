#ifndef BBOX_H
#define BBOX_H

#include <Shapes.h>

namespace lidR
{

struct BoundingBox: public Shape
{
  BoundingBox();
  BoundingBox(double xmin, double xmax, double ymin, double ymax);
  template<typename T> bool contains(const T&);
  template<typename T> double distance(const T&);
  template<typename T> bool intersects(const T&);
};

inline BoundingBox::BoundingBox() : Shape() { }
inline BoundingBox::BoundingBox(double xmin, double xmax, double ymin, double ymax) : Shape(xmin, xmax, ymin, ymax) { }

template<typename T> bool BoundingBox::contains(const T& p)
{
  return(p.x >= xmin - EPSILON &&
         p.x <= xmax + EPSILON &&
         p.y >= ymin - EPSILON &&
         p.y <= ymax + EPSILON);
}

template<typename T> double BoundingBox::distance(const T& p)
{
  if (p.x < xmin)
  {
    if (p.y < ymin) return std::sqrt((xmin-p.x)*(xmin-p.x)+ (ymin-p.y)*(ymin-p.y));
    if (p.y <= ymax) return xmin - p.x;
    return std::sqrt((xmin-p.x)*(xmin-p.x) + (p.y - ymax)*(p.y - ymax));
  }
  else if (p.x <= xmax)
  {
    if (p.y <  ymin) return ymin - p.y;
    if (p.y <= ymax) return 0;
    return p.y - ymax;
  }
  else
  {
    if (p.y <  ymin) return std::sqrt((p.x - xmax)*(p.x - xmax) + (ymin-p.y)*(ymin-p.y));
    if (p.y <= ymax) return p.x - xmax;
    return std::sqrt((p.x - xmax)*(p.x - xmax) + (p.y - ymax)*(p.y - ymax));
  }
}

template<typename T> bool BoundingBox::intersects(const T& shape)
{
  // If one rectangle is on left side of other
  if (xmin >= shape.xmax || shape.xmin >= xmax)
    return false;

  // If one rectangle is above other
  if (ymax <= shape.ymin || shape.ymax <= ymin)
    return false;

  return true;
}

}

#endif //BBOX_H

