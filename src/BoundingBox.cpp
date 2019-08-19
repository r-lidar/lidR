#include "BoundingBox.h"

BoundingBox::BoundingBox(){}
BoundingBox::BoundingBox(const Point center, const Point half_res) : center(center), half_res(half_res) {}

bool BoundingBox::contains(const Point& p, double buffer = 0)
{
  if(p.x >= center.x - half_res.x - buffer &&
     p.x <= center.x + half_res.x + buffer &&
     p.y >= center.y - half_res.y - buffer &&
     p.y <= center.y + half_res.y + buffer)
    return true;
  else
    return false;
}

bool BoundingBox::intersects(const BoundingBox& b)
{

  if(center.x - half_res.x <= b.center.x + b.half_res.x &&
     center.x + half_res.x >= b.center.x - b.half_res.x &&
     center.y - half_res.y <= b.center.y + b.half_res.y &&
     center.y + half_res.y >= b.center.y - b.half_res.y)
  {
    return true;
  }
  else
    return false;
}
