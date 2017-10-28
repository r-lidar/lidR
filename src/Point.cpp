#include "Point.h"

double dist(const Point& lhs, const Point& rhs)
{
  double dx = lhs.x - rhs.x;
  double dy = lhs.y - rhs.y;
  return dx * dx + dy * dy;
}

std::vector<double> sqdistance(std::vector<Point*>& pts, Point& u)
{
  int n = pts.size();
  std::vector<double> y(n);
  std::vector<double>::iterator iy, endy;
  std::vector<Point*>::iterator ip, endp;

  for(ip = pts.begin(), iy = y.begin(), endp = pts.end(), endy = y.end() ; iy < endy && ip < endp ; ++iy, ++ip)
  {
    double dx = (*ip)->x - u.x;
    double dy = (*ip)->y - u.y;
    *iy = dx * dx + dy * dy;
  }

  return y;
}