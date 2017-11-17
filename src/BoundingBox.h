#ifndef BBOX_H
#define BBOX_H

#include "Point.h"

struct BoundingBox
{
	Point center, half_res;

	BoundingBox();
	BoundingBox(const Point,const Point);
	bool contains(const Point&);
	bool intersects(const BoundingBox&);
};

#endif //BBOX_H

