#include "QuadTree.h"
#include <cmath>

int QuadTree::nMain = 0;
int QuadTree::nChildren = 0;

//Constructor
Point::Point()
{
}

Point::Point(const double x, const double y) : x(x), y(y), id(0)
{
}

Point::Point(const double x, const double y, const int id) : x(x), y(y), id(id)
{
}

//Constructors
BoundingBox::BoundingBox()
{
}
BoundingBox::BoundingBox(const Point center, const Point halfDim) : center(center), halfDim(halfDim)
{
}

//See if this BB contains the point p
bool BoundingBox::contains(const Point& p)
{
  //Check to see if the point is within the boundaries of the BB.
	if(p.x >= center.x - halfDim.x && p.x <= center.x + halfDim.x &&	p.y >= center.y - halfDim.y && p.y <= center.y + halfDim.y)
		return true;
	else
		return false;
}

//Checks if this bounding box and the argument BB overlaps.
bool BoundingBox::intersects(const BoundingBox& b)
{
	//Box A is this, B is b. LE/RE/TE/BE = Left/Right/Top/Bottom Edge.
	//The following is true if there is overlap between the two rectangles.
	if(center.x - halfDim.x <= b.center.x + b.halfDim.x && //LE of A is to the left of B's RE
	   center.x + halfDim.x >= b.center.x - b.halfDim.x && //RE of A is to the right of B's LE
	   center.y - halfDim.y <= b.center.y + b.halfDim.y && //BE of A is below TE of B
	   center.y + halfDim.y >= b.center.y - b.halfDim.y)   //TE of A is above BE of B
	{
		return true;
	}
	else
		return false;
}

//Constructor for user
QuadTree::QuadTree(const double cx, const double cy, const double range)
{
  MAX_DEPTH = 6;

	boundary = BoundingBox(Point(cx, cy), Point(range, range));
	depth = 1;

	nE = 0;
	nW = 0;
	sE = 0;
	sW = 0;
}

//Internal constructor used when splitting a node
QuadTree::QuadTree(const BoundingBox boundary, const int in_depth) : boundary(boundary)
{
  MAX_DEPTH = 6;

	depth = in_depth + 1;

	nE = 0;
	nW = 0;
	sE = 0;
	sW = 0;
}

bool QuadTree::insert(const Point& p)
{
	if(!boundary.contains(p))
		return false;

	if(depth == MAX_DEPTH)
	{
		points.push_back(p);
		return true;
	}

	if(nW == 0)
		subdivide();

	if(nW->insert(p))
		return true;
	if(nE->insert(p))
		return true;
	if(sW->insert(p))
		return true;
	if(sE->insert(p))
		return true;

	return false;
}

void QuadTree::subdivide()
{
	double halfDim_half = boundary.halfDim.x * 0.5;

  Point p(halfDim_half, halfDim_half);
  Point pNE(boundary.center.x + halfDim_half, boundary.center.y + halfDim_half);
  Point pNW(boundary.center.x - halfDim_half, boundary.center.y + halfDim_half);
  Point pSE(boundary.center.x + halfDim_half, boundary.center.y - halfDim_half);
  Point pSW(boundary.center.x - halfDim_half, boundary.center.y - halfDim_half);

	nE = new QuadTree(BoundingBox(pNE, p), depth);
	nW = new QuadTree(BoundingBox(pNW, p), depth);
	sE = new QuadTree(BoundingBox(pSE, p), depth);
	sW = new QuadTree(BoundingBox(pSW, p), depth);
}

void QuadTree::queryRange(const BoundingBox bb, std::vector<Point*>& res, const int method)
{
	if(!boundary.intersects(bb))
		return;

	//Deepest level reached. Collect and add the points from this QuadTree.
	if(depth == MAX_DEPTH)
	{
		switch(method)
		{
			case 1: getPointsSquare(bb, points, res);
				break;

			case 2: getPointsCircle(bb, points, res);
				break;
		}
	}

	//No points here. Check children, return if there are none.
	if(nW == 0)
		return;

	nE->queryRange(bb, res, method);
	nW->queryRange(bb, res, method);
	sE->queryRange(bb, res, method);
	sW->queryRange(bb, res, method);

	return;
}

void QuadTree::querySquare(const double cx, const double cy, const double range, std::vector<Point*>& res)
{
	queryRange(BoundingBox(Point(cx, cy), Point(range, range)), res, 1);
	return;
}

void QuadTree::queryCircle(const double cx, const double cy, const double range, std::vector<Point*>& res)
{
	queryRange(BoundingBox(Point(cx, cy), Point(range, range)), res, 2);
	return;
}

//A method for collecting the points within a BB that is a square.
void QuadTree::getPointsSquare(const BoundingBox bb, std::vector<Point>& points, std::vector<Point*>& res)
{
	for(std::vector<Point>::iterator it = points.begin(); it != points.end(); it++)
	{
	  if(in_rect(bb, *it))
		  res.push_back(&(*it));
	}
	return;
}

//A method for collecting the points within a bounding circle.
void QuadTree::getPointsCircle(const BoundingBox bb, std::vector<Point>& points, std::vector<Point*>& res)
{
	for(std::vector<Point>::iterator it = points.begin(); it != points.end(); it++)
	{
		if(in_circle(bb.center, (*it), bb.halfDim.x))
			res.push_back(&(*it));
	}
	return;
}

bool QuadTree::in_circle(const Point& p1, const Point& p2, const double r)
{
	double A = p1.x - p2.x;
	double B = p1.y - p2.y;
	double d = sqrt(A*A + B*B);

	return(d <= r);
}

bool QuadTree::in_rect(const BoundingBox& bb, const Point& p)
{
	double A = bb.center.x - p.x;
	double B = bb.center.y - p.y;
	A = A < 0 ? -A : A;
	B = B < 0 ? -B : B;

	return(A <= bb.halfDim.x && B <= bb.halfDim.y);
}