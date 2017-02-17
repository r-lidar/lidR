#ifndef QT_H
#define QT_H

#include <vector>

struct Point
{
	double x, y;
  int id;

	Point();
	Point(const double, const double);
	Point(const double, const double, const int);
};

struct BoundingBox
{
	Point center, halfDim;

	BoundingBox();
	BoundingBox(const Point,const Point);
	bool contains(const Point&);
	bool intersects(const BoundingBox&);
};

class QuadTree
{
	public:
		static int nMain;
		static int nChildren;
		QuadTree(const double, const double, const double);
		bool insert(const Point&);
		void querySquare(const double, const double, const double, std::vector<Point*>&);
		void queryCircle(const double, const double, const double, std::vector<Point*>&);


	private:
		int MAX_DEPTH;
		int depth;
		BoundingBox boundary;
		std::vector<Point> points;
		QuadTree* nE;
		QuadTree* nW;
		QuadTree* sE;
		QuadTree* sW;

		QuadTree(const BoundingBox, const int);

		void subdivide();
		void queryRange(const BoundingBox, std::vector<Point*>&, const int);
		void getPointsSquare(const BoundingBox, std::vector<Point>&, std::vector<Point*>&);
		void getPointsCircle(const BoundingBox, std::vector<Point>&, std::vector<Point*>&);
		bool in_circle(const Point&, const Point&, const double);
		bool in_rect(const BoundingBox&, const Point&);
};

#endif //QT_H
