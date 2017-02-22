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
	Point center, half_res;

	BoundingBox();
	BoundingBox(const Point,const Point);
	bool contains(const Point&);
	bool intersects(const BoundingBox&);
};

class QuadTree
{
	public:
		QuadTree(const double, const double, const double);
		static QuadTree create(const std::vector<double>, const std::vector<double>);
		bool insert(const Point&);
		void rect_lookup(const double, const double, const double, std::vector<Point*>&);
		void circle_lookup(const double, const double, const double, std::vector<Point*>&);


	private:
		int MAX_DEPTH;
		int depth;
		BoundingBox boundary;
		std::vector<Point> points;
		QuadTree* NE;
		QuadTree* NW;
		QuadTree* SE;
		QuadTree* SW;

		QuadTree(const BoundingBox, const int);

		void subdivide();
		void range_lookup(const BoundingBox, std::vector<Point*>&, const int);
		void getPointsSquare(const BoundingBox, std::vector<Point>&, std::vector<Point*>&);
		void getPointsCircle(const BoundingBox, std::vector<Point>&, std::vector<Point*>&);
		bool in_circle(const Point&, const Point&, const double);
		bool in_rect(const BoundingBox&, const Point&);
};

#endif //QT_H
