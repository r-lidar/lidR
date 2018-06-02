#include "TreeSegment.h"

Rcpp::NumericVector findEllipseParameters(polygon &);

TreeSegment::TreeSegment()
{
  nbPoints = 0;
  area = 0;
  k = 0;
  scoreS = 0;
  scoreO = 0;
  scoreC = 0;
  scoreR = 0;
  scoreGlobal = 0;
}

TreeSegment::TreeSegment(int k_)
{
  nbPoints = 0;
  area = 0;
  k = k_;

  scoreS = 0;
  scoreO = 0;
  scoreC = 0;
  scoreR = 0;
  scoreGlobal = 0;
}

TreeSegment::TreeSegment(PointXYZ &pt, int k_)
{
  nbPoints = 1;
  area = 0;
  k = k_;

  point_t apex(pt.x, pt.y);
  boost::geometry::append(convex_hull, apex);

  Zmax = pt;
  Zmin = pt;

  scoreS = 0;
  scoreO = 0;
  scoreC = 0;
  scoreR = 0;
  scoreGlobal = 0;
}

TreeSegment::~TreeSegment() {}

/*
* Given a new point pt, this function calculates:
* - new distance between points if there was no point or only one point in the initial tree
* - new area using boost::polygon function if the initial tree contains more than two points (update of associated convex hull)
*/
void TreeSegment::calculateArea()
{
  if (nbPoints <=  2)
    return;

  area = boost::geometry::area(convex_hull);
}


/*
* Given a new point pt, this function calculates:
* - difference between old and new area (including pt) using boost::polygon function
* - if the initial tree contains more than two points (update of associated convex hull)
*/
double TreeSegment::testArea(PointXYZ &pt)
{
  point_t p(pt.x, pt.y);

  if(boost::geometry::covered_by(p, convex_hull))
    return 0;

  polygon old_hull(convex_hull);
  polygon new_hull;
  boost::geometry::append(old_hull, p);
  boost::geometry::convex_hull(old_hull, new_hull);

  double area_Pt = boost::geometry::area(new_hull);
  return(std::fabs(area_Pt - area));
}

/*
* Given a new point pt, this function calculates:
* - distance if only one point in initial tree
* - minimum distance between pt and all points of initial tree if there is more than one point
*/
double TreeSegment::testDist(PointXYZ &pt)
{
  point_t p(pt.x, pt.y);

  //If more than one point in points --> Before distance calculation, search for closest point to pt
  double valRef = boost::geometry::distance(p, convex_hull.outer().at(0));
  double val = 0;

  for (int i = 1 ; i < convex_hull.outer().size() ; i++ )
  {
    val = boost::geometry::distance(p, convex_hull.outer().at(i));

    if (valRef > val)
      valRef = val;
  }

  return valRef;
}

void TreeSegment::addPoint(PointXYZ &pt)
{
  nbPoints++;

  if (pt.z < Zmin.z) Zmin = pt;

  point_t p(pt.x, pt.y);

  if(boost::geometry::covered_by(p, convex_hull))
    return;

  polygon old_hull(convex_hull);
  polygon new_hull;
  boost::geometry::append(old_hull, p);
  boost::geometry::convex_hull(old_hull, new_hull);
  convex_hull = new_hull;

  calculateArea();
}

point_t TreeSegment::get_apex()
{
  point_t p(Zmax.x, Zmax.y);
  return p;
}

void TreeSegment::compute_size_score()
{
  if (area == 0)
  {
    scoreS = 0;
    return;
  }

  // page 101 size creterion

  double H = Zmax.z;
  double D = nbPoints / area;
  double Th = k * D * std::log(H);

  if (nbPoints > Th)
    scoreS = 1;
  else
    scoreS = (nbPoints/Th);
}

void TreeSegment::compute_orientation_score()
{
  scoreO = 0;

  if (area == 0)
    return;

  if (convex_hull.outer().size() <= 2)
    return;

  point_t M = get_apex();

  point_t G;
  boost::geometry::centroid(convex_hull, G);
  double D_MG = boost::geometry::distance(M, G);

  point_t pt(convex_hull.outer().at(0));
  double D = boost::geometry::distance(M, pt);

  for (unsigned int j = 1 ; j < convex_hull.outer().size() ; j++ )
  {
    pt = convex_hull.outer().at(j);
    D += boost::geometry::distance(M, pt);
  }

  D /= convex_hull.outer().size();

  // Comparison D_MG and D (page 101 Eq. 7)
  if (D_MG <= D/2.0)
    scoreO = 1.0 - 2.0*(D_MG/D);
}

void TreeSegment::compute_regularity_score()
{
  scoreR = 0;

  if (area == 0)
    return;

  if (convex_hull.outer().size() <= 2)
    return;

  std::vector<double> planimetricDist_MCH;

  point_t pt;
  point_t M = get_apex();

  for (unsigned int j = 0 ; j < convex_hull.outer().size() ; j++ )
  {
    pt = convex_hull.outer().at(j);
    planimetricDist_MCH.push_back(boost::geometry::distance(M, pt));
  }

  sort(planimetricDist_MCH.begin(), planimetricDist_MCH.end());
  int index_percentile95 = (int)(planimetricDist_MCH.size() * 0.95);

  double radius = planimetricDist_MCH[index_percentile95-1];

  //page 101 Eq. 8
  scoreR = (area/(PI*radius*radius));
}

void TreeSegment::compute_circularity_score()
{
  scoreC = 0;

  if (area == 0)
    return;

  if (convex_hull.outer().size() <= 2)
    return;

  //calculate major and minor axes (A and B)
  Rcpp::NumericVector E = findEllipseParameters(convex_hull);
  double B = E(0) > E(1)? E(0): E(1);
  double A = E(0) < E(1)? E(0): E(1);

  // page 101 Eq.6
  scoreC = (A/B);
}

void TreeSegment::compute_all_score()
{
  compute_size_score();
  compute_orientation_score();
  compute_circularity_score();
  compute_regularity_score();

  scoreGlobal = (scoreS + scoreO + scoreR + scoreC) / 4.0;
}

TreeSegment TreeSegment::merge(TreeSegment &t)
{
  TreeSegment newTree(this->k);

  polygon new_convex_hull;
  polygon union_of_convex_hulls(this->convex_hull);

  boost::geometry::append(union_of_convex_hulls, t.convex_hull.outer());
  boost::geometry::convex_hull(union_of_convex_hulls, new_convex_hull);

  PointXYZ new_Zmax = (this->Zmax.z > t.Zmax.z) ? this->Zmax : t.Zmax;

  newTree.convex_hull = new_convex_hull;
  newTree.Zmax = new_Zmax;
  newTree.nbPoints = this->nbPoints +  t.nbPoints;

  newTree.calculateArea();
  newTree.compute_all_score();
  return(newTree);
}

void TreeSegment::apply2DFilter(std::vector<PointXYZ> &subProfile, std::vector<PointXYZ> &subProfileSubset )
{
  double meanValueForThreshold = 0;
  std::vector<double> dist;
  double stdValueForThreshold = 0;
  double val = 0;
  // Euclidian Distance calculation in 2D for all neighbours regarding the reference point (storage in Z value)
  for (unsigned int i = 1; i < subProfile.size(); i++ )
  {
    val = euclidianDistance2D_inZ( subProfile[0], subProfile[i] );
    meanValueForThreshold += val;
    dist.push_back( val );
  }

  //--------------------------------------------------------------------------------------
  // Threshold definition (page 100 'segmentation principles')
  // defined as the mean plus twice the std of the planimetric distances of a subset of points
  meanValueForThreshold /= (double)(subProfile.size() - 1);
  double sum = 0;
  for(unsigned int i = 0; i < dist.size(); i++)
    sum += (dist[i]-meanValueForThreshold) * (dist[i]-meanValueForThreshold);

  stdValueForThreshold = std::sqrt( sum / (double) dist.size() );

  double threshold = meanValueForThreshold + 2*stdValueForThreshold;
  //--------------------------------------------------------------------------------------
  // Keeping all points below threshold and storage of their IDs
  int i = 0, keep = 0;
  subProfileSubset.push_back(subProfile[0]);
  while ( (i < subProfile.size()-1) && (dist[i] <= threshold) )
  {
    subProfileSubset.push_back( subProfile[i+1] );
    i++;
  }
}

//http://nicky.vanforeest.com/misc/fitEllipse/fitEllipse.html
Rcpp::NumericVector findEllipseParameters(polygon &poly)
{
  int nbPoint = poly.outer().size();
  dmat data(2,nbPoint);

  dmat center = zeros(1,2);
  for (int i = 0; i < nbPoint ; i++ )
  {
    data(0,i) = poly.outer().at(i).get<0>();
    data(1,i) = poly.outer().at(i).get<1>();
  }

  unique(data);

  //means of eachcoordinates --> ellipse center
  for (int i = 0; i < data.n_rows ; i++ )
  {
    center(0,0) += data(0,i);
    center(0,1) += data(1,i);
  }
  center(0,0) /= data.n_rows;
  center(0,1) /= data.n_rows;

  //Covariance C of
  dmat C = (data*data.t()) - center.t()*center;

  mat PC = princomp(C);
  mat data_PCA_2d = data.t() * PC;

  double max_x = max(data_PCA_2d.col(0));
  double max_y = max(data_PCA_2d.col(1));

  double min_x = min(data_PCA_2d.col(0));
  double min_y = min(data_PCA_2d.col(1));

  double half_axis_length1 = fabs(max_x - min_x);
  double half_axis_length2 = fabs(max_y - min_y);

  Rcpp::NumericVector L = Rcpp::NumericVector::create(half_axis_length1,half_axis_length2);

  return (L);
}