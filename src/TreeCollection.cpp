

#include "TreeCollection.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

Rcpp::NumericVector findEllipseParameters(boost::geometry::model::ring<point_t> &points);

TreeCollection::TreeCollection()
{
  nbTree = 0;
}

TreeCollection::TreeCollection(TreeSegment &t)
{
  nbTree = 0;
  addTree(t);
}

TreeCollection::~TreeCollection(){}


void TreeCollection::addTree(TreeSegment &t)
{
  treeStorage.push_back(t);
  nbTree++;
}

void TreeCollection::calculateTreeScores(int k)
{
  for (unsigned int i = 0 ; i < nbTree ; i++)
  {
    treeStorage[i].getSize(k);
    treeStorage[i].getOrientation();
    treeStorage[i].getRegularity();
    treeStorage[i].getCircularity();
  }
}

int TreeCollection::searchID(std::vector<int> &knnTreeID, PointXYZ &pointToSort)
{
  // Here we found 2 or more potential trees for the current point. Some of these trees
  // may have less than 3 points. We have to adapt the search method because the rules on the
  // convex hull can't always be applied

  // To know which search method is required for this tree subset (distance or area evaluation)
  // scan the trees to identify if there is at least one of them with less than 2 points

  int searchMethod = 1;
  for (unsigned int j = 0 ; j < knnTreeID.size() ; j++)
  {
    if (treeStorage[knnTreeID[j]-1].nbPoints < 2)
    {
      searchMethod = 2;
      break;
    }
  }

  if(searchMethod == 1)
    return searchID_usingArea(knnTreeID, pointToSort);
  else
    return searchID_usingDist(knnTreeID, pointToSort);
}

int TreeCollection::searchID_usingArea(std::vector<int> &knnTreeID, PointXYZ &pointToSort)
{
  int resultID = knnTreeID[0];

  double area_increment = treeStorage[knnTreeID[0]-1].testArea(pointToSort);   // page 100 eq. 3

  for (unsigned int i = 1; i < knnTreeID.size(); i++ )
  {
    double area_increment2 = treeStorage[knnTreeID[i]-1].testArea(pointToSort);   // page 100 eq. 3
    if (area_increment > area_increment2)
    {
      resultID = knnTreeID[i];
    }
  }

  return resultID;
}

int TreeCollection::searchID_usingDist(std::vector<int> &knnTreeID, PointXYZ &pointToSort)
{
  //Calcul de la premiere distance
  double distValueBis = 0;
  double distValue = treeStorage[knnTreeID[0]-1].testDist(pointToSort);
  int resultID = knnTreeID[0];

  // Comparaison avec les suivantes --> on attribue le point à l'arbre avec la distance la plus petite
  for (unsigned int i = 1 ; i < knnTreeID.size() ; i++ )
  {
    distValueBis = treeStorage[knnTreeID[i]-1].testDist( pointToSort );
    if (distValue > distValueBis)
    {
      distValue = distValueBis;
      resultID = knnTreeID[i];
    }
  }

  return resultID;
}

void TreeCollection::remove_tree_with_less_than_3_points()
{
  unsigned int n = treeStorage.size();

  for (unsigned int i = (n-1) ; i > 0 ; i--)
  {
    if (treeStorage[i].pointsCH.size() < 3)
    {
      treeStorage.erase(treeStorage.begin() + i);
      nbTree--;
    }
  }
}

Rcpp::List TreeCollection::to_R()
{
  int n = treeStorage.size();

  Rcpp::List TreeSegment;

  for (unsigned int i = 0 ; i < treeStorage.size() ; i++ )
  {
    double x = treeStorage[i].apex.get<0>();
    double y = treeStorage[i].apex.get<1>();
    Rcpp::NumericVector Apex = Rcpp::NumericVector::create(x,y);

    std::vector<point_t>& phull = treeStorage[i].convex_hull.outer();
    Rcpp::NumericMatrix rmat(phull.size(), 2);
    for (unsigned int j = 0; j < phull.size(); ++j)
    {
       const point_t& p = phull[j];
       rmat(j,0) = p.get<0>();
       rmat(j,1) = p.get<1>();
    }

    TreeSegment.push_back(Rcpp::List::create(Apex, rmat));
  }

  Rcpp::List output;
  output.push_back(TreeSegment);
  output.push_back(idTreeStorage);

  return(output);
}