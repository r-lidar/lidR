#include "TreeCollection.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

Rcpp::NumericVector findEllipseParameters(boost::geometry::model::ring<point_t> &points);

TreeCollection::TreeCollection()
{
  nbTree = 0;
  //individualTreeSize.clear();
}

TreeCollection::TreeCollection(TreeSegment &t)
{
  nbTree = 0;
  //individualTreeSize.clear();
  addTree(t);
}

TreeCollection::TreeCollection(const TreeCollection &t)
{
  nbTree = t.nbTree;

  //individualTreeSize.reserve(t.individualTreeSize.size());
  //individualTreeSize.assign(t.individualTreeSize.begin(), t.individualTreeSize.end());

  treeStorage.reserve(t.treeStorage.size());
  treeStorage.assign(t.treeStorage.begin(), t.treeStorage.end());

  idTreeStorage.reserve(t.idTreeStorage.size());
  idTreeStorage.assign(t.idTreeStorage.begin(), t.idTreeStorage.end());
}

TreeCollection::~TreeCollection(){}


void TreeCollection::addTree(TreeSegment t)
{
  treeStorage.push_back(t);
  nbTree++;
  //individualTreeSize.push_back(1);
}

void TreeCollection::getSizeCriteria(int k)
{
  for (unsigned int i = 0; i < nbTree; i++)
    treeStorage[i].getSize(k);
}

void TreeCollection::getOrientationCriteria()
{
  for (unsigned int i = 0; i < nbTree; i++)
    treeStorage[i].getOrientation();
}

void TreeCollection::getRegularityCriteria()
{
  for (unsigned int i = 0; i < nbTree; i++)
    treeStorage[i].getRegularity();
}

void TreeCollection::getCircularityCriteria()
{
  for (unsigned int i = 0; i < nbTree; i++)
    treeStorage[i].getCircularity();
}

double TreeCollection::calculateTreeScores(int k)
{
  // enlever plus tard les 4 scores individuels et ne conserver que le score moyen
  getSizeCriteria(k);
  getOrientationCriteria();
  getRegularityCriteria();
  getCircularityCriteria();   //scoreGlobal calculé ici --> bof
}

// Function that calculates convex hull areas for each selected tree in 'knnTreeID'

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

//========================================================================================
//                              SEARCH ID - USING DISTANCE
//========================================================================================

// Function that calculates euclidian distance if at least one tree of selection contains less than 2 points
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