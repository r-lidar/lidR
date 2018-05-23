#ifndef TREECOLLECTION_H
#define TREECOLLECTION_H


#include "Tree.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace arma;

//========================================================================================
//Function outside of TreeCollection
Rcpp::NumericVector findEllipseParameters(boost::geometry::model::ring<point_t> &points);

//========================================================================================
template<typename T> class TreeCollection
{
public:
  TreeCollection();
  TreeCollection( Tree<T> &t );
  ~TreeCollection();

  std::vector<double> individualTreeSize;
  std::vector<Tree<T> >treeStorage;
  std::vector<int> idTreeStorage;

  void addTree( Tree<T> t );
  void updateTree( int &treeID, T &pt );

  void searchID_usingArea( std::vector<int> &knnTreeID, T &pointToSort, int &resultID,
                           double &areaValue, boost::geometry::model::ring<point_t> &hull );
  void searchID_usingDist( std::vector<int> &knnTreeID, T &pointToSort, int &resultID, double &distValue );

  void getSizeCriteria(int k);
  void getOrientationCriteria();
  void getRegularityCriteria();
  void getCircularityCriteria();
  double calculateTreeScores( int k );


  int nbTree;

};

//========================================================================================
//                              CONSTRUCTORS
//========================================================================================
template<typename T> TreeCollection<T>::TreeCollection()
{
  nbTree = 0;
  individualTreeSize.clear();
}

template<typename T> TreeCollection<T>::TreeCollection( Tree<T> &t )
{
  nbTree = 0;
  individualTreeSize.clear();
  addTree( t );
}

//========================================================================================
//                              DESTRUCTOR
//========================================================================================
template<typename T> TreeCollection<T>::~TreeCollection(){}

//========================================================================================
//                              ADD TREE
//========================================================================================
template<typename T> void TreeCollection<T>::addTree( Tree<T> t )
{
  treeStorage.push_back( t );
  nbTree++;
  individualTreeSize.push_back(1);
}

//========================================================================================
//                              UPDATE TREE
//========================================================================================
template<typename T> void TreeCollection<T>::updateTree( int &treeID, T &pt )
{
  treeStorage[treeID-1].addPoint( pt );
  individualTreeSize[treeID-1]++;
}

//========================================================================================
//                     FUNCTIONS RELATED TO CRITERIA CALCULATION
//========================================================================================
template<typename T> void TreeCollection<T>::getSizeCriteria(int k)
{
  for (int i = 0; i < nbTree; i++)
    treeStorage[i].getSize(k);
}

template<typename T> void TreeCollection<T>::getOrientationCriteria()
{
  for (int i = 0; i < nbTree; i++)
  treeStorage[i].getOrientation();
}

template<typename T> void TreeCollection<T>::getRegularityCriteria()
{
  for (int i = 0; i < nbTree; i++)
    treeStorage[i].getRegularity();
}

template<typename T> void TreeCollection<T>::getCircularityCriteria()
{
  for (int i = 0; i < nbTree; i++)
    treeStorage[i].getCircularity();
}

template<typename T> double TreeCollection<T>::calculateTreeScores( int k )
{
  //enlever plus tard les 4 scores individuels et ne conserver que le score moyen
  getSizeCriteria(k);
  getOrientationCriteria();
  getRegularityCriteria();
  getCircularityCriteria();   //scoreGlobal calculé ici --> bof
}

//========================================================================================
//                              SEARCH ID - USING AREA
//========================================================================================
//Function that calculates convex hull areas for each selected tree in 'knnTreeID'
template<typename T> void TreeCollection<T>::searchID_usingArea( std::vector<int> &knnTreeID, T &pointToSort,
                                             int &resultID, double &areaValue, boost::geometry::model::ring<point_t> &hull )
{
  //Calcul de la premiere aire de la selection d'arbres
  double areaValue_diff = 0;
  areaValue_diff = treeStorage[knnTreeID[0]-1].testArea( pointToSort, areaValue, hull );   //page 100 Eq3

  //Comparaison avec les suivantes --> on garde la plus petite
  //Attention -> pourquoi 0 quand calcul du convex hull avec trois points?
  double areaValueBis = 0;
  double areaValueBis_diff = 0;
  for ( int i = 1; i < knnTreeID.size(); i++ )
  {
    areaValueBis_diff = treeStorage[knnTreeID[i]-1].testArea( pointToSort, areaValueBis, hull );   //page 100 Eq3
    if (areaValue_diff > areaValueBis_diff)
    {
      areaValue = areaValueBis;
      resultID = knnTreeID[i];
    }
  }
}

//========================================================================================
//                              SEARCH ID - USING DISTANCE
//========================================================================================
//Function that calculates euclidian distance if at least one tree of selection contains less than 2 points
template<typename T> void TreeCollection<T>::searchID_usingDist( std::vector<int> &knnTreeID, T &pointToSort, int &resultID, double &distValue )
{
  //Calcul de la premiere distance
  double distValueBis = 0;
  distValue = treeStorage[knnTreeID[0]-1].testDist( pointToSort );
  //Comparaison avec les suivantes --> on attribue le point à l'arbre avec la distance la plus petite
  for ( int i = 1; i < knnTreeID.size(); i++ )
  {
    distValueBis = treeStorage[knnTreeID[i]-1].testDist( pointToSort );
    if (distValue > distValueBis)
    {
      distValue = distValueBis;
      resultID = knnTreeID[i];
    }
  }
}

#endif //TREECOLLECTION_H