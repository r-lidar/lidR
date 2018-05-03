#ifndef TREECOLLECTION_H
#define TREECOLLECTION_H

#include "Tree.h"

template<typename T> class TreeCollection
{
public:
  TreeCollection();
  TreeCollection( Tree<T> &t );
  ~TreeCollection();

  std::vector<double> individualTreeSize;
  std::vector<Tree<T> >treeStorage;

  void addTree( Tree<T> &t );
  void updateTree( int &treeID, T &pt );

//private:
  int nbTree;

};

template<typename T> TreeCollection<T>::TreeCollection()
{
  nbTree = 0;
}

template<typename T> TreeCollection<T>::TreeCollection( Tree<T> &t )
{
  nbTree = 0;
  addTree( t );
}

template<typename T> TreeCollection<T>::~TreeCollection(){}

template<typename T> void TreeCollection<T>::addTree( Tree<T> &t )
{
  treeStorage.push_back( t );
  nbTree++;
  individualTreeSize.push_back(1);
}

template<typename T> void TreeCollection<T>::updateTree( int &treeID, T &pt )
{
  treeStorage[treeID-1].addPoint( pt );
  individualTreeSize[treeID-1]++;
}

#endif //TREECOLLECTION_H