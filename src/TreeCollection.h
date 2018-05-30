#ifndef TREECOLLECTION_H
#define TREECOLLECTION_H

#include "TreeSegment.h"

using namespace arma;

class TreeCollection
{
  public:
    TreeCollection();
    TreeCollection(TreeSegment &t);
    TreeCollection(const TreeCollection &t);
    ~TreeCollection();

    void addTree(TreeSegment t);

    int searchID(std::vector<int> &knnTreeID, PointXYZ &pointToSort);
    int searchID_usingArea(std::vector<int> &knnTreeID, PointXYZ &pointToSort);
    int searchID_usingDist(std::vector<int> &knnTreeID, PointXYZ &pointToSort);

    void getSizeCriteria(int k);
    void getOrientationCriteria();
    void getRegularityCriteria();
    void getCircularityCriteria();
    double calculateTreeScores(int k);

    void remove_tree_with_less_than_3_points();

    unsigned int nbTree;
    std::vector<TreeSegment>treeStorage;
    std::vector<int> idTreeStorage;
};

#endif //TREECOLLECTION_H