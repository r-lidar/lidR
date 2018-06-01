#ifndef TREECOLLECTION_H
#define TREECOLLECTION_H

#include "TreeSegment.h"

using namespace arma;

class TreeCollection
{
  public:
    TreeCollection();
    TreeCollection(TreeSegment &t);
    ~TreeCollection();

    void addTree(TreeSegment &t);
    void calculateTreeScores();
    void remove_tree_with_less_than_3_points();
    int searchID(std::vector<int> &knnTreeID, PointXYZ &pointToSort);
    std::vector<TreeSegment> search_trees_in_polygon(polygon);
    static std::vector<std::vector<int> > createCombination(int);
    static TreeSegment build_combination(std::vector<TreeSegment> &, std::vector<int> &);
    static std::vector<TreeSegment> get_non_combined_tree(std::vector<TreeSegment> &, std::vector<int> &);
    static double average_score(std::vector<TreeSegment>&);
    Rcpp::List to_R();

  private:
    int searchID_usingArea(std::vector<int> &knnTreeID, PointXYZ &pointToSort);
    int searchID_usingDist(std::vector<int> &knnTreeID, PointXYZ &pointToSort);


  public:
    unsigned int nbTree;
    std::vector<TreeSegment>treeStorage;
    std::vector<int> idTreeStorage;
};

#endif //TREECOLLECTION_H