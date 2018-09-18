#ifndef TREECOLLECTION_H
#define TREESEGMENTMANAGER_H

#include "TreeSegment.h"

namespace ptrees
{
  class TreeSegmentManager
  {
    public:
      TreeSegmentManager(unsigned int, double);
      ~TreeSegmentManager();

      size_t size();
      std::vector<PointXYZ>get_apices();

      bool is_labeled(const PointXYZ&);
      void add_treesegment(TreeSegment &t);
      void add_point_at(unsigned int p, PointXYZ &u);
      void calculateTreeScores();
      void remove_tree_with_less_than_3_points();

      int search_best_match_tree_id(std::vector<int> &knnTreeID, PointXYZ &pointToSort);
      std::vector<int> search_neighbours_labels(std::vector<PointXYZ>&);
      std::vector<TreeSegment> search_trees_in(const TreeSegment&);

      static std::vector<std::vector<int> > createCombination(int);
      static TreeSegment build_combination(std::vector<TreeSegment> &, std::vector<int> &);
      static std::vector<TreeSegment> get_non_combined_tree(std::vector<TreeSegment> &, std::vector<int> &);
      static double average_score(std::vector<TreeSegment>&);
      static void planimetric_filter(std::vector<PointXYZ> &, std::vector<PointXYZ> &);

      Rcpp::List to_R();

    private:
      int searchID_usingArea(std::vector<int> &knnTreeID, PointXYZ &pointToSort);
      int searchID_usingDist(std::vector<int> &knnTreeID, PointXYZ &pointToSort);

    public:
      std::vector<TreeSegment>treeStorage;

    private:
      double hmin;
      std::vector<int> points_labels;
  };
} //end Vega

#endif //TREECOLLECTION_H