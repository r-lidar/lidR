#include "index.h"

#include <stdexcept>

namespace IncrementalDelaunay
{

Grid::Grid() : ncols(0), nrows(0), ncells(0), xmin(0), ymin(0), xmax(0), ymax(0), xres(0), yres(0) {}

Grid::Grid(double x_min, double y_min, double x_max, double y_max, double res) : xres(res), yres(res)
{
  xmin = std::floor(x_min);
  ymin = std::floor(y_min);

  // Number of cells needed to fully cover the domain
  ncols = static_cast<int>(std::ceil((x_max - xmin) / xres));
  nrows = static_cast<int>(std::ceil((y_max - ymin) / yres));

  if (ncols <= 0 || nrows <= 0)
    throw std::runtime_error("Invalid grid dimensions.");

  xmax = xmin + ncols * xres;
  ymax = ymin + nrows * yres;

  ncells = ncols * nrows;
}

int Grid::cell_from_xy(double x, double y) const
{
  if (x < xmin || x > xmax || y < ymin || y > ymax)
    return -1;

  int col = std::floor((x - xmin) / xres);
  int row = std::floor((ymax - y) / yres);
  if (y == ymin) row = nrows-1;
  if (x == xmax) col = ncols-1;
  return cell_from_row_col(row, col);
}

void Grid::get_cells(double xmin, double ymin, double xmax, double ymax, std::vector<int>& cells) const
{
  int colmin = (xmin - this->xmin) / xres;
  int colmax = (xmax - this->xmin) / xres;
  int rowmin = (this->ymax - ymax) / yres;
  int rowmax = (this->ymax - ymin) / yres;
  cells.clear();

  for (int col = std::max(colmin,0) ; col <= std::min(colmax, (int)ncols-1) ; col++) {
    for (int row = std::max(rowmin,0) ; row <= std::min(rowmax, (int)nrows-1) ; row++) {
      cells.push_back(row * ncols + col);
    }
  }
}

}