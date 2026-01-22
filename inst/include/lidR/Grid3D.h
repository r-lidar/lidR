#ifndef GRID3D_H
#define GRID3D_H

#include <Rcpp.h>

#include <queue>
#include <tuple>

#include "Shapes.h"

namespace lidR
{

#define ROUNDANY(x,m) round((x) / m) * m

class Grid3D
{
public:
  Grid3D();
  Grid3D(const Rcpp::S4 las, double res);
  int64_t ncols, nrows, nlayers, ncells;
  double xmin,ymin,xmax,ymax,zmin,zmax;
  double xres, yres, zres;
  double area, volume;
  int npoints;

public:
  Rcpp::IntegerVector connected_components(int connectivity);
  int64_t get_cell(double, double, double);
  int64_t get_cell_id(int64_t, int64_t, int64_t);

protected:
  Rcpp::NumericVector X;
  Rcpp::NumericVector Y;
  Rcpp::NumericVector Z;

protected:
  void bfs(int64_t, int64_t, int64_t, int, std::unordered_map<int64_t, int>&, int connectivity);
};

inline Grid3D::Grid3D()
{
  xmin = 0;
  ymin = 0;
  zmin = 0;
  zmax = 0;
  ymax = 0;
  zmax = 0;
  xres = 0;
  yres = 0;
  zres = 0;
  area = 0;
  volume = 0;
  npoints = 0;
}

inline Grid3D::Grid3D(const Rcpp::S4 las, double res)
{
  Rcpp::DataFrame data = Rcpp::as<Rcpp::DataFrame>(las.slot("data"));
  Rcpp::NumericVector x = data["X"];
  Rcpp::NumericVector y = data["Y"];
  Rcpp::NumericVector z = data["Z"];

  npoints = data.nrow();

  // Number of points
  X = x;
  Y = y;
  Z = z;

  //  Rcpp::stop("Internal error in spatial index: impossible to build an index with 0 points."); // # nocov
  xres = res;
  yres = res;
  zres = res;

  // Compute the bounding box
  xmin =  XYINF;
  xmax = -XYINF;
  ymin =  XYINF;
  ymax = -XYINF;
  zmin =  ZINF;
  zmax = -ZINF;

  for (auto i = 0 ; i < x.size() ; i++)
  {
    if (x[i] < xmin) xmin = x[i];
    if (x[i] > xmax) xmax = x[i];
    if (y[i] < ymin) ymin = y[i];
    if (y[i] > ymax) ymax = y[i];
    if (z[i] < zmin) zmin = z[i];
    if (z[i] > zmax) zmax = z[i];
  }

  xmin = std::floor(xmin / xres) * xres;
  xmax = std::ceil (xmax / xres) * xres;
  ymin = std::floor(ymin / yres) * yres;
  ymax = std::ceil (ymax / yres) * yres;
  zmin = std::floor(zmin / zres) * zres;
  zmax = std::ceil (zmax / zres) * zres;

  ncols   = (int64_t)std::floor((xmax - xmin) / xres) + 1;
  nrows   = (int64_t)std::floor((ymax - ymin) / yres) + 1;
  nlayers = (int64_t)std::floor((zmax - zmin) / zres) + 1;

  uint64_t max_cells =
    static_cast<uint64_t>(ncols) *
    static_cast<uint64_t>(nrows) *
    static_cast<uint64_t>(nlayers);

  if (max_cells > static_cast<uint64_t>(std::numeric_limits<int64_t>::max()))
  {
    Rcpp::stop("Internal error: Number of grid cells exceeds the maximum allowable integer value.");
  }

  ncells  = ncols*nrows*nlayers;

  double xrange = xmax - xmin;
  double yrange = ymax - ymin;
  double zrange = zmax - zmin;

  area = xrange * yrange;
  volume = area * zrange;
}

inline int64_t Grid3D::get_cell(double x, double y, double z)
{
  if (x < xmin || x > xmax || y < ymin || y > ymax || z < zmin || z > zmax)
  {
    Rcpp::stop("Internal error in spatial index: point out of range.");
  }

  int64_t col = std::floor((x - xmin) / xres);
  int64_t row = std::floor((y - ymin) / yres);
  int64_t lay = std::floor((z - zmin) / zres);

  if (col < 0 || col >= ncols || row < 0 || row >= nrows || lay < 0 || lay >= nlayers)
  {
    Rcpp::Rcout << col << row << lay << std::endl;
    Rcpp::stop("Internal error in spatial index: indices out of range.");
  }

  return  get_cell_id(row, col, lay);
}

inline int64_t Grid3D::get_cell_id(int64_t row, int64_t col, int64_t lay)
{
  if (row < 0 || row > nrows-1 || col < 0 || col > ncols-1 || lay < 0 || lay > nlayers-1)
    Rcpp::stop("Internal error in spatial index: cell out of the range."); // # nocov
  int64_t cell = lay * nrows * ncols + row * ncols + col;
  return cell;
}

inline void Grid3D::bfs(int64_t start_x, int64_t start_y, int64_t start_z,
                        int label_id, std::unordered_map<int64_t, int>& label_grid,
                        int connectivity = 6)
{
  // Define neighbor offsets based on connectivity
  std::vector<std::tuple<int,int,int>> neighbors;

  for (int dx = -1; dx <= 1; ++dx) {
    for (int dy = -1; dy <= 1; ++dy) {
      for (int dz = -1; dz <= 1; ++dz) {
        if (dx == 0 && dy == 0 && dz == 0) continue; // skip self

        int sum_abs = std::abs(dx) + std::abs(dy) + std::abs(dz);

        if ((connectivity == 6 && sum_abs == 1) ||     // face neighbors
            (connectivity == 18 && sum_abs <= 2) ||    // face + edge neighbors
            (connectivity == 26))                      // all neighbors
        {
          neighbors.emplace_back(dx, dy, dz);
        }
      }
    }
  }

  std::queue<std::tuple<int64_t, int64_t, int64_t>> q;
  q.push(std::make_tuple(start_x, start_y, start_z));

  // Mark the starting voxel with the label
  label_grid[get_cell_id(start_y, start_x, start_z)] = label_id;

  while (!q.empty())
  {
    auto [x, y, z] = q.front();
    q.pop();

    // Check all 6-connected neighbors
    for (auto [dx, dy, dz] : neighbors)
    {
      int64_t nx = x + dx;
      int64_t ny = y + dy;
      int64_t nz = z + dz;

      // Ensure the neighbor is within bounds
      if (nx >= 0 && ny >= 0 && nz >= 0 && nx < ncols && ny < nrows && nz < nlayers)
      {
        int64_t neighbor_index = get_cell_id(ny, nx, nz);

        // If the neighbor is labeled and not yet labeled
        if (label_grid.find(neighbor_index) != label_grid.end() && label_grid[neighbor_index] == 0)
        {
          label_grid[neighbor_index] = label_id;
          q.push(std::make_tuple(nx, ny, nz));
        }
      }
    }
  }
}

inline Rcpp::IntegerVector Grid3D::connected_components(int connectivity)
{

  std::unordered_map<int64_t, int> label_grid;

  // Populate the label_grid with occupied voxels
  for (auto i = 0; i < X.size(); i++)
  {
    int64_t cell = get_cell(X[i], Y[i], Z[i]);
    label_grid[cell] = 0; // Initialize the voxel as unlabelled (0)
  }

  int label_id = 1; // Start labeling from 1 (0 means no component)

  // Loop through all voxels in the grid
  for (int64_t z = 0; z < nlayers; ++z)
  {
    for (int64_t y = 0; y < nrows; ++y)
    {
      for (int64_t x = 0; x < ncols; ++x)
      {
        int64_t index = get_cell_id(y, x, z);

        // If the voxel is labeled and unlabelled, perform BFS/DFS
        if (label_grid.find(index) != label_grid.end() && label_grid[index] == 0)
        {
          bfs(x, y, z, label_id, label_grid, connectivity);
          label_id++;
        }
      }
    }
  }

  Rcpp::IntegerVector id(X.size());
  for (auto i = 0; i < X.size(); i++)
  {
    int64_t cell = get_cell(X[i], Y[i], Z[i]);
    id[i] = label_grid[cell]; // Safe access since we only label occupied cells
  }

  return id;
}

}

#endif // GRID3D_H
