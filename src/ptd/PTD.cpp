#include <stdexcept>
#include <limits>
#include <algorithm>
#include <numeric>
#include <random>
#include <cmath>
#include <chrono>
#include <unordered_map>

#include "PTD.h"

#ifdef _OPENMP
#include <omp.h>
#else
#define omp_get_num_threads() 1
#define omp_get_thread_num() 0
#define omp_get_max_threads() 1
#define omp_get_thread_limit() 1
#define omp_set_max_active_levels(x)
#endif

namespace PTD
{

using clock = std::chrono::steady_clock;

bool axelsson_metrics(const Vec3& P, const Vec3& A, const Vec3& B, const Vec3& C, double& dist_d, double& angle);
double distance_to_fitted_plane(const Point& query, const std::vector<size_t>& neighbor_indices, const IncrementalDelaunay::Vertex* vertices);

struct PointAdaptor
{
  const std::vector<Point>& pts;
  PointAdaptor(const std::vector<Point>& pts_) : pts(pts_) {}
  inline size_t kdtree_get_point_count() const { return pts.size(); }
  inline double kdtree_get_pt(const size_t idx, const size_t dim) const { return (dim == 0 ? pts[idx].x : pts[idx].y); }
  template <class BBOX> bool kdtree_get_bbox(BBOX&) const { return false; }
};

struct VertexAdaptor
{
  const IncrementalDelaunay::Vertex* vertices;
  size_t count;
  VertexAdaptor(const IncrementalDelaunay::Vertex* v, size_t c) : vertices(v), count(c) {}
  inline size_t kdtree_get_point_count() const { return count; }
  inline double kdtree_get_pt(const size_t idx, const size_t dim) const {
    if (dim == 0) return vertices[idx].pos.x;
    if (dim == 1) return vertices[idx].pos.y;
    return vertices[idx].pos.z;
  }
  template <class BBOX> bool kdtree_get_bbox(BBOX& /*bb*/) const { return false; }
};

PTD::PTD(const Parameters& p, Bbox b)
{
  if (p.seed_resolution_search <= 0.0) throw std::invalid_argument("seed_resolution_search must be > 0");
  if (p.max_iteration_angle < 0.0 || params.max_iteration_angle > 90.0) throw std::invalid_argument("max_iteration_angle must be in [0, 90] degrees");
  if (p.max_iteration_distance <= 0.0) throw std::invalid_argument("max_iteration_distance must be > 0");
  if (p.spacing < 0.0) throw std::invalid_argument("spacing must be >= 0");
  if (p.buffer_size < 0.0) throw std::invalid_argument("buffer_size must be >= 0");

  params = p;
  params.spacing = params.spacing *  params.spacing; //squared comparable distance
  if (params.ncores > (unsigned int)omp_get_max_threads()) params.ncores = omp_get_max_threads();

  logger = nullptr;
  d = nullptr;

  spacing_grid = Grid(b.xmin, b.ymin, b.xmax, b.ymax, p.spacing);
  candidates.resize(spacing_grid.get_ncells());
  for(auto& p : candidates) { p.z = std::numeric_limits<double>::infinity(); }

  x_offset = std::round((b.xmin+b.xmax)/2);
  y_offset = std::round((b.ymin+b.ymax)/2);
  z_offset = std::round((b.zmin+b.zmax)/2);

  bbox = b;
  bbox.xmin -= x_offset;
  bbox.xmax -= x_offset;
  bbox.ymin -= y_offset;
  bbox.ymax -= y_offset;
}

PTD::~PTD() { if (d) delete d; }

void PTD::set_logger(Logger l)
{
  logger = l;
}

bool PTD::insert_point(double x, double y, double z, unsigned int fid)
{
  int cell = spacing_grid.cell_from_xy(x, y);
  if (cell < 0 || cell >= (int)candidates.size()) return false;

  if (z < candidates[cell].z)
  {
    auto& pt = candidates[cell];
    pt.x = x - x_offset;
    pt.y = y - y_offset;
    pt.z = z;
    pt.fid = fid;
  }

  return true;
}

bool PTD::run()
{
  // Delete the triangulation if reran
  if (d) delete d;

  // Constructor initialized one point per grid cell with +inf Z.
  // Some may still be +inf if no point inserted for a given cell
  auto new_end = std::remove_if(candidates.begin(), candidates.end(), [](const Point& p) {
    return p.z == std::numeric_limits<double>::infinity();
  });
  candidates.erase(new_end,  candidates.end());

  if (candidates.empty()) throw std::runtime_error("0 point to process");

  // Boolean vector that record if a point was already inserted
  inserted.assign(candidates.size(), false);

  sort_by_z();        // Sort to process bottom to top
  make_seeds();       // Generate the seed points for iteration 0
  make_buffer();      // Generate extra virtual buffer seeds

  // Grid used as spatial index by the triangulation (and this class)
  double bs = params.buffer_size;
  index_grid = IncrementalDelaunay::Grid(bbox.xmin-bs, bbox.ymin-bs, bbox.xmax+bs, bbox.ymax+bs, 1);

  // Incremental triangulation
  d = new IncrementalDelaunay::Triangulation(index_grid);

  // For the few first points spatial index search is slow because triangle
  // are too big. We can deactivate spatial indexing. This is actually faster
  d->desactivate_spatial_index();

  tin_buffer();
  tin_seeds();

  // Now the edges and seeds are inserted we can leverage spatial indexing.
  d->activate_spatial_index();

  // Progressive densification
  tin_densify();

  // Detect spikes to find outliers
  is_spike.assign(d->vcount, false);
  detect_spikes(12, 0.75);
  detect_spikes(12, 0.75);

  return true;
}

void PTD::sort_by_z()
{
  std::stable_sort(candidates.begin(), candidates.end(), [](const auto& a, const auto& b)
  {
    return a.z < b.z;
  });
}

void PTD::make_seeds()
{
  const auto t0 = clock::now();

  seeds.clear();

  const double r = params.seed_resolution_search;
  const double dx = r * 0.5;
  const double dy = r * 0.5;

  // Original grid
  IncrementalDelaunay::Grid grid0(bbox.xmin, bbox.ymin, bbox.xmax, bbox.ymax, r);
  IncrementalDelaunay::Grid grid1(bbox.xmin+dx, bbox.ymin+dy, bbox.xmax+dx, bbox.ymax+dy, r);

  sample_lowest_per_cell(candidates, grid0, seeds);
  sample_lowest_per_cell(candidates, grid1, seeds);

  if (seeds.empty()) throw std::runtime_error("0 seed to process");

  deduplicate_seeds();

  z_default = std::accumulate(
    seeds.begin(), seeds.end(), 0.0,
    [](double sum, const Point& p) { return sum + p.z; }
  ) / seeds.size();


  if (params.verbose)
  {
    const auto t1 = clock::now();
    const double elapsed_sec = std::chrono::duration<double>(t1 - t0).count();
    std::string timing = "  Searching seed done in " + std::to_string(elapsed_sec) + " s";
    log(timing);
  }
}

void PTD::make_buffer()
{
  if (params.buffer_size <= 0) return;

  std::mt19937 generator(42);
  std::uniform_real_distribution<double> distribution(-0.5, 0.5);

  double xmin = bbox.xmin - (params.buffer_size - 1);
  double ymin = bbox.ymin - (params.buffer_size - 1);
  double xmax = bbox.xmax + (params.buffer_size - 1);
  double ymax = bbox.ymax + (params.buffer_size - 1);

  double dx = xmax - xmin;
  double dy = ymax - ymin;

  int nx = std::round(dx / params.seed_resolution_search);
  int ny = std::round(dy / params.seed_resolution_search);
  double sx = dx / nx;
  double sy = dy / ny;

  vbuff.clear();
  vbuff.reserve(2 * (nx + ny) + 4); // perimeter only

  Point p;
  double x, y, x_noise, y_noise;

  // Bottom and top edges
  for (int i = 0; i <= nx; i++)
  {
    x = xmin + i * sx;
    x_noise = distribution(generator);

    // Bottom (ymin)
    y_noise = distribution(generator);
    p.x = x + x_noise;
    p.y = ymin + y_noise;
    vbuff.push_back(p);

    // Top
    y_noise = distribution(generator);
    p.x = x + x_noise;
    p.y = ymax + y_noise;
    vbuff.push_back(p);
  }

  // Left and right edges (skip corners to avoid duplicates)
  for (int j = 1; j < ny; j++)
  {
    y = ymin + j * sy;
    y_noise = distribution(generator);

    // Left (xmin)
    x_noise = distribution(generator);
    p.x = xmin + x_noise;
    p.y = y + y_noise;
    vbuff.push_back(p);

    // Right (xmax)
    x_noise = distribution(generator);
    p.x = xmax + x_noise;
    p.y = y + y_noise;
    vbuff.push_back(p);
  }

  // Using nanoflann to set Z for buffer points based on nearest seeds
  PointAdaptor adaptor(seeds);
  typedef nanoflann::KDTreeSingleIndexAdaptor<nanoflann::L2_Simple_Adaptor<double, PointAdaptor>, PointAdaptor, 2> kd_tree_t;
  kd_tree_t index(2, adaptor, nanoflann::KDTreeSingleIndexAdaptorParams(10));
  index.buildIndex();

  for (auto& vp : vbuff)
  {
    double query_pt[2] = { vp.x, vp.y };
    size_t ret_index;
    double out_dist_sqr;
    nanoflann::KNNResultSet<double> resultSet(1);
    resultSet.init(&ret_index, &out_dist_sqr);
    index.findNeighbors(resultSet, query_pt, nanoflann::SearchParameters());
    vp.z = seeds[ret_index].z;
  }
}

void PTD::tin_buffer()
{
  const auto t0 = clock::now();

  int t;
  for (auto& p : vbuff)
  {
    t = d->findContainerTriangleFast(p);
    if (!d->delaunayInsertion(p, t))
    {
      throw std::runtime_error("Internal error: virtual seed point not inserted");
    }
  }

  if (params.verbose)
  {
    const auto t1 = clock::now();
    const double elapsed_sec = std::chrono::duration<double>(t1 - t0).count();
    std::string timing = "  Triangulating buffer points done in " + std::to_string(elapsed_sec) + " s";
    log(timing);
  }
}

void PTD::tin_seeds()
{
  const auto t0 = clock::now();

  // To retrive the index
  std::unordered_map<unsigned int, std::size_t> fid_to_candidate_idx;
  fid_to_candidate_idx.reserve(candidates.size());
  for (std::size_t i = 0; i < candidates.size(); ++i)
    fid_to_candidate_idx[candidates[i].fid] = i;

  int t;
  for (auto& seed : seeds)
  {
    t = d->findContainerTriangleFast(seed);
    if (d->delaunayInsertion(seed, t))
    {
      auto it = fid_to_candidate_idx.find(seed.fid);
      if (it != fid_to_candidate_idx.end())
      {
        inserted[it->second] = true;
      }
      else
      {
        throw std::runtime_error("Internal error: invalid fid detected");
      }
    }
  }

  if (params.verbose)
  {
    const auto t1 = clock::now();
    const double elapsed_sec = std::chrono::duration<double>(t1 - t0).count();
    std::string timing = "  Triangulating seed points done in " + std::to_string(elapsed_sec) + " s";
    log(timing);
  }
}


void PTD::tin_densify()
{
  const auto t0 = clock::now();

  unsigned int iteration = 0;
  unsigned int count = 0;
  int t = -1;

  int n_cells = index_grid.get_ncells();
  std::vector<bool> active_regions(n_cells, true);

  // We loop while we are adding new triangles
  do
  {
    const auto tt1 = clock::now();

    if (params.max_iter == 0) break;

    // Reset the dirty tracker inside the triangulation
    // so it only records changes happening during this specific iteration
    d->reset_dirty_cells();

    count = 0;
    iteration++;

    for (size_t i = 0; i < candidates.size(); i++)
    {
      if (inserted[i]) continue;

      const auto& P = candidates[i];

      // Optimization Step
      // Check if the region containing this point was modified in the previous step.
      // If the cell is valid and was NOT marked active/modified in the previous loop,
      // we can skip this point entirely. The triangulation here hasn't changed.
      int cell_id = index_grid.cell_from_xy(P.x, P.y);
      if (!active_regions[cell_id]) continue;

      t = d->findContainerTriangleFast(P);
      if (t < 0) continue;

      // Retrieve vertices (A, B, C)
      IncrementalDelaunay::Triangle& tri = d->triangles[t];
      Vec3 A(d->vertices[tri.v[0]].pos);
      Vec3 B(d->vertices[tri.v[1]].pos);
      Vec3 C(d->vertices[tri.v[2]].pos);

      // Skip too small triangles. Small triangle are frozen and cannot be subdivided.
      Vec3 u = A - B;
      Vec3 v = A - C;
      Vec3 w = B - C;
      double edge_AB = u.dot(u);
      double edge_AC = v.dot(v);
      double edge_BC = w.dot(w);
      double sq_max_edge = std::max(edge_AB, std::max(edge_AC, edge_BC));
      double spacing = params.spacing;
      if (sq_max_edge < spacing)
      {
        inserted[i] = true; // Freeze
        continue;
      }

      double dist_d, angle;

      Vec3 p(P);
      if (!axelsson_metrics(p, A, B, C, dist_d, angle)) continue;

      if (angle < params.max_iteration_angle && dist_d < params.max_iteration_distance)
      {
        if (d->delaunayInsertion(P, t))
        {
          inserted[i] = true;
          count++;
        }
      }
    }

    active_regions = d->get_dirty_cells();
    float perc = (double)count / (double)d->vcount;
    if (perc < 0.5f / 1000.0f) break;

    if (params.verbose)
    {
      const auto t1 = clock::now();
      const double elapsed_sec = std::chrono::duration<double>(t1 - tt1).count();
      std::string timing = "    Densification " + std::to_string(iteration) + " done in " + std::to_string(elapsed_sec) + " s";
      log(timing);
    }

  } while (count > 0 && iteration < params.max_iter);

  if (params.verbose)
  {
    const auto t1 = clock::now();
    const double elapsed_sec = std::chrono::duration<double>(t1 - t0).count();
    std::string timing = "  Progressive densification done in " + std::to_string(elapsed_sec) + " s";
    log(timing);
  }
}


void PTD::detect_spikes(int k, double threshold)
{
  const auto t0 = clock::now();

  if (params.max_iter == 0) return;

  const double spacing_limit = 3 * params.spacing;

  // RANSAC parameters
  constexpr int ransac_iters = 25;
  const double consensus_threshold = threshold * 0.75;

  // Build index
  VertexAdaptor adaptor(d->vertices, d->vcount);
  typedef nanoflann::KDTreeSingleIndexAdaptor<nanoflann::L2_Simple_Adaptor<double, VertexAdaptor>, VertexAdaptor, 2> kdtree;
  kdtree index(2, adaptor, nanoflann::KDTreeSingleIndexAdaptorParams(10));
  index.buildIndex();

  #pragma omp parallel num_threads(params.ncores)
  {
    // Thread-local Memory Reuse (Hoisting)
    // Each thread gets its own vectors to avoid allocation overhead
    std::vector<unsigned int> ret_index(k + 1);
    std::vector<double> out_dist_sqr(k + 1);
    std::vector<size_t> neighbors;
    std::vector<Vec3> nb_pos;
    neighbors.reserve(k);
    nb_pos.reserve(k);

    #pragma omp for schedule(dynamic)
    for (int i = 0; i < d->vcount; ++i)
    {
      if (is_spike[i]) continue;

      double query_pt[2] = {d->vertices[i].pos.x, d->vertices[i].pos.y};
      size_t num_results = index.knnSearch(&query_pt[0], k + 1, &ret_index[0], &out_dist_sqr[0]);

      neighbors.clear();
      nb_pos.clear();

      // Filter neighbors
      for (size_t j = 1; j < num_results; ++j)
      {
        if (out_dist_sqr[j] <= spacing_limit) continue;

        unsigned int idx = ret_index[j];

        if (is_spike[idx]) continue;

        neighbors.push_back(idx);

        auto& v = d->vertices[idx].pos;
        nb_pos.push_back({v.x, v.y, v.z});

        if ((int)neighbors.size() == k) break;
      }

      if (neighbors.size() < 3) continue;

      double best_nx = 0, best_ny = 0, best_nz = 0, best_d = 0;
      int max_inliers = -1;

      // Optimization: Sufficient Inlier Count (Early Exit)
      // If 75% of neighbors fit the plane, it's a good plane. Stop searching.
      int sufficient_inliers = (int)(neighbors.size() * 0.75);

      std::mt19937 rng(i);
      std::uniform_int_distribution<int> dist_indices(0, (int)nb_pos.size() - 1);

      for (int iter = 0; iter < ransac_iters; ++iter)
      {
        int idx1 = dist_indices(rng);
        int idx2 = dist_indices(rng);
        int idx3 = dist_indices(rng);

        // Quick check to avoid degenerate triangles
        if (idx1 == idx2 || idx1 == idx3 || idx2 == idx3) continue;

        const Vec3& p1 = nb_pos[idx1];
        const Vec3& p2 = nb_pos[idx2];
        const Vec3& p3 = nb_pos[idx3];

        double v1x = p2.x - p1.x;
        double v1y = p2.y - p1.y;
        double v1z = p2.z - p1.z;
        double v2x = p3.x - p1.x;
        double v2y = p3.y - p1.y;
        double v2z = p3.z - p1.z;

        double nx = v1y * v2z - v1z * v2y;
        double ny = v1z * v2x - v1x * v2z;
        double nz = v1x * v2y - v1y * v2x;

        double len_sq = nx*nx + ny*ny + nz*nz;
        if (len_sq < 1e-18) continue;

        double len = std::sqrt(len_sq);
        double inv_len = 1.0 / len;
        nx *= inv_len;
        ny *= inv_len;
        nz *= inv_len;

        double d_val = -(nx * p1.x + ny * p1.y + nz * p1.z);

        int inliers = 0;
        for (const auto& p : nb_pos)
        {
          if (std::abs(nx * p.x + ny * p.y + nz * p.z + d_val) < consensus_threshold)
          {
            inliers++;
          }
        }

        if (inliers > max_inliers)
        {
          max_inliers = inliers;
          best_nx = nx; best_ny = ny; best_nz = nz; best_d = d_val;

          if (max_inliers >= sufficient_inliers) break;
        }
      }

      if (max_inliers == -1) continue;

      auto& q = d->vertices[i].pos;
      double dist = std::abs(best_nx * q.x + best_ny * q.y + best_nz * q.z + best_d);

      if (!is_spike[i] && dist > threshold)
      {
        is_spike[i] = true; // Not armful data race
      }
    }
  } // End parallel region

  if (params.verbose)
  {
    const auto t1 = clock::now();
    const double elapsed_sec = std::chrono::duration<double>(t1 - t0).count();
    std::string timing = "  Detecting outlier vertices done in " + std::to_string(elapsed_sec) + " s";
    log(timing);
  }
}

std::vector<unsigned int> PTD::get_ground_fid() const
{
  std::vector<unsigned int> idx;
  idx.reserve(d->vcount);
  unsigned int offset = (4 + vbuff.size() + 1);

  for (unsigned int i = offset; i < (unsigned int)d->vcount; i++)
  {
    unsigned int fid = d->vertices[i].pos.fid;
    if (!is_spike[i]) idx.push_back(fid);
  }

  return idx;
}

std::vector<unsigned int> PTD::get_spikes_fid() const
{
  std::vector<unsigned int> idx;
  idx.reserve(d->vcount);
  unsigned int offset = (4 + vbuff.size() + 1);

  for (unsigned int i = offset; i < (unsigned int) d->vcount; i++)
  {
    unsigned int fid = d->vertices[i].pos.fid;
    if (is_spike[i]) idx.push_back(fid);
  }

  return idx;
}

bool axelsson_metrics(const Vec3& P, const Vec3& A, const Vec3& B, const Vec3& C, double& dist_d, double& angle)
{
  // Calculate the normal of the triangle (A->B cross A->C)
  Vec3 edge1 = B - A;
  Vec3 edge2 = C - A;
  Vec3 n = edge1.cross(edge2).normalize();

  // Distance from P to the plane
  Vec3 v = P - A;
  dist_d = std::abs(v.dot(n));

  // Project P onto the plane
  Vec3 P_proj = P - n * v.dot(n);

  // Manual "contains" check (Barycentric Coordinates)
  // Using the same edges defined above
  Vec3 v2 = P_proj - A;
  double d00 = edge1.dot(edge1);
  double d01 = edge1.dot(edge2);
  double d11 = edge2.dot(edge2);
  double d20 = v2.dot(edge1);
  double d21 = v2.dot(edge2);
  double denom = d00 * d11 - d01 * d01;
  if (std::abs(denom) < 1e-9) return false;
  double v_coord = (d11 * d20 - d01 * d21) / denom;
  double w_coord = (d00 * d21 - d01 * d20) / denom;
  double u_coord = 1.0 - v_coord - w_coord;
  if (u_coord < 0 || v_coord < 0 || w_coord < 0) return false;

  // Calculate metrics
  double h0 = P_proj.distance(A);
  double h1 = P_proj.distance(B);
  double h2 = P_proj.distance(C);
  double alpha = std::atan2(dist_d, h0) * 180.0 / M_PI;
  double beta  = std::atan2(dist_d, h1) * 180.0 / M_PI;
  double gamma = std::atan2(dist_d, h2) * 180.0 / M_PI;

  angle = std::max({alpha, beta, gamma});

  return true;
}

double distance_to_fitted_plane(const Point& query, const std::vector<size_t>& neighbor_indices, const IncrementalDelaunay::Vertex* vertices)
{
  if (neighbor_indices.size() < 3)
    return 0.0;

  // --- Centroid for numerical stability ---
  double cx = 0, cy = 0, cz = 0;
  for (size_t idx : neighbor_indices)
  {
    cx += vertices[idx].pos.x;
    cy += vertices[idx].pos.y;
    cz += vertices[idx].pos.z;
  }
  cx /= neighbor_indices.size();
  cy /= neighbor_indices.size();
  cz /= neighbor_indices.size();

  // --- Least squares fit: z' = a*x' + b*y' ---
  double sxx = 0, sxy = 0, syy = 0;
  double sxz = 0, syz = 0;

  for (size_t idx : neighbor_indices)
  {
    double dx = vertices[idx].pos.x - cx;
    double dy = vertices[idx].pos.y - cy;
    double dz = vertices[idx].pos.z - cz;

    sxx += dx * dx;
    sxy += dx * dy;
    syy += dy * dy;
    sxz += dx * dz;
    syz += dy * dz;
  }

  double det = sxx * syy - sxy * sxy;

  // Degenerate XY configuration
  if (std::abs(det) < 1e-9)
    return 0.0;

  double a = (syy * sxz - sxy * syz) / det;
  double b = (sxx * syz - sxy * sxz) / det;

  // Predicted Z at query position
  double qdx = query.x - cx;
  double qdy = query.y - cy;
  double predicted_z = (a * qdx + b * qdy) + cz;

  // Vertical residual
  double vertical_residual = query.z - predicted_z;

  // Convert to perpendicular distance
  // Plane normal = (-a, -b, 1)
  double normal_length = std::sqrt(a * a + b * b + 1.0);

  return vertical_residual / normal_length;
}

void PTD::sample_lowest_per_cell(const std::vector<Point>& points, IncrementalDelaunay::Grid& grid, std::vector<Point>& out)
{
  int n  = 1;
  const int ncells = grid.get_ncells();

  // Per-cell storage
  std::vector<std::vector<Point>> cells(ncells);
  cells.reserve(ncells);

  for (const auto& p : points)
  {
    int cell = grid.cell_from_xy(p.x, p.y);
    if (cell < 0) continue;

    auto& bucket = cells[cell];

    if ((int)bucket.size() < n)
    {
      bucket.push_back(p);
    }
    else
    {
      // Find current max-z point in this cell
      int imax = 0;
      double zmax = bucket[0].z;

      for (int i = 1; i < n; ++i)
      {
        if (bucket[i].z > zmax)
        {
          zmax = bucket[i].z;
          imax = i;
        }
      }

      // Replace worst point if new one is better
      if (p.z < zmax)
        bucket[imax] = p;
    }
  }

  // Output
  for (const auto& bucket : cells)
  {
    for (const auto& p : bucket)
      out.push_back(p);
  }
}

void PTD::deduplicate_seeds()
{
  std::stable_sort(seeds.begin(), seeds.end(),
            [](const Point& a, const Point& b) {
              if (a.x != b.x) return a.x < b.x;
              if (a.y != b.y) return a.y < b.y;
              return a.z < b.z;
            });

  seeds.erase(
    std::unique(seeds.begin(), seeds.end(),
                [](const Point& a, const Point& b) {
                  return a.x == b.x && a.y == b.y && a.z == b.z;
                }),
                seeds.end());
}

void PTD::log(const std::string& msg) const
{
  if (params.verbose && logger)
    logger(msg);
}




}
