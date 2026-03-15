#include "constants.h"
#include "pred3d.h"
#include "delaunay.h"

#include <algorithm>
#include <cmath>

static bool initialized = false;

#ifndef MAX3
#define MAX3(a,b,c) (((a)>(b)) ? (((a)>(c)) ? (a) : (c)) : (((b)>(c)) ? (b) : (c)))
#define MIN3(a,b,c) (((a)>(b)) ? (((b)>(c)) ? (c) : (b)) : (((a)>(c)) ? (c) : (a)))
#endif


namespace IncrementalDelaunay
{

Triangulation::Triangulation(const Grid& index_) : index(index_)
{
  int numP = 10000; // allocate memory for 10000 points

  if (!initialized)
  {
    exactinit();
    initialized = true;
  }

  double minx = -1e8;
  double miny = -1e8;
  double maxx = 1e8;
  double maxy = 1e8;

  a = std::max(maxx-minx,maxy-miny);

  p0 = Vec2(minx,miny) + Vec2(-a/10,-a/10);
  p1 = p0 + Vec2(a+2*a/10,0);
  p2 = p0 + Vec2(a+2*a/10,a+2*a/10);
  p3 = p0 + Vec2(0,a+2*a/10);

  maxVertices = numP+6;
  maxTriangles = numP*2+7;
  vertices = new Vertex[maxVertices]; // num of vertices
  triangles = new Triangle[maxTriangles]; // 2(n+6) - 2 - 3 = 2n+7 // num of faces

  vertices[0] = Vertex(Vec2(minx, miny));
  vertices[1] = Vertex(Vec2(maxx, miny));
  vertices[2] = Vertex(Vec2(maxx, maxy));
  vertices[3] = Vertex(Vec2(minx, maxy));

  triangles[0] = Triangle(0,1,2,-1,1,-1);
  triangles[1] = Triangle(0,2,3,-1,-1,0);

  vcount = 4;
  tcount = 2;

  grid.resize(index.get_ncells());
  dirty_cells.resize(index.get_ncells(), true);
  index_active = true;
}

Triangulation::~Triangulation()
{
  delete[] triangles;
  delete[] vertices;
}

bool Triangulation::delaunayInsertion(const Vec2& p, int prop)
{
  remem();

  int tri_index = prop; // assuming prop is the correct triangle from findContainerTriangleFast

  // Check if we are trying to insert into a frozen triangle
  if (tri_index == -1 || triangles[tri_index].frozen) {
    return false;
  }

  Vec2 a = vertices[triangles[tri_index].v[0]].pos;
  Vec2 b = vertices[triangles[tri_index].v[1]].pos;
  Vec2 c = vertices[triangles[tri_index].v[2]].pos;
  Vec2 points[] = {a,b,c};

  // Check for duplicates
  for (int i = 0 ; i < 3 ; i++)
  {
    if ((std::abs(p.x-points[i].x) < IN_TRIANGLE_EPS) && (std::abs(p.y-points[i].y) < IN_TRIANGLE_EPS))
    {
      return false;
    }
  }

  // --- Check Edges ---
  for (int i = 0; i < 3 ; i++)
  {
    if (pointInSegment(p, points[(i+1)%3], points[(i+2)%3]))
    {
      int t_neighbor = triangles[tri_index].t[i];

      if (t_neighbor == -1)
      {
        addPointInEdge(p, tri_index);
        // Created 2 new triangles from 1.
        // The modified triangles are tri_index and tcount-1.
        // Actually addPointInEdge(p, t) usually creates more splits, let's look at your implementation:
        // It creates t1 (new) and modifies t.
        legalize({tri_index, tcount - 1});
      }
      else
      {
        addPointInEdge(p, tri_index, t_neighbor);
        // addPointInEdge(p, t0, t1) creates t2, t3 and modifies t0, t1.
        // We need to legalize all 4.
        legalize({tri_index, t_neighbor, tcount - 1, tcount - 2});
      }

      return true;
    }
  }

  // --- Inside Triangle ---
  if (tri_index != -1)
  {
    this->incount++;
    addPointInside(p, tri_index);
    // addPointInside splits tri_index into 3: tri_index, tcount-2, tcount-1
    legalize({tri_index, tcount - 1, tcount - 2});

    return true;
  }

  return true;
}

int Triangulation::findContainerTriangle(const Vec2& p, int prop) const
{
  // 1. Heuristic: Start from the provided hint or the last triangle
  int t = (prop < 0 || prop >= tcount) ? tcount - 1 : prop;

  // Safety check for empty triangulation
  if (t < 0) return -1;

  // 2. Stochastic/Straight Walk
  // We remember the edge we entered from to avoid immediate backtracking.
  // -1 implies we didn't enter from a neighbor (start of search)
  int edge_from = -1;

  int iter = 0;
  const int MAX_ITER = tcount + 100; // Safety break for non-convex/invalid states

  while (iter++ < MAX_ITER)
  {
    const Triangle& tri = triangles[t];

    // Identify the vertices
    // Edge i is defined by vertices (i+1)%3 and (i+2)%3
    // The neighbor across edge i is tri.t[i]

    bool moved = false;

    // We check edges in a specific order: start checking the edge *after* the one we came from.
    // This optimization (Move-To-Front heuristic) reduces average tests per step.
    int start_edge = (edge_from == -1) ? 0 : (edge_from + 1) % 3;

    for (int k = 0; k < 3; k++)
    {
      int i = (start_edge + k) % 3;

      // If we just came from neighbor at i, we know the point is on the "inside"
      // side of that shared edge, so we skip it.
      if (i == edge_from) continue;

      int v1_idx = tri.v[(i + 1) % 3];
      int v2_idx = tri.v[(i + 2) % 3];

      // Robustness: Handle ghost triangles or uninitialized memory if necessary
      if (v1_idx == -1 || v2_idx == -1) continue;

      const Vec2& p1 = vertices[v1_idx].pos;
      const Vec2& p2 = vertices[v2_idx].pos;

      // Orient2d: Returns positive if p is to the left of p1->p2 (CCW)
      // If result < 0, p is to the Right. We must walk to neighbor t[i].
      // We also handle == 0 (on edge) cautiously. Standard walk moves if strictly right (<0).
      // However, for robustness, if it is exactly on the edge, we consider it "inside"
      // or "on edge" of the current triangle and do not jump, effectively stopping.
      if (orient2d(&p1.x, &p2.x, &p.x) < 0)
      {
        int neighbor = tri.t[i];

        // If we hit the boundary (neighbor is -1), the point is outside the triangulation
        if (neighbor == -1)
        {
          return -1;
        }

        // Move to neighbor
        // We need to calculate which edge index in 'neighbor' corresponds to 't'
        // to set 'edge_from' correctly for the next iteration.
        // Optimization: We don't strictly need to scan the neighbor's edges to find 't'
        // if we just want the simple walk. But finding the index allows the (i==edge_from) optimization.

        // Simple version (slightly slower but less code): edge_from = -1;
        // Optimized version: Find the edge index in 'neighbor' that points back to 't'

        const Triangle& n_tri = triangles[neighbor];
        if (n_tri.t[0] == t) edge_from = 0;
        else if (n_tri.t[1] == t) edge_from = 1;
        else edge_from = 2;

        t = neighbor;
        moved = true;
        break;
      }
    }

    // If we checked all relevant edges and didn't move, we are inside (or on the edge).
    if (!moved)
    {
      return t;
    }
  }

  // Fallback if max iterations reached (should not happen in valid Delaunay with bounding box)
  return -1;
}

int Triangulation::findContainerTriangleFast(const Vec2& p) const
{
  if (!index_active)
    return findContainerTriangle(p, -1);

  //auto start_time = std::chrono::high_resolution_clock::now();

  int cell = index.cell_from_xy(p.x, p.y);

  if (cell != -1)
  {
    // Search the candidate triangles in the grid cell
    const auto& candidate_triangles = grid[cell];
    //n_triangle_tested += candidate_triangles.size();

    // Search the candidate triangles for an exact match first
    for (int t : candidate_triangles)
    {
      // Check if the ID is valid
      if (t < tcount)
      {
        if (isInside(t, p) || isInEdge(t, p))
        {
          return t;
        }
      }
    }

    if (!candidate_triangles.empty())
    {
      return findContainerTriangle(p, candidate_triangles[0]);
    }
  }

  // Fallback if no match is found
  return findContainerTriangle(p, -1);
}

bool Triangulation::isInside(int t, Vec2 p) const
{
  if (t == -1)
    return false;

  if ((triangles[t].v[0] == -1) && (triangles[t].v[1] == -1) && (triangles[t].v[2] == -1))
    return false;

  Vec2 p1 = vertices[triangles[t].v[0]].pos;
  Vec2 p2 = vertices[triangles[t].v[1]].pos;
  Vec2 p3 = vertices[triangles[t].v[2]].pos;

  double lim;

  lim = MIN3(p1.x, p2.x, p3.x);
  if (p.x < lim) return false;

  lim = MAX3(p1.x, p2.x, p3.x);
  if (p.x > lim) return false;

  lim = MIN3(p1.y, p2.y, p3.y);
  if (p.y < lim) return false;

  lim = MAX3(p1.y, p2.y, p3.y);
  if (p.y > lim) return false;

  return (orient2d(&(p1.x),&(p2.x),&(p.x)) > 0) &&
    (orient2d(&(p2.x),&(p3.x),&(p.x)) > 0) &&
    (orient2d(&(p3.x),&(p1.x),&(p.x)) > 0);
}

bool Triangulation::isInside(int t, int v) const
{
  if (t==-1) return 0;

  return isInside(t, vertices[v].pos);
}

bool Triangulation::isInEdge(int t, Vec2 p) const
{
  if (t==-1) return false;

  if (triangles[t].v[0] == -1 && triangles[t].v[1] == -1 && triangles[t].v[2] == -1)
    return false;

  Vec2 p1 = vertices[triangles[t].v[0]].pos;
  Vec2 p2 = vertices[triangles[t].v[1]].pos;
  Vec2 p3 = vertices[triangles[t].v[2]].pos;

  double lim;

  lim = MIN3(p1.x, p2.x, p3.x);
  if (p.x < lim) return false;

  lim = MAX3(p1.x, p2.x, p3.x);
  if (p.x > lim) return false;

  lim = MIN3(p1.y, p2.y, p3.y);
  if (p.y < lim) return false;

  lim = MAX3(p1.y, p2.y, p3.y);
  if (p.y > lim) return false;

  double o1 = orient2d(&(p1.x), &(p2.x), &(p.x));
  double o2 = orient2d(&(p2.x), &(p3.x), &(p.x));
  double o3 = orient2d(&(p3.x), &(p1.x), &(p.x));

  // On an edge: exactly one orient2d is 0, others are > 0
  if (o1 < 0 || o2 < 0 || o3 < 0) return false;
  return (o1 == 0) || (o2 == 0) || (o3 == 0);
}

void Triangulation::addPointInside(const Vec2& v, int tri_index)
{
  remem();

  int f = tri_index;
  int f1 = tcount++;
  int f2 = tcount++;

  unindexTriangle(f);

  int p = vcount++;

  int p0 = triangles[f].v[0];
  int p1 = triangles[f].v[1];
  int p2 = triangles[f].v[2];
  int t1 = triangles[f].t[1];
  int t2 = triangles[f].t[2];

  triangles[f1] = Triangle(p,p2,p0,t1,f2,f);
  triangles[f2] = Triangle(p,p0,p1,t2,f,f1);

  if (t1 != -1)
  {
    if (triangles[t1].t[0] == f) triangles[t1].t[0] = f1;
    if (triangles[t1].t[1] == f) triangles[t1].t[1] = f1;
    if (triangles[t1].t[2] == f) triangles[t1].t[2] = f1;
  }

  if (t2 != -1)
  {
    if(triangles[t2].t[0]==f) triangles[t2].t[0] = f2;
    if(triangles[t2].t[1]==f) triangles[t2].t[1] = f2;
    if(triangles[t2].t[2]==f) triangles[t2].t[2] = f2;
  }

  triangles[f].v[0] = p;
  triangles[f].t[1] = f1;
  triangles[f].t[2] = f2;

  vertices[p] = Vertex(v,f);

  indexTriangle(f);
  indexTriangle(f1);
  indexTriangle(f2);
}

void Triangulation::addPointInEdge(const Vec2& v, int t0, int t1)
{
  remem();

  unindexTriangle(t0);
  unindexTriangle(t1);

  int p = vcount;
  vertices[vcount++] = Vertex(v);

  int t0_v = -1;
  int t1_v = -1;

  int f0,f3;
  int p1;

  for (int i = 0 ; i < 3 ; i++)
  {
    for (int j = 0 ; j < 3 ; j++)
    {
      if (triangles[t0].t[i]==t1 && triangles[t1].t[j]==t0)
      {
        t0_v = i;
        t1_v = j;
      }
    }
  }

  f0 = triangles[t0].t[(t0_v+1)%3];
  p1 = triangles[t0].v[(t0_v+2)%3];

  f3 = triangles[t1].t[(t1_v+2)%3];

  int t2 = tcount++;
  int t3 = tcount++;

  triangles[t2] = Triangle(p,p1,triangles[t0].v[t0_v],f0,t0,t3);
  if(f0!=-1)for(int i=0;i<3;i++)if(triangles[f0].t[i]==t0)triangles[f0].t[i] = t2;

  triangles[t3] = Triangle(p,triangles[t1].v[t1_v],p1,f3,t2,t1);
  if(f3!=-1)for(int i=0;i<3;i++)if(triangles[f3].t[i]==t1)triangles[f3].t[i] = t3;

  triangles[t0].v[(t0_v+2)%3] = p;
  triangles[t0].t[(t0_v+1)%3] = t2;
  triangles[t1].v[(t1_v+1)%3] = p;
  triangles[t1].t[(t1_v+2)%3] = t3;

  vertices[p].tri_index = t2;

  indexTriangle(t0);
  indexTriangle(t1);
  indexTriangle(t2);
  indexTriangle(t3);

  remem();
}

void Triangulation::addPointInEdge(const Vec2& v, int t)
{
  remem();

  unindexTriangle(t);

  int x = triangles[t].t[0] == -1 ? 0 : (triangles[t].t[1] == -1 ? 1 : 2);

  int f1 = triangles[t].t[x];
  int f2 = triangles[t].t[(x+1)%3];

  int p0 = triangles[t].v[(x+2)%3];
  int p1 = triangles[t].v[x];

  int t1 = tcount++;
  int p = vcount++;

  vertices[p] = Vertex(v);
  triangles[t1] = Triangle(p0,p1,p,t,f1,f2);
  triangles[t].v[(x+2)%3] = p;
  triangles[t].t[(x+1)%3] = t1;

  vertices[p].tri_index = t;

  if (f2 != -1)
  {
    triangles[f2].t[0] = (triangles[f2].t[0] == t ? t1 : triangles[f2].t[0]);
    triangles[f2].t[1] = (triangles[f2].t[1] == t ? t1 : triangles[f2].t[1]);
    triangles[f2].t[2] = (triangles[f2].t[2] == t ? t1 : triangles[f2].t[2]);
  }

  indexTriangle(t);
  indexTriangle(t1);

  remem();
}

void Triangulation::legalize(const std::vector<int>& input_triangles)
{
  m_flip_stack.clear();

  // 1. Initialize stack with edges of the newly inserted triangles
  // We only check edges that connect to existing triangles (neighbor != -1)
  for (int t : input_triangles)
  {
    for (int i = 0; i < 3; i++)
    {
      int neighbor = triangles[t].t[i];
      if (neighbor != -1)
      {
        m_flip_stack.push_back({t, neighbor});
      }
    }
  }

  // 2. Process the stack
  while (!m_flip_stack.empty())
  {
    auto [t1, t2] = m_flip_stack.back();
    m_flip_stack.pop_back();

    // Sanity check: Ensure they are still connected (topology might have changed due to previous flips)
    if (!areConnected(t1, t2)) continue;

    // If either triangle is frozen, we CANNOT flip the edge shared between them.
    // Flipping would change the geometry of the frozen triangle.
    if (triangles[t1].frozen || triangles[t2].frozen) {
      continue;
    }

    // --- Prepare vertices for InCircle test ---
    // We need to identify the vertices of the quad formed by t1 and t2
    // t1 vertices: a0, a1, a2
    // t2 vertices: b0, b1, b2
    // Shared edge: 2 vertices.
    // Opposing vertices: The ones not on the shared edge.

    int a[3] = {triangles[t1].v[0], triangles[t1].v[1], triangles[t1].v[2]};
    int b[3] = {triangles[t2].v[0], triangles[t2].v[1], triangles[t2].v[2]};

    // Find the vertex in T2 that is NOT in T1 (the point we test against T1's circumcircle)
    int p_opp_idx = -1;
    for (int i = 0; i < 3; i++)
    {
      if (b[i] != a[0] && b[i] != a[1] && b[i] != a[2])
      {
        p_opp_idx = b[i];
        break;
      }
    }

    // Find the vertex in T1 that is NOT in T2
    int t1_opp_idx = -1;
    for(int i = 0; i < 3; i++)
    {
      if(a[i] != b[0] && a[i] != b[1] && a[i] != b[2])
      {
        t1_opp_idx = a[i];
        break;
      }
    }

    // If data is inconsistent, skip
    if (p_opp_idx == -1 || t1_opp_idx == -1) continue;

    // --- Delaunay Test ---
    // If the quadrilateral is convex AND the point p_opp is inside the circumcircle of T1
    if (isConvexBicell(t1, t2) &&
        inCircle(vertices[a[0]].pos, vertices[a[1]].pos, vertices[a[2]].pos, vertices[p_opp_idx].pos) > 0)
    {
      // Perform the flip
      flip(t1, t2);

      // --- Update Stack ---
      // After flipping t1 and t2, the diagonal changed.
      // The new "external" edges of the t1+t2 quad might now be illegal.
      // We push the neighbors of t1 and t2 (excluding each other) to the stack.

      for (int i = 0; i < 3; i++)
      {
        int n1 = triangles[t1].t[i];
        if (n1 != -1 && n1 != t2) m_flip_stack.push_back({t1, n1});

        int n2 = triangles[t2].t[i];
        if (n2 != -1 && n2 != t1) m_flip_stack.push_back({t2, n2});
      }
    }
  }
}

// check if two triangles are neighbours
bool Triangulation::areConnected(int t1, int t2) const
{
  if (t1 == -1 || t2 == -1) return true;
  return (triangles[t1].t[0] == t2 ||
          triangles[t1].t[1] == t2 ||
          triangles[t1].t[2] == t2);
}

bool Triangulation::isCCW(int f) const
{
  if (f == -1) return true;

  Vec2 p0 = vertices[triangles[f].v[0]].pos;
  Vec2 p1 = vertices[triangles[f].v[1]].pos;
  Vec2 p2 = vertices[triangles[f].v[2]].pos;

  return (orient2d(&(p0.x),&(p1.x),&(p2.x)) > 0);

  // if((crossa(p0,p1)+crossa(p1,p2)+crossa(p2,p0))>IN_TRIANGLE_EPS) return true;
  // return false;
}


bool Triangulation::flip(int t1, int t2)
{
  unindexTriangle(t1);
  unindexTriangle(t2);

  int i;
  if (triangles[t1].t[0] == t2) { i = 0; }
  else if (triangles[t1].t[1] == t2) { i = 1; }
  else { i = 2; }

  int j;
  if (triangles[t2].t[0] == t1) { j = 0; }
  else if (triangles[t2].t[1] == t1) { j = 1; }
  else { j = 2; }

  int p10 = triangles[t1].v[i];
  int p11 = triangles[t1].v[(i+1)%3];
  int p12 = triangles[t1].v[(i+2)%3];

  int f10 = triangles[t1].t[i];
  int f11 = triangles[t1].t[(i+1)%3];
  int f12 = triangles[t1].t[(i+2)%3];

  int p20 = triangles[t2].v[j];
  int p21 = triangles[t2].v[(j+1)%3];
  int p22 = triangles[t2].v[(j+2)%3];

  int f20 = triangles[t2].t[j];
  int f21 = triangles[t2].t[(j+1)%3];
  int f22 = triangles[t2].t[(j+2)%3];

  triangles[t1].v[0] = p11;
  triangles[t1].v[1] = p20;
  triangles[t1].v[2] = p10;

  vertices[p11].tri_index = t1;
  vertices[p20].tri_index = t1;
  vertices[p10].tri_index = t1;

  triangles[t1].t[0] = t2;
  triangles[t1].t[1] = f12;
  triangles[t1].t[2] = f21;

  triangles[t2].v[0] = p12;
  triangles[t2].v[1] = p10;
  triangles[t2].v[2] = p20;

  vertices[p12].tri_index = t2;

  triangles[t2].t[0] = t1;
  triangles[t2].t[1] = f22;
  triangles[t2].t[2] = f11;

  if (f11 != -1)
  {
    for (int k = 0 ; k < 3 ; k++)
    {
      if (triangles[f11].t[k] == t1)
      {
        triangles[f11].t[k] = t2;
      }
    }
  }

  if (f21 != -1)
  {
    for(int k = 0 ; k < 3 ; k++)
    {
      if (triangles[f21].t[k] == t2)
      {
        triangles[f21].t[k] = t1;
      }
    }
  }

  indexTriangle(t1);
  indexTriangle(t2);

  return true;
}

/*bool Triangulation::sanity(int t)
{
  if (t==-1) return true;

  for(int i = 0 ; i < 3 ; i++)
  {
    int count = 0;
    int f = triangles[t].t[i];
    if (f==-1) continue;
    for(int j = 0 ; j < 3 ; j++)
    {
      for(int k = 0 ; k < 3 ; k++)
      {
        if(triangles[t].v[k]==triangles[f].v[j])count++;
      }
    }
    if(count != 2) return false;
  }
  return true;
}*/

void Triangulation::remem()
{
  // we must get more space
  if (tcount >= maxTriangles-4)
  {
    Triangle *newTriangles = new Triangle[maxTriangles*2];
    std::copy(triangles, triangles+tcount, newTriangles);
    delete[] triangles;
    triangles = newTriangles;

    maxTriangles *= 2;
  }

  // we must get more space
  if (vcount >= maxVertices-3)
  {
    Vertex *newVertices = new Vertex[maxVertices*2];
    std::copy(vertices,vertices+vcount,newVertices);
    delete[] vertices;
    vertices = newVertices;
    maxVertices *= 2;
  }
}

// We assume that both bicells are ccw oriented
bool Triangulation::isConvexBicell(int t1, int t2)
{
  int i,j; // find which are the different indices

  for(i = 0 ; i < 3 ; i++)
  {
    if (triangles[t1].v[i] != triangles[t2].v[0] &&
        triangles[t1].v[i] != triangles[t2].v[1] &&
        triangles[t1].v[i] != triangles[t2].v[2])
      break;
  }

  for(j = 0 ; j < 3 ; j++)
  {
    if (triangles[t2].v[j]!=triangles[t1].v[0] &&
        triangles[t2].v[j]!=triangles[t1].v[1] &&
        triangles[t2].v[j]!=triangles[t1].v[2]
    )
      break;
  }

   /*                   i
   *                    ^
   *                   / \
   *                  /   \
   *                 /     \
   *        (i+1)%3 <-------> (j+1)%3
   *                 \     /
   *                  \   /
   *                   \ /
   *                    v
   *                    j
   */

  std::vector<Vec2> bicell = {vertices[triangles[t1].v[(i+1)%3]].pos, vertices[triangles[t2].v[j]].pos, vertices[triangles[t2].v[(j+1)%3]].pos, vertices[triangles[t1].v[i]].pos};

  for( i = 0 ; i < 4 ; i++)
  {
    Vec2 p0 = bicell[(i-1+4)%4];
    Vec2 p1 = bicell[i];
    Vec2 p2 = bicell[(i+1)%4];
    // Vec2 prev = p1-p0;
    // Vec2 act = p2-p1;
    // if(crossa(prev,act)<0) return false;
    if(orient2d(&(p0.x),&(p1.x),&(p2.x))<=0) return false;
  }

  return true;
}

/*double Triangulation::orient2d(const Vec2& pa, const Vec2& pb, const Vec2& pc) const
{
  return orient2d(pa, pb, pc);

  //double det = (pb.x - pa.x) * (pc.y - pa.y) - (pb.y - pa.y) * (pc.x - pa.x);
  //if (std::abs(det) < IN_TRIANGLE_EPS) det = 0.0;
  //return det;
}*/

double Triangulation::inCircle(const Vec2& a, const Vec2& b, const Vec2& c, const Vec2& d) const
{
  return incircle(&(a.x), &(b.x), &(c.x), &(d.x));

  // Compute the determinant of the 4x4 matrix
  /*double ax = a.x - d.x;
  double ay = a.y - d.y;
  double bx = b.x - d.x;
  double by = b.y - d.y;
  double cx = c.x - d.x;
  double cy = c.y - d.y;

  double det = (ax * ax + ay * ay) * (bx * cy - by * cx) -
    (bx * bx + by * by) * (ax * cy - ay * cx) +
    (cx * cx + cy * cy) * (ax * by - ay * bx);

  if (std::abs(det) < IN_CIRCLE_EPS) det = 0.0; // Cocircular

  return det;*/
}

bool Triangulation::pointInSegment(const Vec2& p, const Vec2& p1, const Vec2& p2) const
{
  if (p1 == p2) return false;
  if (p.x < std::min(p1.x, p2.x) - IN_TRIANGLE_EPS) return false;
  if (p.x > std::max(p1.x, p2.x) + IN_TRIANGLE_EPS) return false;
  if (p.y < std::min(p1.y, p2.y) - IN_TRIANGLE_EPS) return false;
  if (p.y > std::max(p1.y, p2.y) + IN_TRIANGLE_EPS) return false;

  Vec2 a = p1-p2;
  Vec2 n = Vec2(-a.y,a.x);

  return std::abs((p-p1).dot(n)) < IN_TRIANGLE_EPS;
}

void Triangulation::unindexTriangle(int t)
{
  if (!index_active) return;
  if (t < 0 || t >= tcount) return;

  Vec2 p1 = vertices[triangles[t].v[0]].pos;
  Vec2 p2 = vertices[triangles[t].v[1]].pos;
  Vec2 p3 = vertices[triangles[t].v[2]].pos;

  double min_x = std::min({p1.x, p2.x, p3.x});
  double max_x = std::max({p1.x, p2.x, p3.x});
  double min_y = std::min({p1.y, p2.y, p3.y});
  double max_y = std::max({p1.y, p2.y, p3.y});

  index.get_cells(min_x, min_y, max_x, max_y, m_temp_cells);

  for (int cell_index : m_temp_cells)
  {
    dirty_cells[cell_index] = true;
    auto& cell = grid[cell_index];

    // Remove ALL occurrences, not just the first
    size_t write = 0;
    for (size_t read = 0; read < cell.size(); ++read)
    {
      if (cell[read] != t)
      {
        cell[write++] = cell[read];
      }
    }
    cell.resize(write);
  }
}

void Triangulation::indexTriangle(int t)
{
  if (!index_active) return;

  if (t < 0 || t >= tcount) return;

  // Get the triangle's AABB
  Vec2 p1 = vertices[triangles[t].v[0]].pos;
  Vec2 p2 = vertices[triangles[t].v[1]].pos;
  Vec2 p3 = vertices[triangles[t].v[2]].pos;

  double min_x = std::min({p1.x, p2.x, p3.x});
  double max_x = std::max({p1.x, p2.x, p3.x});
  double min_y = std::min({p1.y, p2.y, p3.y});
  double max_y = std::max({p1.y, p2.y, p3.y});

  // 2. Map AABB to Grid Coordinates
  index.get_cells(min_x, min_y, max_x, max_y, m_temp_cells);

  // 3. Insert t into all overlapping cells
  for (auto cell : m_temp_cells)
  {
    dirty_cells[cell] = true; // Mark this region as modified
    grid[cell].push_back(t);
  }
}

void Triangulation::desactivate_spatial_index()
{
  index_active = false;

  for (auto& inner : grid)
    inner.clear();
}

void Triangulation::activate_spatial_index()
{
  index_active = true;

  for (auto& inner : grid)
    inner.clear();

  for (int i = 0 ; i < tcount ; ++i)
    indexTriangle(i);
}

void Triangulation::reset_dirty_cells()
{
  std::fill(dirty_cells.begin(), dirty_cells.end(), false);
}

const std::vector<bool>& Triangulation::get_dirty_cells() const
{
  return dirty_cells;
}

void Triangulation::set_frozen(int t, bool frozen)
{
  if (t >= 0 && t < tcount) {
    triangles[t].frozen = frozen;
  }
}

bool Triangulation::is_frozen(int t) const
{
  if (t >= 0 && t < tcount) {
    return triangles[t].frozen;
  }
  return false;
}

/*
#include <iostream>
#include <fstream>

void Triangulation::write(const std::string& filename) const
{
  std::ofstream outfile(filename);

  if (!outfile.is_open())
  {
    throw std::runtime_error("Error: Could not open file " + filename + " for writing.");
  }

  outfile << "# 2.5D Delaunay Triangulation Export\n";

  // 1. Write Vertices (starting from index 4)
  // We skip the first 4 vertices entirely.
  for (int i = 4; i < vcount; ++i) {
    const Vec2& p = vertices[i].pos;
    outfile << "v " << p.x << " " << p.y << " " << p.z << "\n";
  }

  outfile << "\n";

  // 2. Write Faces (Triangles)
  int facesWritten = 0;
  for (int i = 0; i < tcount; ++i) {
    int v0 = triangles[i].v[0];
    int v1 = triangles[i].v[1];
    int v2 = triangles[i].v[2];

    // Only export if the triangle does NOT connect to any super-structure vertex
    if (v0 > 3 && v1 > 3 && v2 > 3) {
      // Recomputing the index:
      // NewIndex = OriginalIndex - 4 (to account for skipped vertices)
      // .obj Index = NewIndex + 1 (1-based indexing)
      // Result: OriginalIndex - 3
      outfile << "f " << (v0 - 3) << " "
              << (v1 - 3) << " "
              << (v2 - 3) << "\n";
      facesWritten++;
    }
  }

  outfile.close();
}*/

}