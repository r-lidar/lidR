#ifndef BBOX3D_H
#define BBOX3D_H

template<typename T> struct BoundingBox3D
{
  T center, half_res;

  BoundingBox3D();
  BoundingBox3D(const T, const T);
  bool contains_2D(const T&);
  bool contains_3D(const T&);
  bool intersects_2D(const BoundingBox3D&);
  bool intersects_3D(const BoundingBox3D&);
};

template<typename T> BoundingBox3D<T>::BoundingBox3D(){}
template<typename T> BoundingBox3D<T>::BoundingBox3D(const T center, const T half_res) : center(center), half_res(half_res) {}

template<typename T> bool BoundingBox3D<T>::contains_2D(const T& p)
{
  if( p.x >= center.x - half_res.x &&
      p.x <= center.x + half_res.x &&
      p.y >= center.y - half_res.y &&
      p.y <= center.y + half_res.y )
    return true;
  else
    return false;
}

template<typename T> bool BoundingBox3D<T>::contains_3D(const T& p)
{
  if( p.x >= center.x - half_res.x &&
      p.x <= center.x + half_res.x &&
      p.y >= center.y - half_res.y &&
      p.y <= center.y + half_res.y &&
      p.z >= center.z - half_res.z &&
      p.z <= center.z + half_res.z )
    return true;
  else
    return false;
}

template<typename T> bool BoundingBox3D<T>::intersects_2D(const BoundingBox3D& b)
{
  if(center.x - half_res.x <= b.center.x + b.half_res.x &&
     center.x + half_res.x >= b.center.x - b.half_res.x &&
     center.y - half_res.y <= b.center.y + b.half_res.y &&
     center.y + half_res.y >= b.center.y - b.half_res.y)
  {
    return true;
  }
  else
    return false;
}

template<typename T> bool BoundingBox3D<T>::intersects_3D(const BoundingBox3D& b)
{
  if(center.x - half_res.x <= b.center.x + b.half_res.x &&
     center.x + half_res.x >= b.center.x - b.half_res.x &&
     center.y - half_res.y <= b.center.y + b.half_res.y &&
     center.y + half_res.y >= b.center.y - b.half_res.y &&
     center.z - half_res.z <= b.center.z + b.half_res.z &&
     center.z + half_res.z >= b.center.z - b.half_res.z )
  {
    return true;
  }
  else
    return false;
}

#endif //BBOXT3D_H

