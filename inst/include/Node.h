#ifndef NODE_H
#define NODE_H

#include "Point.h"

namespace lidR
{

namespace Node
{

struct Quadnode
{
  Quadnode();
  Quadnode(unsigned char parent_level, unsigned char parent_xloc, unsigned char parent_yloc, int parent_pos, unsigned char _pos);

  unsigned char pos;
  unsigned char level;
  unsigned char xLocCode;
  unsigned char yLocCode;
  int parent;
  int firstChild;
  std::vector<PointXYZ> points;
};

inline Quadnode::Quadnode()
{
  pos = 0;
  level = 0;
  xLocCode = 0;
  yLocCode = 0;
  parent = -1;
  firstChild = -1;
}

inline Quadnode::Quadnode(unsigned char parent_level, unsigned char parent_xloc, unsigned char parent_yloc, int parent_pos, unsigned char _pos)
{
  level = parent_level - 1;
  parent = parent_pos;
  pos = _pos;

  unsigned char onx = (pos & (1 << 0)) >> 0;
  unsigned char ony = (pos & (1 << 1)) >> 1;

  xLocCode = parent_xloc;
  yLocCode = parent_yloc;

  xLocCode = ((onx << level) | xLocCode);
  yLocCode = ((ony << level) | yLocCode);

  firstChild = -1;
}

struct Ocnode
{
  Ocnode();
  Ocnode(unsigned char parent_level, unsigned char parent_xloc, unsigned char parent_yloc,  unsigned char parent_zloc, int parent_pos, unsigned char _pos);

  unsigned char pos;
  unsigned char level;
  unsigned char xLocCode;
  unsigned char yLocCode;
  unsigned char zLocCode;
  int parent;
  int firstChild;
  std::vector<PointXYZ> points;
};

inline Ocnode::Ocnode()
{
  pos = 0;
  level = 0;
  xLocCode = 0;
  yLocCode = 0;
  zLocCode = 0;
  parent = -1;
  firstChild = -1;
}

inline Ocnode::Ocnode(unsigned char parent_level, unsigned char parent_xloc, unsigned char parent_yloc, unsigned char parent_zloc, int parent_pos, unsigned char _pos)
{
  level = parent_level - 1;
  parent = parent_pos;
  pos = _pos;

  unsigned char onx = (pos & (1 << 0)) >> 0;
  unsigned char ony = (pos & (1 << 1)) >> 1;
  unsigned char onz = (pos & (1 << 2)) >> 2;

  xLocCode = parent_xloc;
  yLocCode = parent_yloc;
  zLocCode = parent_zloc;

  xLocCode = ((onx << level) | xLocCode);
  yLocCode = ((ony << level) | yLocCode);
  zLocCode = ((onz << level) | zLocCode);

  firstChild = -1;
}

}
}

#endif //NODE_H
