#ifndef SUBTREEH
#define SUBTREEH

#include "float.h"
#include "point2.h"

struct Subtree {
  Subtree() : none(true) {}
  Subtree(point2f source,
          Float distance,
          std::vector<point2f> sinks) :
    source(source), distance(distance), sinks(sinks), none(false) {}
  point2f source;
  Float distance;
  std::vector<point2f> sinks;
  bool none;
};


#endif
