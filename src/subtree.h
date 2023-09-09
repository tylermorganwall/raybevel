#ifndef SUBTREEH
#define SUBTREEH

#include "float.h"
#include "point2.h"

struct Subtree {
  Subtree() : none(true) {}
  Subtree(point2f source,
          Float distance,
          std::vector<point2f> sinks,
          std::vector<int> sink_ids,
          bool from_split) :
    source(source), distance(distance), sinks(sinks), sink_ids(sink_ids), none(false),
    from_split(from_split) {}
  point2f source;
  Float distance;
  std::vector<point2f> sinks;
  std::vector<int> sink_ids;
  bool none;
  bool from_split;
};


#endif
