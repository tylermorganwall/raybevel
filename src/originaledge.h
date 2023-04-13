#ifndef ORIGINALEDGE
#define ORIGINALEDGE

#include "float.h"
#include "point2.h"
#include "vec2.h"
#include "linesegment.h"
#include "ray.h"
#include "Rcpp.h"

struct OriginalEdge {
  OriginalEdge() {}
  OriginalEdge(point2f vertex, point2f prev_vertex,
               Ray bisector, Ray prev_bisector, int id) {
    Edge = LineSegment(prev_vertex, vertex, id);
    PrevBisector = prev_bisector;
    Bisector = bisector;
  }
  LineSegment Edge;
  Ray PrevBisector;
  Ray Bisector;
};

#endif
