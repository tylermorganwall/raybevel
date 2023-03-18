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
               Ray bisector, Ray prev_bisector) {
    Edge = LineSegment(prev_vertex, vertex);
    PrevBisector = prev_bisector;
    Bisector = bisector;
    Rcpp::Rcout << "Constructing Original Edge: Origin " << Edge.origin << " Adj: " << Edge.adjacent <<
      " PBi " << PrevBisector.o << " , " << PrevBisector.d << " Bi " << Bisector.o << " , " << Bisector.d <<
        "\n";

  }
  LineSegment Edge;
  Ray PrevBisector;
  Ray Bisector;
};

#endif
