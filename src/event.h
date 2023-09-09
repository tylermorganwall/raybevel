#ifndef EVENTH
#define EVENTH

#include "float.h"
#include "point2.h"
#include "linesegment.h"
#include <iostream>

struct LAVertex;

struct Event {
  Event() : distance(std::numeric_limits<double>::infinity()), none(true) {}
  Event(Float distance,
        point2f intersection,
        LAVertex* vertex_a,
        LineSegment opposite_edge,
        bool split) :
    distance(distance),
    intersection(intersection),
    vertex_a(vertex_a),
    opposite_edge(opposite_edge),
    split(split),
    none(false) {}
  Event(Float distance,
        point2f intersection,
        LAVertex* vertex_a,
        LAVertex* vertex_b,
        bool split) :
    distance(distance),
    intersection(intersection),
    vertex_a(vertex_a),
    vertex_b(vertex_b),
    split(split), none(false) {}
  bool IsObsolete();
  bool SharesParent(Event e2, Float tol);
  std::string PrintInfo();
  friend std::ostream& operator<<(std::ostream& os, const Event& e);
  Float distance;
  point2f intersection;
  LAVertex* vertex_a;
  LAVertex* vertex_b;
  LineSegment opposite_edge;
  void printInfo();
  bool split;
  bool none;
};

struct CompareEvents {
  bool operator()(Event const& e1, Event const& e2) {
    return e1.distance > e2.distance;
  }
};

std::ostream& operator<<(std::ostream& os, const Event& e);


#endif
