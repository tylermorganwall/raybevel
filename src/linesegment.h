#ifndef LINESEGMENTH
#define LINESEGMENTH

#include "point2.h"
#include "float.h"
#include "vec2.h"
#include <algorithm>

struct LineSegment {
  LineSegment() {};
  LineSegment(point2f _origin, point2f _adjacent, int _id) :
    origin(_origin), adjacent(_adjacent) {
    d = adjacent - origin;
    id = _id;
  };
  LineSegment(const LineSegment &l) {
    origin = l.origin;
    adjacent = l.adjacent;
    d = l.d;
    id = l.id;
  }
  ~LineSegment() {};
  inline bool is_equivalent(LineSegment l, Float tolerance) {
    return((origin.is_equivalent(l.origin,tolerance) &&
           adjacent.is_equivalent(l.adjacent,tolerance)) ||
           (origin.is_equivalent(l.adjacent,tolerance) &&
           adjacent.is_equivalent(l.origin,tolerance)));
  }
  inline Float distance_to_point(point2f p) {
      Float l2 = d.squared_length();
      if (l2 == 0.0) {
        return((p-origin).length());
      }
      vec2f pa = p - origin;
      // Float t = std::max<Float>(0, std::min<Float>(1, dot(pa, d) / l2));
      // Float t = std::fmin(1,std::fmax(0,dot(pa, d) / l2));
      Float t = dot(pa, d) / l2;

      return((pa - t*d).length());
  }
  point2f origin;
  point2f adjacent;
  vec2f d;
  int id;
};

#endif
