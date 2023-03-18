#ifndef LINESEGMENTH
#define LINESEGMENTH

#include "point2.h"
#include "float.h"
#include "vec2.h"

struct LineSegment {
  LineSegment() {};
  LineSegment(point2f _origin, point2f _adjacent) :
    origin(_origin), adjacent(_adjacent) {
    d = adjacent - origin;
  };
  LineSegment(const LineSegment &l) {
    origin = l.origin;
    adjacent = l.adjacent;
    d = l.d;
  }
  ~LineSegment() {};
  inline bool is_equivalent(LineSegment l, Float tolerance) {
    return((origin.is_equivalent(l.origin,tolerance) &&
           adjacent.is_equivalent(l.adjacent,tolerance)) ||
           (origin.is_equivalent(l.adjacent,tolerance) &&
           adjacent.is_equivalent(l.origin,tolerance)));
  }
  inline Float distance_to_point(point2f p) {
      // Return minimum distance between line segment vw and point p
      const Float l2 = d.squared_length();  // i.e. |w-v|^2 -  avoid a sqrt
      if (l2 == 0.0) return (p-origin).length();   // v == w case
      // Consider the line extending the segment, parameterized as v + t (w - v).
      // We find projection of point p onto the line.
      // It falls where t = [(p-v) . (w-v)] / |w-v|^2
      // We clamp t from [0,1] to handle points outside the segment vw.
      const float t = std::fmax(0, std::fmin(1, dot(p - origin, d) / l2));
      const point2f projection = origin + t * d;  // Projection falls on the segment
      return((p-projection).length());
  }
  point2f origin;
  point2f adjacent;
  vec2f d;
};

#endif
