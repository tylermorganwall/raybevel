#ifndef LINESEGMENTH
#define LINESEGMENTH

#include "point2.h"
#include "float.h"
#include "vec2.h"
#include <algorithm>
#include "format.h"


struct LineSegment {
  LineSegment() {};
  LineSegment(point2f _origin, point2f _adjacent, int _id) :
    origin(_origin), adjacent(_adjacent) {
    d = adjacent - origin;
    id = _id;
  };
  LineSegment(point2f _origin, vec2f d) :
    origin(_origin), d(d), adjacent(_origin + d) {
    id = 0;
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
  std::string PrintInfo() {
    std::string s = vformat("LineSegment2(<%.2f, %.2f> to <%.2f, %.2f>)",
                            origin.x(), origin.y(), adjacent.x(), adjacent.y());
    return(s);
  }

  inline Float distance_to_point(point2f p) {
      Float l2 = d.squared_length();
      if (l2 == 0.0) {
        return((p-origin).length());
      }
      vec2f pa = p - origin;
      Float t = dot(pa, d) / l2; //Assume infinite line

      return((pa - t*d).length());
  }
  point2f origin;
  point2f adjacent;
  vec2f d;
  int id;
};

#endif
