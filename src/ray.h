#ifndef RAYH
#define RAYH

#include "float.h"
#include "point2.h"
#include "vec2.h"

struct Ray {
  Ray() {};
  Ray(point2f _o, vec2f _d) : o(_o), d(_d) {};
  ~Ray() {};
  point2f operator()(Float t) const { return o + d * t; }
  point2f origin() const {return(o);}
  vec2f direction() const {return(d);}
  std::string PrintInfo();
  point2f o;
  vec2f d;
};

#endif
