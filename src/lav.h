#ifndef LAVH
#define LAVH

// DONE

#include "float.h"
#include "point2.h"
#include "vec2.h"
#include "lavertex.h"
#include "linesegment.h"
#include <vector>
#include "Rcpp.h"

struct SLAV;
struct LAVertex;

struct LAV {
  LAV(std::vector<point2f>& polygon,
      Float tol,
      SLAV* slav,
      int& current_vertex_id);
  LAV(LAVertex* _head, SLAV* slav);
  void invalidate(LAVertex* vertex);
  LAVertex* unify(LAVertex* vertex_a, LAVertex* vertex_b, point2f point);
  LAVertex* unify_multi(LAVertex* vertex_a, LAVertex* vertex_b, point2f point);
  LAVertex* head;
  SLAV* slav;
  size_t len;
  Float tol;
  bool none;
};

#endif
