#include "lav.h"
#include "slav.h"
#include "lavertex.h"
#include "vec2.h"
#include "point2.h"
#include <algorithm>
#include  <memory>

void LAV::invalidate(LAVertex* vertex) {
  if(!(vertex->lav == this)) {
    throw std::runtime_error("rayskeleton: attempted to invalidate vertex not owned by this LAV");
  }
  vertex->is_valid = false;
  if(head.get() == vertex) {
    head = head->next;
  }
  vertex->lav = nullptr;
}


std::shared_ptr<LAVertex> LAV::unify(LAVertex* vertex_a, LAVertex* vertex_b, point2f point) {
  //Generate a new LAVertex from input point2f and resolves adjacency to old edges.
  //Returns:
  //LAVertex of intersection point.
  vec2f normed_b_bisector = unit_vector(vertex_b->bisector.d);
  vec2f normed_a_bisector = unit_vector(vertex_a->bisector.d);
  std::shared_ptr<LAVertex> replacement = std::make_shared<LAVertex>(
    point,
    vertex_a->edge_left,
    vertex_b->edge_right,
    this,
    normed_b_bisector,
    normed_a_bisector,
    slav->current_vertex_id,
    vertex_a->tol
  );
  if(head.get() == vertex_a || head.get() == vertex_b) {
    head = replacement;
  }

  vertex_a->prev->next = replacement;
  vertex_b->next->prev = replacement;
  replacement->prev = vertex_a->prev;
  replacement->next = vertex_b->next;

  vertex_a->invalidate();
  vertex_b->invalidate();
  len -= 1;
  return(replacement);
}

