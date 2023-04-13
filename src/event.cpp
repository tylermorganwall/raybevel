#include "event.h"
#include "lavertex.h"
#include "point2.h"

static char buff2[32];

bool Event::IsObsolete() {
  return(split ? !vertex_a->is_valid : (!vertex_a->is_valid || !vertex_b->is_valid));
}

bool Event::SharesParent(Event e2, Float tol) {
  if(split && e2.split) {
    return(e2.vertex_a->vertex.is_equivalent(vertex_a->vertex, tol));
  } else {
    return(e2.vertex_a->vertex.is_equivalent(vertex_a->vertex, tol) ||
           e2.vertex_b->vertex.is_equivalent(vertex_b->vertex, tol) ||
           e2.vertex_a->vertex.is_equivalent(vertex_b->vertex, tol) ||
           e2.vertex_b->vertex.is_equivalent(vertex_a->vertex, tol));
  }
  return(false);
}

std::ostream& operator<<(std::ostream& os, const Event& e) {
  if(e.split) {
    os << e.distance << " Split event @ " <<  e.intersection << " from " << e.vertex_a->vertex_id << " to " << e.opposite_edge.id;
  } else {
    os << e.distance << " Edge event @ " <<  e.intersection << " between " << e.vertex_a->vertex_id << " & " << e.vertex_b->vertex_id;
  }
  return(os);
}
