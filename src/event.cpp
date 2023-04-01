#include "event.h"
#include "lavertex.h"

static char buff2[32];


std::ostream& operator<<(std::ostream& os, const Event& e) {
  if(e.split) {
    os << e.distance << " Split event @ " <<  e.intersection << " from " << e.vertex_a->vertex_id << " to " << e.opposite_edge.id;
  } else {
    os << e.distance << " Edge event @ " <<  e.intersection << " between " << e.vertex_a->vertex_id << " & " << e.vertex_b->vertex_id;
  }
  return(os);
}
