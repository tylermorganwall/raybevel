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

std::string Event::PrintInfo() {
  // std::ostringstream os;
  // if(split) {
  //   os << distance << " Split event @ " <<  intersection << " from " << vertex_a->vertex_id << " to " << opposite_edge.id;
  // } else {
  //   os << distance << " Edge event @ " <<  intersection << " between " << vertex_a->vertex_id << " & " << vertex_b->vertex_id;
  // }
  // return(os.str());
  if(split) {
    return(vformat("%.10f Split event @ %s from %s to %s",
                   distance,
                   intersection.PrintInfo().c_str(),
                   vertex_a->PrintInfo().c_str(),
                   opposite_edge.PrintInfo().c_str()));
  } else {
    return(vformat("%.10f Edge event @ %s between %s and %s",
                   distance,
                   intersection.PrintInfo().c_str(),
                   vertex_a->PrintInfo().c_str(),
                   vertex_b->PrintInfo().c_str()));
  }
}

std::ostream& operator<<(std::ostream& os, const Event& e) {
  if(e.split) {
    os << e.distance << " Split event @ " <<  e.intersection << " from " << e.vertex_a->vertex_id << " to " << e.opposite_edge.id;
  } else {
    os << e.distance << " Edge event @ " <<  e.intersection << " between " << e.vertex_a->vertex_id << " & " << e.vertex_b->vertex_id;
  }
  return(os);
}
