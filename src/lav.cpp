#include "lav.h"
#include "slav.h"
#include "lavertex.h"
#include "vec2.h"
#include "point2.h"
#include <algorithm>
#include  <memory>

LAV::LAV(std::vector<point2f>& polygon,
    Float tol,
    SLAV* slav,
    int& current_vertex_id) : slav(slav) {
  size_t full_length = polygon.size();
  std::shared_ptr<LAVertex> temp_head = std::make_shared<LAVertex>(polygon[0],
                                    LineSegment(polygon[full_length-2],polygon[0],current_vertex_id),
                                    LineSegment(polygon[0],polygon[1],current_vertex_id),
                                    this,
                                    current_vertex_id,
                                    tol);
  slav->all_vertices.push_back(temp_head);
  head = temp_head.get();
  head->next = head;
  head->prev = head;
  none = false;
  for(size_t i = 1; i < full_length-1; i++) {
    std::shared_ptr<LAVertex> temp_vert = std::make_shared<LAVertex>(
      polygon[i],
             LineSegment(polygon[i-1],polygon[i],current_vertex_id),
             LineSegment(polygon[i],polygon[i+1],current_vertex_id),
             this,
             current_vertex_id,
             tol
    );
    slav->all_vertices.push_back(temp_vert);
    LAVertex* vert = temp_vert.get();

    //Create circular references
    vert->next = head;       //Link the new last node back to the first
    vert->prev = head->prev; //Set the previous last node to be right before the new last node
    vert->prev->next = vert; //Set the previous last node's next node to the new last node
    head->prev = vert;       //Set the first node's previous node to the new last node
  }
  len = full_length - 1;
  //Ensure that head vertex is valid
  size_t counter = 0;
  size_t original_head_id = head->vertex_id;
  while(!head->is_straight && counter < len) {
    head = head->next;
    counter++;
    if(head->is_straight) {
      Rcpp::Rcout << "Changing head from vertex " << original_head_id << " to vertex " << head->vertex_id << "\n";
    }
  }
  if(counter == len) {
    //No valid vertices
    none = true;
  }
}

LAV::LAV(LAVertex* _head, SLAV* slav) : slav(slav) {
  head = _head;
  head->lav = this;
  head->lav->len = 1;
  LAVertex* cur = head->next;
  // slav->all_vertices.push_back(cur);

  while(cur != head) {
    // Rcpp::Rcout << cur->lav->len << "\n";
    cur->lav = this;
    cur->lav->len += 1;
    cur = cur->next;
  }
}

void LAV::invalidate(LAVertex* vertex) {
  if(!(vertex->lav == this)) {
    throw std::runtime_error("rayskeleton: attempted to invalidate vertex not owned by this LAV");
  }
  vertex->is_valid = false ;
  if(head == vertex) {
    head = head->next;
  }
  vertex->lav = nullptr;
}


LAVertex* LAV::unify(LAVertex* vertex_a, LAVertex* vertex_b, point2f point, bool merging = false) {
  //Generate a new LAVertex from input point2f and resolves adjacency to old edges.
  //Returns:
  //LAVertex of intersection point.
  vec2f normed_a_bisector = unit_vector(vertex_a->bisector.d);
  vec2f normed_b_bisector = unit_vector(vertex_b->bisector.d);

  Rcpp::Rcout << "Unifying vertices #" << vertex_a->vertex_id << " and #" << vertex_b->vertex_id << "\n";
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
  slav->all_vertices.push_back(replacement);
  if(!merging) {
    slav->new_verts.push_back(replacement.get());
  }
  if(head == vertex_a || head == vertex_b) {
    head = replacement.get();
  }

  vertex_a->prev->next = replacement.get();
  vertex_b->next->prev = replacement.get();
  replacement->prev = vertex_a->prev;
  replacement->next = vertex_b->next;
  slav->invalidate_verts.push_back(vertex_a);
  slav->invalidate_verts.push_back(vertex_b);

  // vertex_a->invalidate();
  // vertex_b->invalidate();

  // len -= 1;
  return(replacement.get());
}

