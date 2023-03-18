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

struct LAV {
  LAV(std::vector<point2f>& polygon,
      Float tol,
      SLAV* slav,
      int& current_vertex_id) : slav(slav) {
    size_t full_length = polygon.size();
    head = std::make_shared<LAVertex>(polygon[0],
                                      LineSegment(polygon[full_length-2],polygon[0]),
                                      LineSegment(polygon[0],polygon[1]),
                                      this,
                                      current_vertex_id,
                                      tol);
    head->next = head;
    head->prev = head;
    none = false;
    for(size_t i = 1; i < full_length-1; i++) {
      std::shared_ptr<LAVertex> vert = std::make_shared<LAVertex>(
        polygon[i],
        LineSegment(polygon[i-1],polygon[i]),
        LineSegment(polygon[i],polygon[i+1]),
        this,
        current_vertex_id,
        tol
      );
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
  LAV(std::shared_ptr<LAVertex> _head, SLAV* slav) : slav(slav) {
    head = _head;
    head->lav = this;
    head->lav->len = 1;
    std::shared_ptr<LAVertex> cur = head->next;
    while(cur.get() != head.get()) {
      cur->lav = this;
      cur->lav->len += 1;
    }
  }
  void invalidate(LAVertex* vertex);
  std::shared_ptr<LAVertex> unify(LAVertex* vertex_a, LAVertex* vertex_b, point2f point);
  std::shared_ptr<LAVertex> head;

  SLAV* slav;
  size_t len;
  Float tol;
  bool none;
};

#endif
