#ifndef SLAVH
#define SLAVH

// DONE

#include "float.h"
#include "point2.h"
#include "vec2.h"
#include "lavertex.h"
#include "lav.h"
#include "originaledge.h"
#include "subtree.h"
#include <utility>


struct SLAV {
  SLAV(std::vector<std::vector<point2f> > contours, Float tol) {
    current_vertex_id = 0;
    for(size_t i = 0; i < contours.size(); i++) {
      lavs.push_back(std::make_shared<LAV>(contours[i], tol, this, current_vertex_id));
    }
    //We need the original edges because they drive the bisector angle
    for(size_t i = 0; i < lavs.size(); i++) {
      LAVertex* tmp = lavs[i]->head.get();
      for(size_t j = 0; j < lavs[i]->len; j++) {
        OriginalEdges.push_back(OriginalEdge(tmp->vertex,
                                             tmp->prev->vertex,
                                             tmp->bisector,
                                             tmp->prev->bisector));
        tmp = tmp->next.get();
      }
    }
  };
  size_t len() {
    return(lavs.size());
  }
  bool empty() {
    return(lavs.empty());
  }
  LAV* operator[](size_t i) {
    return(lavs[i].get());
  }
  void remove(LAV* lav);
  std::pair<Subtree, std::vector<Event>> handle_split_event(Event event);
  std::pair<Subtree, std::vector<Event>> handle_edge_event(Event event);

  Float tol;
  std::vector<std::shared_ptr<LAV> > lavs;
  std::vector<OriginalEdge> OriginalEdges;
  int current_vertex_id;
};


#endif
