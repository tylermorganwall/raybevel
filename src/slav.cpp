#include "slav.h"
#include "lavertex.h"
#include "subtree.h"
#include "event.h"

void SLAV::remove(LAV* lav) {
  for(size_t i = 0; i < lavs.size(); i++) {
    if(lavs[i].get() == lav) {
      lavs.erase(lavs.begin() + i);
    }
  }
}

std::pair<Subtree, std::vector<Event>> SLAV::handle_edge_event(Event event) {
  //     """Resolves adjacency of new edge event.
  //
  // This function resolves the edge event with previous edges, LAV, and then stores
  // edge information in a Subtree.
  //
  // Args:
  //   event: EdgeEvent
  //
  // Returns:
  //   Subtree namedTuple
  //   """
  std::vector<point2f> sinks;
  std::vector<Event> events;

  LAV* lav = event.vertex_a->lav;
  // # Triangle, one sink point
  if(event.vertex_a->prev->vertex.is_equivalent(event.vertex_b->next->vertex, tol)) {
    // log.info('%.2f Peak event at intersection %s from <%s,%s,%s> in %s',
    //          event.distance, event.intersection_point, event.vertex_a,
    //          event.vertex_b, event.vertex_a.prev, lav)
    std::shared_ptr<LAVertex> current_vert = lav->head;
    Rcpp::Rcout << "Vertex id: " << current_vert->vertex_id  << " " << current_vert->vertex << "\n";

    for(size_t i = 0; i < lav->len; i++) {
      sinks.push_back(current_vert->vertex);
      current_vert->invalidate();
      current_vert = current_vert->next;
    }
    remove(lav);
  } else {
    //     log.info('%.2f Edge event at intersection %s from <%s,%s> in %s',
    //              event.distance, event.intersection_point, event.vertex_a,
    //              event.vertex_b, lav)
    std::shared_ptr<LAVertex> new_vertex = lav->unify(event.vertex_a,
                                                      event.vertex_b,
                                                      event.intersection);
    if(lav->head.get() == event.vertex_a || lav->head.get() == event.vertex_b) {
      lav->head = new_vertex;
    }
    sinks.push_back(event.vertex_a->vertex);
    sinks.push_back(event.vertex_b->vertex);
    Event next_event = new_vertex->next_event();
    if(!next_event.none) {
      events.push_back(next_event);
    }
  }
  return(std::pair<Subtree, std::vector<Event> >(Subtree(event.intersection, event.distance, sinks), events));
}

std::pair<Subtree, std::vector<Event>> SLAV::handle_split_event(Event event) {
  // """Consumes a split event.
  //
  // This function resolves the adjacency of new split event with previous edges, LAV,
  // and then stores edge information in a Subtree.
  //
  // Args:
  // event: EdgeEvent
  //
  // Returns:
  // Subtree namedTuple
  // """
  //
  LAV* lav = event.vertex_a->lav;
  // log.info(
  //   '%.2f Split event at intersection %s from vertex %s, for edge %s in %s',
  //   event.distance,
  //   event.intersection_point,
  //   event.vertex,
  //   event.opposite_edge,
  //   lav)
  std::vector<point2f> sinks;
  std::vector<std::shared_ptr<LAVertex> > vertices;

  sinks.push_back(event.vertex_a->vertex);
  std::shared_ptr<LAVertex> x = nullptr;// = None  # right vertex
  std::shared_ptr<LAVertex> y = nullptr;// = None  # left vertex
  vec2f norm = unit_vector(event.opposite_edge.d);
  // for v in chain.from_iterable(self._lavs) {
  for(size_t i = 0; i < lavs.size(); i++) {
    std::shared_ptr<LAVertex> v = lavs[i]->head;
    for(size_t j = 0; j < lavs[i]->len; j++) {
      bool equal_to_edge_left_p = event.opposite_edge.origin.is_equivalent(
        v->edge_left.origin, tol);
      bool equal_to_edge_right_p = event.opposite_edge.origin.is_equivalent(
        v->edge_right.origin, tol);
      if(norm.is_equivalent(unit_vector(v->edge_left.d), tol) &&
         equal_to_edge_left_p) {
        x = v;
        y = x->prev;
      } else if (norm.is_equivalent(unit_vector(v->edge_right.d), tol) &&
                 equal_to_edge_right_p) {
        y = v;
        x = y->next;
      }
      if(x) {
        bool xleft = determinant(unit_vector(y->bisector.d),
                                 unit_vector(event.intersection - y->vertex)) >= 0;
        bool xright = determinant(unit_vector(x->bisector.d),
                                 unit_vector(event.intersection - x->vertex)) <= 0;

        // log.debug(
        //   'Vertex %s holds edge as %s edge (%s, %s)', v,
        //   ('left' if x == v else 'right'), xleft, xright)

        if(xleft && xright) {
          break;
        } else {
          x = nullptr;
          y = nullptr;
        }
      }
    }
  }
  if(!x) {
    // log.info(
    //   'Failed split event %s (equivalent edge event is expected to follow)',
    //   event
    // )
    std::vector<Event> empty_events;
    return(std::pair<Subtree, std::vector<Event>>(Subtree(), empty_events));
  }
  std::shared_ptr<LAVertex> v1 = std::make_shared<LAVertex>(
    event.intersection,
    event.vertex_a->edge_left,
    event.opposite_edge,
    current_vertex_id,
    tol
  );
  std::shared_ptr<LAVertex> v2 = std::make_shared<LAVertex>(
    event.intersection,
    event.opposite_edge,
    event.vertex_a->edge_right,
    current_vertex_id,
    tol
  );

  v1->prev = event.vertex_a->prev;
  v1->next = x;
  event.vertex_a->prev->next = v1;
  x->prev = v1;

  v2->prev = y;
  v2->next = event.vertex_a->next;
  event.vertex_a->next->prev = v2;
  y->next = v2;

  std::vector<std::shared_ptr<LAV> > new_lavs;
  remove(lav);
  if(lav != x->lav) {
    // the split event actually merges two lavs
    new_lavs.push_back(std::make_shared<LAV>(v1, this));
    remove(x->lav);
  } else {
    new_lavs.push_back(std::make_shared<LAV>(v1, this));
    new_lavs.push_back(std::make_shared<LAV>(v2, this));
  }
  for(size_t i = 0; i < new_lavs.size(); i++) {
    std::shared_ptr<LAV> lv = new_lavs[i];
    // log.debug(lv)
    if(lv->len > 2) {
      lavs.push_back(lv);
      vertices.push_back(lv->head);
    } else {
        // log.info(
        //   'LAV %s has collapsed into the line %s--%s',
        //   lv, lv.head.point, lv.head.next.point
        // )
      sinks.push_back(lv->head->next->vertex);
      LAVertex* cur = lv->head.get();
      for(size_t j = 0; j < lv->len; j++) {
        cur->invalidate();
        cur = cur->next.get();
      }
    }
  }
  std::vector<Event> events;
  for(size_t i = 0; i < vertices.size(); i++) {
    Event next_event = vertices[i]->next_event();
    if(!next_event.none) {
      events.push_back(next_event);
    }
  }
  event.vertex_a->invalidate();
  return(std::pair<Subtree, std::vector<Event> >(Subtree(event.intersection, event.distance, sinks), events));
}
