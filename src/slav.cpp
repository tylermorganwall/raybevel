#include "slav.h"
#include "lavertex.h"
#include "subtree.h"
#include "event.h"

void SLAV::remove(LAV* lav) {
  // for(size_t i = 0; i < lavs.size(); i++) {
  //   if(lavs[i].get() == lav) {
  //     Rcpp::Rcout << "Erasing lav #" << i << " of " << lavs.size() << "\n";
  //     lavs.erase(lavs.begin() + i);
  //   }
  // }
  for(size_t i = 0; i < lavs.size(); i++) {
    if(lavs[i].get() == lav) {
      Rcpp::Rcout << "Erasing lav #" << i << " of " << lavs.size() << "\n";
      lav_valid[i] = false;
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
  // Rcpp::Rcout << "VA: " << event.vertex_a->prev->vertex << " VB: " << event.vertex_b->next->vertex << "\n";
  if(event.vertex_a->prev->vertex.is_equivalent(event.vertex_b->next->vertex, tol)) {
    Rcpp::Rcout << event.distance << " Peak event at intersection " <<  event.intersection << " from <" <<
      event.vertex_a->vertex_id << "," << event.vertex_b->vertex_id << "," << event.vertex_a->prev->vertex_id << ">\n";
    LAVertex* current_vert = lav->head;
    // Rcpp::Rcout << "Vertex id: " << current_vert->vertex_id  << " " << current_vert->vertex << "\n";
    // Rcpp::Rcout << "PEAK EVENT" << "\n";
    for(size_t i = 0; i < lav->len; i++) {
      sinks.push_back(current_vert->vertex);
      invalidate_verts.push_back(current_vert);
      // current_vert->invalidate();
      current_vert = current_vert->next;
    }
    events.push_back(event);
    remove(lav);
  } else {
    Rcpp::Rcout << event.distance << " Edge event at intersection " <<  event.intersection << " from <" <<
      event.vertex_a->vertex_id << "," << event.vertex_b->vertex_id << ">\n";
    //     log.info('%.2f Edge event at intersection %s from <%s,%s> in %s',
    //              event.distance, event.intersection_point, event.vertex_a,
    //              event.vertex_b, lav)
    LAVertex* new_vertex = lav->unify(event.vertex_a,
                                      event.vertex_b,
                                      event.intersection,
                                      false);
    if(lav->head == event.vertex_a || lav->head == event.vertex_b) {
      // Rcpp::Rcout << "Setting new head for vertex #" << new_vertex->vertex_id << "\n";
      lav->head = new_vertex;
    }
    sinks.push_back(event.vertex_a->vertex);
    sinks.push_back(event.vertex_b->vertex);
    Event next_event = new_vertex->next_event();
    if(!next_event.none) {
      events.push_back(next_event);
    } else {
      // Rcpp::Rcout << "NO EVENTS FOR VERTEX #" << new_vertex->vertex_id << "\n";
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
  Rcpp::Rcout << event.distance << " Split event at intersection " <<  event.intersection << " for vertex " <<
    event.vertex_a->vertex_id << " at " << event.opposite_edge.id << "\n";
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
  LAVertex* x = nullptr;// = None  # right vertex
  LAVertex* y = nullptr;// = None  # left vertex
  // vec2f norm = unit_vector(event.opposite_edge.d);
  int edge_id = event.opposite_edge.id;

  // for v in chain.from_iterable(self._lavs) {
  for(size_t i = 0; i < lavs.size(); i++) {
    if(lav_valid[i]) {
      LAVertex* v = lavs[i]->head;
      for(size_t j = 0; j < lavs[i]->len; j++) {
        // Rcpp::Rcout << v->vertex_id << "\n";
        bool equal_to_edge_left_p = edge_id == v->edge_left.id;
        bool equal_to_edge_right_p = edge_id == v->edge_right.id;

        if(equal_to_edge_left_p) {
          x = v;
          y = x->prev;
        } else if (equal_to_edge_right_p) {
          y = v;
          x = y->next;
        }
        if(x) {
          bool xleft = determinant(unit_vector(y->bisector.d),
                                   unit_vector(event.intersection - y->vertex)) <= tol;
          bool xright = determinant(unit_vector(x->bisector.d),
                                    unit_vector(event.intersection - x->vertex)) >= -tol;
          Rcpp::Rcout << "Vertex " << v->vertex_id << " holds edge as " << (x == v ? "left" : "right") <<
            " edge (" << xleft << "," << xright << ")\n";
          if(xleft && xright) {
            // Rcpp::Rcout << "vertex found\n";
            break;
          } else {
            x = nullptr;
            y = nullptr;
          }
        }
        v = v->next;
      }
    }
  }
  if(!x) {
    Rcpp::Rcout << "Failed split event at " << event.intersection << "\n";
    std::vector<Event> empty_events;
    lav->slav->error_return = false;
    return(std::pair<Subtree, std::vector<Event>>(Subtree(), empty_events));
  }
  if(event.vertex_a->edge_left.id == event.opposite_edge.id ||
     event.vertex_a->edge_right.id == event.opposite_edge.id) {
    Rcpp::Rcout << "Edge IDs identical \n";
    std::vector<Event> empty_events;
    lav->slav->error_return = true;
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
  all_vertices.push_back(v1);
  all_vertices.push_back(v2);
  new_verts.push_back(v1.get());
  new_verts.push_back(v2.get());

  v1->prev = event.vertex_a->prev;
  v1->next = x;
  event.vertex_a->prev->next = v1.get();
  x->prev = v1.get();

  v2->prev = y;
  v2->next = event.vertex_a->next;
  event.vertex_a->next->prev = v2.get();
  y->next = v2.get();

  std::vector<std::shared_ptr<LAV> > new_lavs;
  remove(lav);
  if(lav != x->lav) {
    // the split event actually merges two lavs
    remove(x->lav);

    new_lavs.push_back(std::make_shared<LAV>(v1.get(), this));
  } else {
    new_lavs.push_back(std::make_shared<LAV>(v1.get(), this));
    new_lavs.push_back(std::make_shared<LAV>(v2.get(), this));
  }
  for(size_t i = 0; i < new_lavs.size(); i++) {
    std::shared_ptr<LAV> lv = new_lavs[i];
    if(lv->len > 2) {
      Rcpp::Rcout << "Pushing new lav #" << i << " of length " << lv->len << "\n";
      lavs.push_back(lv);
      lav_valid.push_back(true);
    } else {
      Rcpp::Rcout << "lav collapsed to line " << lv->head->vertex << " to " << lv->head->next->vertex << "\n";

      sinks.push_back(lv->head->next->vertex);
      LAVertex* cur = lv->head;
      for(size_t j = 0; j < lv->len; j++) {
        invalidate_verts.push_back(cur);
        // cur->invalidate();
        cur = cur->next;
      }
    }
  }
  std::vector<Event> events;
  // if(vertices.empty()) {
  //   throw std::runtime_error("Err");
  // }
  for(size_t i = 0; i < vertices.size(); i++) {
    Event next_event = vertices[i]->next_event();
    if(!next_event.none) {
      events.push_back(next_event);
    } else {
      Rcpp::Rcout << "NO EVENTS FOR VERTEX #" << vertices[i]->vertex_id << "\n";
    }
  }
  invalidate_verts.push_back(event.vertex_a);
  // event.vertex_a->invalidate();

  return(std::pair<Subtree, std::vector<Event> >(Subtree(event.intersection, event.distance, sinks), events));
}
