#include "slav.h"
#include "lavertex.h"
#include "subtree.h"
#include "event.h"

void SLAV::remove(LAV* lav) {
  for(size_t i = 0; i < lavs.size(); i++) {
    if(lavs[i].get() == lav) {
      lavs.erase(lavs.begin() + i);
      break;
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
  std::vector<int> sink_ids;
  std::vector<Event> events;

  LAV* lav = event.vertex_a->lav;
  // # Triangle, one sink point
  // Rcpp::Rcout << "VA: " << event.vertex_a->prev->vertex << " VB: " << event.vertex_b->next->vertex << "\n";
  if(event.vertex_a->prev->vertex.is_equivalent(event.vertex_b->next->vertex, tol)) {
  // if(event.vertex_a->prev->vertex_id == event.vertex_b->next->vertex_id) {

    // Rcpp::Rcout << event.distance << " Peak event at intersection " <<  event.intersection << " from <" <<
      // event.vertex_a->vertex_id << "," << event.vertex_b->vertex_id << "," << event.vertex_a->prev->vertex_id << ">\n";
    LAVertex* current_vert = lav->head;
    // Rcpp::Rcout << "Vertex id: " << current_vert->vertex_id  << " " << current_vert->vertex << "\n";
    // Rcpp::Rcout << "PEAK EVENT" << "\n";

    for(size_t i = 0; i < lav->len; i++) {
      sinks.push_back(current_vert->vertex);
      sink_ids.push_back(current_vert->vertex_id);
      current_vert->invalidate();
      current_vert = current_vert->next;
    }
    remove(lav);
  } else {
    // // Rcpp::Rcout << event.distance << " Edge event at intersection " <<  event.intersection << " from <" <<
    //   event.vertex_a->vertex_id << "," << event.vertex_b->vertex_id << ">\n";
    //     log.info('%.2f Edge event at intersection %s from <%s,%s> in %s',
    //              event.distance, event.intersection_point, event.vertex_a,
    //              event.vertex_b, lav)
    LAVertex* new_vertex = lav->unify(event.vertex_a,
                                      event.vertex_b,
                                      event.intersection);
    if(lav->head == event.vertex_a || lav->head == event.vertex_b) {
      // Rcpp::Rcout << "Setting new head for vertex #" << new_vertex->vertex_id << "\n";
      lav->head = new_vertex;
    }
    sinks.push_back(event.vertex_a->vertex);
    sinks.push_back(event.vertex_b->vertex);
    sink_ids.push_back(event.vertex_a->vertex_id);
    sink_ids.push_back(event.vertex_b->vertex_id);
    Event next_event = new_vertex->next_event();
    if(!next_event.none) {
      events.push_back(next_event);
    } else {
      // Rcpp::Rcout << "NO EVENTS FOR VERTEX #" << new_vertex->vertex_id << "\n";
    }
  }
  return(std::pair<Subtree, std::vector<Event> >(Subtree(event.intersection, event.distance,
                                                         sinks, sink_ids,
                                                         false), events));
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
  // Rcpp::Rcout << event.distance << " Split event at intersection " <<  event.intersection << " for vertex " <<
    // event.vertex_a->vertex_id << " at " << event.opposite_edge.id << "\n";
  // log.info(
  //   '%.2f Split event at intersection %s from vertex %s, for edge %s in %s',
  //   event.distance,
  //   event.intersection_point,
  //   event.vertex,
  //   event.opposite_edge,
  //   lav)
  std::vector<point2f> sinks;
  std::vector<int> sink_ids;

  std::vector<LAVertex* > vertices;

  sinks.push_back(event.vertex_a->vertex);
  sink_ids.push_back(event.vertex_a->vertex_id);

  LAVertex* x = nullptr; // next vertex
  LAVertex* y = nullptr; // prev vertex
  LineSegment &Edge = event.opposite_edge;
  // for v in chain.from_iterable(self._lavs) {
  for(size_t i = 0; i < lavs.size(); i++) {
    // if(lav_valid[i]) {
      LAVertex* v = lavs[i]->head;
      for(size_t j = 0; j < lavs[i]->len; j++) {
        bool equal_to_edge_left_p = Edge.is_equivalent(v->edge_left, tol);
        bool equal_to_edge_right_p = Edge.is_equivalent(v->edge_right, tol);

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
    // }
  }
  if(!x) {
    // Rcpp::Rcout << "Failed split event at " << event.intersection << "\n";
    std::vector<Event> empty_events;
    lav->slav->error_return = false;
    return(std::pair<Subtree, std::vector<Event>>(Subtree(), empty_events));
  }
  if(event.vertex_a->edge_left.is_equivalent(event.opposite_edge, tol) ||
     event.vertex_a->edge_right.is_equivalent(event.opposite_edge, tol)) {
    // Rcpp::Rcout << "Edges identical \n";
    std::vector<Event> empty_events;
    lav->slav->error_return = false;
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
  all_vertices.push_back(v1); //Just for ownership purposes
  all_vertices.push_back(v2); //Just for ownership purposes
  // vertices.push_back(v1);
  // vertices.push_back(v2);

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
      // Rcpp::Rcout << "Pushing new lav #" << i << " of length " << lv->len << "\n";
      lavs.push_back(lv);
      vertices.push_back(lv->head);
    } else {
      // Rcpp::Rcout << "lav collapsed to line " << lv->head->vertex << " to " << lv->head->next->vertex << "\n";

      sinks.push_back(lv->head->next->vertex);
      sink_ids.push_back(lv->head->next->vertex_id);
      LAVertex* cur = lv->head;
      for(size_t j = 0; j < lv->len; j++) {
        cur->invalidate();
        cur = cur->next;
      }
    }
  }
  std::vector<Event> events;
  for(size_t i = 0; i < vertices.size(); i++) {
    Event next_event = vertices[i]->next_event();
    if(!next_event.none) {
      events.push_back(next_event);
    } else {
      // Rcpp::Rcout << "NO EVENTS FOR VERTEX #" << vertices[i]->vertex_id << "\n";
    }
  }
  event.vertex_a->invalidate();

  return(std::pair<Subtree, std::vector<Event> >(Subtree(event.intersection, event.distance, sinks, sink_ids, true), events));
}

std::vector<std::pair<Subtree, std::vector<Event>>> SLAV::handle_multi_event(std::vector<Event> events) {
  Rcpp::Rcout << "Handling Multi Event with " << events.size() << " shared events\n";
  std::vector<std::pair<Subtree, std::vector<Event>>> MultiSubtreeEvents;

  std::vector<Event> edgeEvents;
  std::vector<Event> splitEvents;
  std::vector<point2f> vertexEventParents;
  std::vector<point2f> sinks;
  std::vector<int> sink_ids;

  for(size_t i = 0; i < events.size(); i++) {
    if(!events[i].IsObsolete()) {
      if(!events[i].split) {
        if(events[i].vertex_a->prev->vertex.is_equivalent(events[i].vertex_b->next->vertex, tol)) {
          LAV* lav = events[i].vertex_a->lav;

          // Rcpp::Rcout << event.distance << " Peak event at intersection " <<  event.intersection << " from <" <<
          // event.vertex_a->vertex_id << "," << event.vertex_b->vertex_id << "," << event.vertex_a->prev->vertex_id << ">\n";
          LAVertex* current_vert = lav->head;
          // Rcpp::Rcout << "Vertex id: " << current_vert->vertex_id  << " " << current_vert->vertex << "\n";
          // Rcpp::Rcout << "PEAK EVENT" << "\n";

          for(size_t i = 0; i < lav->len; i++) {
            sinks.push_back(current_vert->vertex);
            sink_ids.push_back(current_vert->vertex_id);
            current_vert->invalidate();
            current_vert = current_vert->next;
          }
          remove(lav);
        } else {
          edgeEvents.push_back(events[i]);
        }
      }
      if(events[i].split) {
        splitEvents.push_back(events[i]);
        vertexEventParents.push_back(events[i].vertex_a->vertex);
      }
    }
  }
  if(!sinks.empty()) {
    MultiSubtreeEvents.push_back(std::pair<Subtree, std::vector<Event> >(Subtree(events[0].intersection, events[0].distance, sinks, sink_ids, false),
                                                                         std::vector<Event>()));
  }
  //Loop through split
  while(!splitEvents.empty()) {
    MultiSubtreeEvents.push_back(handle_split_event((*splitEvents.begin())));
    splitEvents.erase(splitEvents.begin());
  }

  //Loop through edge events and determine how many have shared edges, add lines/sinks for all of them
  while(!edgeEvents.empty()) {
    std::vector<Event> new_events;
    if(edgeEvents.begin()->IsObsolete()) {
      edgeEvents.erase(edgeEvents.begin());
      continue;
    }
    Rcpp::Rcout << "Size: " << edgeEvents.size() << "\n";
    std::vector<point2f> new_sinks;
    std::vector<int> new_sink_ids;
    std::set<int> remove_events;
    bool growing = true;
    LAVertex* begin_vertex = nullptr;
    LAVertex* end_vertex = nullptr;
    point2f intersection;
    intersection = edgeEvents.begin()->intersection;
    begin_vertex = edgeEvents.begin()->vertex_a;
    end_vertex   = edgeEvents.begin()->vertex_b;
    new_sinks.push_back(begin_vertex->vertex);
    new_sinks.push_back(end_vertex->vertex);
    new_sink_ids.push_back(begin_vertex->vertex_id);
    new_sink_ids.push_back(end_vertex->vertex_id);

    //Manually invalidate to change head later
    begin_vertex->is_valid = false ;
    end_vertex->is_valid = false ;

    edgeEvents.erase(edgeEvents.begin());

    //Find all adjacent events
    int cntr = 0;
    while(growing) {
      Rcpp::Rcout << growing << "\n";
      growing = false;
      for(size_t i = 0; i < edgeEvents.size(); i++) {
        if(edgeEvents[i].vertex_a == end_vertex &&
           edgeEvents[i].vertex_a->is_valid) {
          remove_events.insert(i);
          end_vertex = edgeEvents[i].vertex_b;
          end_vertex->is_valid = false ;

          new_sinks.push_back(end_vertex->vertex);
          new_sink_ids.push_back(end_vertex->vertex_id);
          growing = true;
        }
        if(edgeEvents[i].vertex_b == begin_vertex &&
           edgeEvents[i].vertex_b->is_valid) {
          remove_events.insert(i);
          begin_vertex = edgeEvents[i].vertex_a;
          begin_vertex->is_valid = false;
          new_sinks.push_back(begin_vertex->vertex);
          new_sink_ids.push_back(begin_vertex->vertex_id);
          growing = true;
        }
      }
    }
    //Remove edge events
    while(!remove_events.empty()) {
      int element = (*remove_events.end());
      Rcpp::Rcout << element << "\n";
      edgeEvents.erase(edgeEvents.begin() + element);
      remove_events.erase(element);
    }
    if(begin_vertex->lav == end_vertex->lav) {
      LAVertex* new_vertex = begin_vertex->lav->unify_multi(begin_vertex,
                                                            end_vertex,
                                                            intersection);

      Event next_event = new_vertex->next_event();
      if(!next_event.none) {
        new_events.push_back(next_event);
      }
    } else {
      throw std::runtime_error("Error: Unifying edge event from two different LAVs.");
    }
    MultiSubtreeEvents.push_back(std::pair<Subtree, std::vector<Event> >(
        Subtree(intersection, events[0].distance, new_sinks, new_sink_ids, true),
        new_events));
  }
  //Process two-node lavs
  int nlavs = lavs.size();
  for(int i = nlavs-1; i >= 0; i-- ) {
    std::vector<point2f> final_sinks;
    std::vector<int> final_sink_ids;
    if(lavs[i]->len == 2) {
      final_sinks.push_back(lavs[i]->head->next->vertex);
      final_sink_ids.push_back(lavs[i]->head->next->vertex_id);
      LAVertex* cur = lavs[i]->head;
      cur->invalidate();
      cur->next->invalidate();
      MultiSubtreeEvents.push_back(std::pair<Subtree, std::vector<Event> >(
          Subtree(lavs[i]->head->vertex, events[0].distance, final_sinks, final_sink_ids, true),
          std::vector<Event>()));
      lavs.erase(lavs.begin() + i);
    }
  }
  return(MultiSubtreeEvents);
}
