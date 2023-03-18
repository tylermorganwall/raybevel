#include <Rcpp.h>
using namespace Rcpp;

#include "float.h"
#include "vec2.h"
#include "point2.h"
#include "ray.h"
#include "lav.h"
#include "originaledge.h"
#include "linesegment.h"
#include "lavertex.h"
#include "slav.h"
#include "subtree.h"
#include <queue>

// namespace RaySkeleton {

struct OriginalEdge;

std::vector<point2f> NormalizeContour(NumericMatrix vertices,
                                      Float tol) {
  ///ADD CHECK FOR ORIENTATION
  std::vector<point2f> verts;
  size_t nr = vertices.nrow();
  for(size_t i = 0; i < nr; i++) {
    if(i == 0) {
      point2f prev(vertices(nr-1,0),vertices(nr-1,1));
      point2f point(vertices(0,0),vertices(0,1));
      point2f next(vertices(1,0),vertices(1,1));
      vec2f normed_prev = unit_vector(point - prev);
      vec2f normed_next = unit_vector(next - point);
      if(!point.is_equivalent(next, tol) |
         !normed_next.is_equivalent(normed_prev, tol)) {
         verts.push_back(point);
      }
    } else if (i == nr - 1) {
      point2f prev(vertices(i-1,0),vertices(i-1,1));
      point2f point(vertices(i,0),vertices(i,1));
      point2f next(vertices(0,0),vertices(0,1));
      vec2f normed_prev = unit_vector(point - prev);
      vec2f normed_next = unit_vector(next - point);
      if(!point.is_equivalent(next, tol) |
         !normed_next.is_equivalent(normed_prev, tol)) {
         verts.push_back(point);
      }
    } else {
      point2f prev(vertices(i-1,0),vertices(i-1,1));
      point2f point(vertices(i,0),vertices(i,1));
      point2f next(vertices(0,0),vertices(0,1));
      vec2f normed_prev = unit_vector(point - prev);
      vec2f normed_next = unit_vector(next - point);
      if(!point.is_equivalent(next, tol) |
         !normed_next.is_equivalent(normed_prev, tol)) {
         verts.push_back(point);
      }
    }
  }
  return(verts);
}



// [[Rcpp::export]]
List skeletonize_rcpp(NumericMatrix vertices, List holes, double tol) {
  if(vertices(0,0) !=  vertices(vertices.nrow()-1,0) ||
     vertices(0,1) !=  vertices(vertices.nrow()-1,1)) {
    throw std::runtime_error("First vertex not equal to last");
  }
  std::vector<std::vector<point2f> > contours;
  contours.push_back(NormalizeContour(vertices, tol));
  for(size_t i = 0; i < holes.size(); i++) {
    contours.push_back(NormalizeContour(Rcpp::as<NumericMatrix>(holes(i)), tol));
  }
  SLAV slav(contours, tol);
  std::vector<Subtree> output;
  std::priority_queue<Event, std::vector<Event>, CompareEvents> EventQueue;
  Rcpp::Rcout << "Adding initial events:\n";
  for(size_t i = 0; i < slav.len(); i++) {
    LAV* lav = slav[i];
    LAVertex* cur = lav->head.get();
    for(size_t j = 0; j < lav->len; j++) {
      Event e = cur->next_event();
      if(!e.none) {
        EventQueue.push(cur->next_event());
      }
      cur = cur->next.get();
    }
  }
  std::vector<Subtree> arcs;
  // # While the priority queue or SLAV is not empty, compute the intersection of
  // # bisectors at edge. The 'handle_edge_event' method computes the next event
  // # from the new intersection via the next_event method. Thus, this while loop
  // # iteratively adds new events without recursion.
  Rcpp::Rcout << "Skeletonizing:\n";
  while(!(EventQueue.empty() || slav.empty())) {
    // log.debug('SLAV is %s', [repr(lav) for lav in slav])
    Event top = EventQueue.top(); // # vertex a, b is self or next vertex
    EventQueue.pop();
    Rcpp::Rcout << EventQueue.size() << " " << slav.len() << "\n";
    std::pair<Subtree, std::vector<Event>> event_output;
    // # Handle edge or split events.
    // # arc: subtree(event.intersection_point, event.distance, sinks)
    // # events: updated events with new vertex
    if(!top.split) {
      if(!top.vertex_a->is_valid || !top.vertex_b->is_valid) {
        Rcpp::Rcout << "Discarded outdated edge event " << top.distance << "\n";
        // log.info('%.2f Discarded outdated edge event %s', i.distance, i)
        continue;
      }
      event_output = slav.handle_edge_event(top);
    } else if (top.split) {
      if(!top.vertex_a->is_valid) {
        Rcpp::Rcout << "Discarded outdated split event " << top.distance << "\n";
        // log.info('%.2f Discarded outdated split event %s', i.distance, i)
        continue;
      }
      event_output = slav.handle_split_event(top);
    }
    std::vector<Event> new_events = event_output.second;
    for(size_t i = 0; i < new_events.size(); i++) {
      Rcpp::Rcout << "Adding event at distance " << new_events[i].distance << "\n";
      EventQueue.push(new_events[i]);
    }
    // As we traverse priorque, output list of "subtrees", which are in the form
    // of (source, height, sinks) where source is the highest points, height is
    // its distance to an edge, and sinks are the point connected to the source.
    if(!event_output.first.none) {
      arcs.push_back(event_output.first);
    }
  }

  Rcpp::List FinalList(arcs.size());
  for(size_t i = 0; i < arcs.size(); i++) {
    Rcpp::NumericMatrix sink_source(arcs[i].sinks.size(),4);
    for(size_t j = 0; j < arcs[i].sinks.size(); j++) {
      sink_source(j,0) = arcs[i].source.x();
      sink_source(j,1) = arcs[i].source.y();
      sink_source(j,2) = arcs[i].sinks[j].x();
      sink_source(j,3) = arcs[i].sinks[j].y();
    }
    FinalList(i) = sink_source;
  }
  return(FinalList);
}

//end namespace RaySkeleton
// }
