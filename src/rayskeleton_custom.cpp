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
#include <csignal>

// namespace RaySkeleton {

struct OriginalEdge;

std::vector<point2f> NormalizeContour(NumericMatrix vertices,
                                      Float tol) {
  std::vector<point2f> verts;
  size_t nr = vertices.nrow();
  for(size_t i = 0; i < nr; i++) {
    if(i == 0) {
      point2f prev(vertices(nr-2,0),vertices(nr-2,1));
      point2f point(vertices(0,0),vertices(0,1));
      point2f next(vertices(1,0),vertices(1,1));
      vec2f normed_prev = unit_vector(point - prev);
      vec2f normed_next = unit_vector(next - point);
      if(!point.is_equivalent(next, tol) &&
         !normed_next.is_equivalent(normed_prev, tol)) {
         verts.push_back(point);
      } else {
        Rcpp::Rcout << "Vertex " << i << " not normalized\n";
      }
    } else if (i == nr - 1) {
      point2f prev(vertices(nr - 2,0),vertices(nr - 2,1));
      point2f point(vertices(nr - 1,0),vertices(nr - 1,1));
      point2f next(vertices(1,0),vertices(1,1));
      vec2f normed_prev = unit_vector(point - prev);
      vec2f normed_next = unit_vector(next - point);
      if(!point.is_equivalent(next, tol) &&
         !normed_next.is_equivalent(normed_prev, tol)) {
         verts.push_back(point);
      } else {
        Rcpp::Rcout << "Vertex " << i << " not normalized\n";
      }
    } else {
      point2f prev(vertices(i-1,0),vertices(i-1,1));
      point2f point(vertices(i,0),vertices(i,1));
      point2f next(vertices(i+1,0),vertices(i+1,1));
      vec2f normed_prev = unit_vector(point - prev);
      vec2f normed_next = unit_vector(next - point);
      if(!point.is_equivalent(next, tol) &&
         !normed_next.is_equivalent(normed_prev, tol)) {
         verts.push_back(point);
      } else {
        Rcpp::Rcout << "Vertex " << i << " not normalized\n";
      }
    }
  }
  return(verts);
}

bool compareEqualEvents(Event e1, Event e2) {
  if(e1.split && !e2.split) {
    return(false);
  }
  if(!e1.split && e2.split) {
    return(true);
  }
  // return(e1.vertex_a->vertex_id > e2.vertex_a->vertex_id);
}

// bool compareEqualEvents(Event e1, Event e2) {
//   return(e1.split < e2.split);
//   // if(!e1.split && !e2.split) {
//   //   if(e1.vertex_a->vertex_id == e2.vertex_a->vertex_id) {
//   //     return (e1.vertex_b->vertex_id < e2.vertex_b->vertex_id);
//   //   }
//   // }
//   // return(e1.vertex_a->vertex_id < e2.vertex_a->vertex_id);
// }


// bool compareEqualEvents(Event e1, Event e2) {
//   if(!e1.split && e2.split) {
//     return(true);
//   }
//   if(e1.split && !e2.split) {
//     return(false);
//   }
//   return(true);
// }
//
//

// Given three collinear points p, q, r, the function checks if
// point q lies on line segment 'pr'
bool onSegment(point2f p, point2f q, point2f r) {
  if (q.x() <= std::fmax(p.x(), r.x()) && q.x() >= std::fmin(p.x(), r.x()) &&
      q.y() <= std::fmax(p.y(), r.y()) && q.y() >= std::fmin(p.y(), r.y())) {
    return true;
  }
  return false;
}

// To find orientation of ordered triplet (p, q, r).
// The function returns following values
// 0 --> p, q and r are collinear
// 1 --> Clockwise
// 2 --> Counterclockwise
int orientation(point2f p, point2f q, point2f r, Float tol) {
  vec2f qp = q - p;
  vec2f rq = r - q;
  Float val = DifferenceOfProducts(qp.y(), rq.x(),
                                   qp.x(), rq.y());
  if (abs(val) < tol) {
    return 0;  // collinear
  }
  return (val > 0) ? 1 : 2; // clock or counterclock wise
}

// The main function that returns true if line segment 'p1q1'
// and 'p2q2' intersect.
bool doIntersect(LineSegment e1, LineSegment e2, Float tol) {
  point2f p1 = e1.origin;
  point2f q1 = e1.adjacent;
  point2f p2 = e2.origin;
  point2f q2 = e2.adjacent;

  int o1 = orientation(p1, q1, p2, tol);
  int o2 = orientation(p1, q1, q2, tol);
  int o3 = orientation(p2, q2, p1, tol);
  int o4 = orientation(p2, q2, q1, tol);

  // General case
  if (o1 != o2 && o3 != o4)
    return true;

  // Special Cases
  // p1, q1 and p2 are collinear and p2 lies on segment p1q1
  if (o1 == 0 && onSegment(p1, p2, q1)) return true;

  // p1, q1 and q2 are collinear and q2 lies on segment p1q1
  if (o2 == 0 && onSegment(p1, q2, q1)) return true;

  // p2, q2 and p1 are collinear and p1 lies on segment p2q2
  if (o3 == 0 && onSegment(p2, p1, q2)) return true;

  // p2, q2 and q1 are collinear and q1 lies on segment p2q2
  if (o4 == 0 && onSegment(p2, q1, q2)) return true;

  return false; // Doesn't fall in any of the above cases
}

bool CheckSelfIntersections(std::vector<OriginalEdge>& OriginalEdges, Float tol) {
  for(size_t i = 0; i < OriginalEdges.size(); i++) {
    for(size_t j = i+2; j < OriginalEdges.size()-2; j++) {
      size_t ii = i % OriginalEdges.size();
      size_t jj = j % OriginalEdges.size();
      if(doIntersect(OriginalEdges[ii].Edge, OriginalEdges[jj].Edge, tol)) {
        return(true);
      }
    }
  }
  return(false);
}


// [[Rcpp::export]]
List skeletonize_custom_rcpp(NumericMatrix vertices, List holes, double dtolerance) {
  Float tolerance = dtolerance;
  if(vertices(0,0) !=  vertices(vertices.nrow()-1,0) ||
     vertices(0,1) !=  vertices(vertices.nrow()-1,1)) {
    throw std::runtime_error("First vertex not equal to last");
  }
  Float tol = tolerance;
  std::vector<std::vector<point2f> > contours; //This includes repeated endpoints
  contours.push_back(NormalizeContour(vertices, tol));
  for(size_t i = 0; i < holes.size(); i++) {
    contours.push_back(NormalizeContour(Rcpp::as<NumericMatrix>(holes(i)), tol));
  }
  SLAV slav(contours, tol);
  bool is_simple = !CheckSelfIntersections(slav.OriginalEdges, 0);
  if(!is_simple) {
    return(List());
  }
  std::vector<Subtree> output;
  std::priority_queue<Event, std::vector<Event>, CompareEvents> EventQueue;
  for(size_t i = 0; i < slav.len(); i++) {
    LAV* lav = slav[i];
    LAVertex* cur = lav->head;
    for(size_t j = 0; j < lav->len; j++) {
      // Rcpp::Rcout << "Initial event vertex #" << cur->vertex_id << "\n";
      Event e = cur->next_event();
      // if(e.split) {
      //   Rcpp::Rcout << e.distance << " Split event @ " <<  e.intersection << " from " << e.vertex_a->vertex_id << " to " << e.opposite_edge.id << "\n";
      // } else {
      //   Rcpp::Rcout << e.distance << " Edge event @ " <<  e.intersection << " between " << e.vertex_a->vertex_id << " & " << e.vertex_b->vertex_id<< "\n";
      // }

      if(!e.none) {
        EventQueue.push(e);
      }
      cur = cur->next;
    }
  }
  std::vector<std::tuple<point2f, int, int, int, int>> intermediate_lavs;
  std::vector<std::tuple<point2f, vec2f>> debug_bisectors;
  std::vector<std::tuple<point2f, bool>> debug_reflex;

  std::vector<Subtree> arcs;
  //Keep track of intermediate lavs
  int counter = 0 ;
  for(size_t i = 0; i < slav.len(); i++) {
    // if(slav.lav_valid[i]) {
      LAVertex* head = slav[i]->head;
      for(size_t j = 0; j < slav[i]->len; j++) {
        intermediate_lavs.push_back(std::make_tuple(head->vertex,
                                                    i,
                                                    counter,
                                                    head->is_valid,
                                                    slav[i]->len));

        //Debug
        debug_bisectors.push_back(std::make_tuple(head->bisector.o, unit_vector(head->bisector.d)/5));
        debug_reflex.push_back(std::make_tuple(head->vertex, head->is_reflex));
        head = head->next;

      }
    // }
  }
  counter++;
  // # While the priority queue or SLAV is not empty, compute the intersection of
  // # bisectors at edge. The 'handle_edge_event' method computes the next event
  // # from the new intersection via the next_event method. Thus, this while loop
  // # iteratively adds new events without recursion.
  // Rcpp::Rcout << "Skeletonizing:\n";
  int iter = 0;

  while(!(EventQueue.empty() || slav.empty()) && !slav.error_return) {
    // iter++;
    // Rcpp::Rcout << iter << "\n";
    // slav.PrintVertices();
    // std::priority_queue<Event, std::vector<Event>, CompareEvents> EventQueueCopy = EventQueue;
    // for(size_t i = 0; i < EventQueueCopy.size(); i++) {
    //   Event e = EventQueueCopy.top();
    //   EventQueueCopy.pop();
    //   // if(e.split) {
    //   //   if(e.vertex_a->is_valid) {
    //   //   Rprintf("%i %0.16f\n",e.vertex_a->vertex_id, e.distance);
    //   //   // Rcpp::Rcout << e.distance << " Split event @ " <<  e.intersection << " from " << e.vertex_a->vertex_id << " to " << e.opposite_edge.id << "\n";
    //   //   }
    //   // } else {
    //   //   if(e.vertex_a->is_valid && e.vertex_b->is_valid) {
    //   //   Rprintf("%i-%i %0.16f\n",e.vertex_a->vertex_id, e.vertex_b->vertex_id, e.distance);
    //   //   // Rcpp::Rcout << e.distance << " Edge event @ " <<  e.intersection << " between " << e.vertex_a->vertex_id << " & " << e.vertex_b->vertex_id<< "\n";
    //   //   }
    //   // }
    // }
    Event first_top = EventQueue.top(); // # vertex a, b is self or next vertex
    EventQueue.pop();
    //Copy event queue

    Event e = first_top;
    // if(e.split) {
    //   // if(e.vertex_a->is_valid) {
    //   Rprintf("%0.16f\n",e.distance);
    //     Rcpp::Rcout << e.distance << " Split event @ " <<  e.intersection << " from " << e.vertex_a->vertex_id << " to " << e.opposite_edge.id << "\n";
    //   // }
    // } else {
    //   // if(e.vertex_a->is_valid && e.vertex_b->is_valid) {
    //   Rprintf("%0.16f\n",e.distance);
    //   Rcpp::Rcout << e.distance << " Edge event @ " <<  e.intersection << " between " << e.vertex_a->vertex_id << " & " << e.vertex_b->vertex_id<< "\n";
    //   // }
    // }
    // if(e.split) {
    //   if(!e.vertex_a->is_valid) {
    //     continue;
    //   }
    // } else {
    //   if(!e.vertex_a->is_valid || !e.vertex_b->is_valid) {
    //     continue;
    //   }
    // }

    // Rcpp::Rcout << first_top.vertex_a->vertex_id << " " <<  first_top.distance << "\n";
    std::vector<Event> equalDistanceEvents;
    equalDistanceEvents.push_back(first_top);
    while(!EventQueue.empty() &&
          (std::fabs(EventQueue.top().distance - first_top.distance) < tol) &&
          first_top.intersection.is_equivalent(EventQueue.top().intersection, tol)) {
      // if(EventQueue.top().split) {
      //   Rcpp::Rcout << "Equal Split Event with vert#" << EventQueue.top().vertex_a->vertex_id << "\n";
      // } else {
      //   Rcpp::Rcout << "Equal Edge Event with vert#" << EventQueue.top().vertex_a->vertex_id <<
      //     " and vert#" << EventQueue.top().vertex_b->vertex_id << "\n";
      // }
      Event tmp_e = EventQueue.top();
      if(!tmp_e.none && !tmp_e.IsObsolete()) {
        equalDistanceEvents.push_back(EventQueue.top());
      }
      EventQueue.pop();
    }
    sort(equalDistanceEvents.begin(), equalDistanceEvents.end(), compareEqualEvents);
    point2f first_intersection = first_top.intersection;
    // if(equalDistanceEvents.size() > 1) {
    //   e = equalDistanceEvents[0];
    //   // if(e.split) {
    //   //   // if(e.vertex_a->is_valid) {
    //   //     Rcpp::Rcout << "Choosing Split event @ " << e.distance << " Split event @ " <<  e.intersection << " from " << e.vertex_a->vertex_id << " to " << e.opposite_edge.id << " --- Valid A: " << e.vertex_a->is_valid << "\n";
    //   //   // }
    //   // } else {
    //   //   // if(e.vertex_a->is_valid && e.vertex_b->is_valid ) {
    //   //   Rcpp::Rcout << "Choosing Edge event @ " <<  e.intersection << " between " << e.vertex_a->vertex_id << " & " << e.vertex_b->vertex_id<< " --- Valid A/B: " << e.vertex_a->is_valid << "/" << e.vertex_b->is_valid << "\n";
    //   //   // }
    //   // }
    // }
    if(equalDistanceEvents.size() == 1) {
      Event e;
      Event& top = equalDistanceEvents[0];
      std::pair<Subtree, std::vector<Event>> event_output;
      // Handle edge or split events.
      // arc: subtree(event.intersection_point, event.distance, sinks)
      // events: updated events with new vertex
      if(top.IsObsolete()) {
        // Rcpp::Rcout << "Discarding outdated " << (top.split ? "Split" : "Edge") << "event\n";
        slav.writeInfoLine(vformat("%.2f Discarded outdated edge event %s",
                                   top.distance, top.PrintInfo().c_str()));
        continue;
      }
      if(!top.split) {
        event_output = slav.handle_edge_event(top);
      } else if (top.split) {
        event_output = slav.handle_split_event(top);
      }
      std::vector<Event> new_events = event_output.second;
      for(size_t i = 0; i < new_events.size(); i++) {
        EventQueue.push(new_events[i]);
      }
      // As we traverse the event queue, output list of "subtrees", which are in the form
      // of (source, height, sinks) where source is the highest points, height is
      // its distance to an edge, and sinks are the point connected to the source.
      if(!event_output.first.none) {
        arcs.push_back(event_output.first);
      }
    } else {
      std::vector<std::pair<Subtree, std::vector<Event>>> MultiSubtreeEvents = slav.handle_multi_event(equalDistanceEvents);
      for(size_t i = 0; i < MultiSubtreeEvents.size(); i++) {
      // for(size_t i = 0; i < equalDistanceEvents.size(); i++) {
      // std::pair<Subtree, std::vector<Event>> event_output;
      // if(equalDistanceEvents[i].IsObsolete()) {
      //   slav.writeInfoLine(vformat("%.2f Discarded outdated edge event %s",
      //                              equalDistanceEvents[i].distance,
      //                              equalDistanceEvents[i].PrintInfo().c_str()));
      //   continue;
      // }
      // if(equalDistanceEvents[i].split) {
      //   event_output = slav.handle_split_event(equalDistanceEvents[i]);
      // } else {
      //   event_output = slav.handle_edge_event(equalDistanceEvents[i]);
      // }

        std::pair<Subtree, std::vector<Event>>& event_output = MultiSubtreeEvents[i];
        std::vector<Event>& new_events = event_output.second;
        for(size_t j = 0; j < new_events.size(); j++) {
          EventQueue.push(new_events[j]);
        }
        if(!event_output.first.none) {
          arcs.push_back(event_output.first);
        }
      }
      // As we traverse the event queue, output list of "subtrees", which are in the form
      // of (source, height, sinks) where source is the highest points, height is
      // its distance to an edge, and sinks are the point connected to the source.
      // if(!event_output.first.none) {
      //   arcs.push_back(event_output.first);
      // }
    }
    // for(size_t k = 0; k < equalDistanceEvents.size(); k++) {
    //   e = equalDistanceEvents[k];
    //   // if(e.split) {
    //   //   // if(e.vertex_a->is_valid) {
    //   //     Rcpp::Rcout << e.distance << " Split event @ " <<  e.intersection << " from " << e.vertex_a->vertex_id << " to " << e.opposite_edge.id << " --- Valid A: " << e.vertex_a->is_valid << "\n";
    //   //   // }
    //   // } else {
    //   //   // if(e.vertex_a->is_valid && e.vertex_b->is_valid ) {
    //   //   Rcpp::Rcout << e.distance << " Edge event @ " <<  e.intersection << " between " << e.vertex_a->vertex_id << " & " << e.vertex_b->vertex_id<< " --- Valid A/B: " << e.vertex_a->is_valid << "/" << e.vertex_b->is_valid << "\n";
    //   //   // }
    //   // }
    //   Event& top = equalDistanceEvents[k];
    //   std::pair<Subtree, std::vector<Event>> event_output;
    //   // Handle edge or split events.
    //   // arc: subtree(event.intersection_point, event.distance, sinks)
    //   // events: updated events with new vertex
    //   if(top.IsObsolete()) {
    //     // Rcpp::Rcout << "Discarding outdated " << (top.split ? "Split" : "Edge") << "event\n";
    //     continue;
    //   }
    //   if(!top.split) {
    //     event_output = slav.handle_edge_event(top);
    //
    //   } else if (top.split) {
    //     event_output = slav.handle_split_event(top);
    //   }
    //   std::vector<Event> new_events = event_output.second;
    //   for(size_t i = 0; i < new_events.size(); i++) {
    //     e = new_events[i];
    //     // if(e.split) {
    //     //   Rcpp::Rcout << "New event: " << e.distance << " Split event @ " <<  e.intersection << " from " << e.vertex_a->vertex_id << " to " << e.opposite_edge.id << "\n";
    //     // } else {
    //     //   Rcpp::Rcout << "New event: " << e.distance << " Edge event @ " <<  e.intersection << " between " << e.vertex_a->vertex_id << " & " << e.vertex_b->vertex_id<< "\n";
    //     // }
    //     EventQueue.push(new_events[i]);
    //   }
    //   // As we traverse the event queue, output list of "subtrees", which are in the form
    //   // of (source, height, sinks) where source is the highest points, height is
    //   // its distance to an edge, and sinks are the point connected to the source.
    //   if(!event_output.first.none) {
    //     arcs.push_back(event_output.first);
    //   }
    // }
    for(size_t i = 0; i < slav.len(); i++) {
      LAVertex* head = slav[i]->head;
      for(size_t j = 0; j < slav[i]->len; j++) {
        intermediate_lavs.push_back(std::make_tuple(head->vertex,
                                                    i,
                                                    counter,
                                                    head->is_valid,
                                                    slav[i]->len));
        head = head->next;
      }
    }
    counter++;
  }

  Rcpp::List FinalList(arcs.size());
  for(size_t i = 0; i < arcs.size(); i++) {
    Rcpp::NumericMatrix sink_source(arcs[i].sinks.size(),7);
    for(size_t j = 0; j < arcs[i].sinks.size(); j++) {
      sink_source(j,0) = arcs[i].source.x();
      sink_source(j,1) = arcs[i].source.y();
      sink_source(j,2) = arcs[i].sinks[j].x();
      sink_source(j,3) = arcs[i].sinks[j].y();
      sink_source(j,4) = arcs[i].distance;
      sink_source(j,5) = arcs[i].from_split;
      sink_source(j,6) = arcs[i].sink_ids[j];
    }
    FinalList(i) = sink_source;
  }
  Rcpp::NumericMatrix dp(slav.discarded_points.size(),7);
  for(size_t i = 0; i < slav.discarded_points.size(); i++) {
    point2f pt = std::get<0>(slav.discarded_points[i]);
    point2f vtx = std::get<1>(slav.discarded_points[i]);
    int id = std::get<2>(slav.discarded_points[i]);
    int reflex = std::get<3>(slav.discarded_points[i]);
    int split = std::get<4>(slav.discarded_points[i]);

    dp(i,0) = pt.x();
    dp(i,1) = pt.y();
    dp(i,2) = vtx.x();
    dp(i,3) = vtx.y();
    dp(i,4) = id;
    dp(i,5) = reflex;
    dp(i,6) = split;
  }
  Rcpp::NumericMatrix ilav(intermediate_lavs.size(),6);
  for(size_t i = 0; i < intermediate_lavs.size(); i++) {
    point2f pt = std::get<0>(intermediate_lavs[i]);
    int lav_id = std::get<1>(intermediate_lavs[i]);
    int counter = std::get<2>(intermediate_lavs[i]);
    int valid = std::get<3>(intermediate_lavs[i]);
    int nlavs = std::get<4>(intermediate_lavs[i]);

    ilav(i,0) = pt.x();
    ilav(i,1) = pt.y();
    ilav(i,2) = lav_id;
    ilav(i,3) = counter;
    ilav(i,4) = valid;
    ilav(i,5) = nlavs;


  }
  RayRayInterectionInfo& intersection_data = slav.event_rays;
  Rcpp::NumericMatrix int_debug_data(intersection_data.size(),13);

  for(size_t i = 0; i < intersection_data.size(); i++) {
    point2f o1 = std::get<0>(intersection_data[i]);
    vec2f d1 = std::get<1>(intersection_data[i]);
    point2f o2 = std::get<2>(intersection_data[i]);
    vec2f d2 = std::get<3>(intersection_data[i]);
    point2f intersect = std::get<4>(intersection_data[i]);
    bool success = std::get<5>(intersection_data[i]);
    int id = std::get<6>(intersection_data[i]);
    int edge = std::get<7>(intersection_data[i]);

    int_debug_data(i,0) = o1.x();
    int_debug_data(i,1) = o1.y();
    int_debug_data(i,2) = d1.x();
    int_debug_data(i,3) = d1.y();
    int_debug_data(i,4) = o2.x();
    int_debug_data(i,5) = o2.y();
    int_debug_data(i,6) = d2.x();
    int_debug_data(i,7) = d2.y();
    int_debug_data(i,8) = intersect.x();
    int_debug_data(i,9) = intersect.y();
    int_debug_data(i,10) = success;
    int_debug_data(i,11) = id;
    int_debug_data(i,12) = edge;
  }

  SplitInfo& splitinfo = slav.split_points;
  Rcpp::NumericMatrix split_debug_data(splitinfo.size(),29);

  for(size_t i = 0; i < splitinfo.size(); i++) {
    point2f point1 = std::get<0>(splitinfo[i]);
    vec2f d1 = std::get<1>(splitinfo[i]);
    point2f point3 = std::get<2>(splitinfo[i]);
    point2f b = std::get<3>(splitinfo[i]);
    int left = std::get<4>(splitinfo[i]);
    int right = std::get<5>(splitinfo[i]);
    int edge = std::get<6>(splitinfo[i]);
    int vid = std::get<7>(splitinfo[i]);
    int e_id = std::get<8>(splitinfo[i]);
    point2f vert = std::get<9>(splitinfo[i]);
    vec2f d2 = std::get<10>(splitinfo[i]);
    bool bintersect = std::get<11>(splitinfo[i]);
    point2f interpoint = std::get<12>(splitinfo[i]);
    point2f edge_edge_intersect = std::get<13>(splitinfo[i]);
    vec2f edge_edge_bisect = std::get<14>(splitinfo[i]);
    point2f edge_start = std::get<15>(splitinfo[i]);
    vec2f edge_d = std::get<16>(splitinfo[i]);
    vec2f chosen_edge_d = std::get<17>(splitinfo[i]);

    split_debug_data(i,0) = point1.x();
    split_debug_data(i,1) = point1.y();
    split_debug_data(i,2) = d1.x();
    split_debug_data(i,3) = d1.y();
    split_debug_data(i,4) = point3.x();
    split_debug_data(i,5) = point3.y();
    split_debug_data(i,6) = b.x();
    split_debug_data(i,7) = b.y();
    split_debug_data(i,8) = left;
    split_debug_data(i,9) = right;
    split_debug_data(i,10) = edge;
    split_debug_data(i,11) = vid;
    split_debug_data(i,12) = e_id;
    split_debug_data(i,13) = vert.x();
    split_debug_data(i,14) = vert.y();
    split_debug_data(i,15) = d2.x();
    split_debug_data(i,16) = d2.y();
    if(bintersect) {
      split_debug_data(i,17) = interpoint.x();
      split_debug_data(i,18) = interpoint.y();
    } else {
      split_debug_data(i,17) = std::numeric_limits<Float>::infinity();
      split_debug_data(i,18) = std::numeric_limits<Float>::infinity();
    }
    split_debug_data(i,19) = edge_edge_intersect.x();
    split_debug_data(i,20) = edge_edge_intersect.y();
    split_debug_data(i,21) = edge_edge_bisect.x();
    split_debug_data(i,22) = edge_edge_bisect.y();
    split_debug_data(i,23) = edge_start.x();
    split_debug_data(i,24) = edge_start.y();
    split_debug_data(i,25) = edge_d.x();
    split_debug_data(i,26) = edge_d.y();
    split_debug_data(i,27) = chosen_edge_d.x();
    split_debug_data(i,28) = chosen_edge_d.y();
  }

  Rcpp::NumericMatrix oe(slav.OriginalEdges.size(),4);
  for(size_t i = 0; i < slav.OriginalEdges.size(); i++) {
    LineSegment edge = slav.OriginalEdges[i].Edge;
    oe(i,0) = edge.origin.x();
    oe(i,1) = edge.origin.y();
    oe(i,2) = edge.adjacent.x();
    oe(i,3) = edge.adjacent.y();
  }

  Rcpp::NumericMatrix bisector_debug(debug_bisectors.size(),4);
  for(size_t i = 0; i < debug_bisectors.size(); i++) {
    point2f bo = std::get<0>(debug_bisectors[i]);
    vec2f bd = std::get<1>(debug_bisectors[i]);
    bisector_debug(i,0) = bo.x();
    bisector_debug(i,1) = bo.y();
    bisector_debug(i,2) = bd.x();
    bisector_debug(i,3) = bd.y();
  }
  Rcpp::NumericMatrix debug_ref(debug_reflex.size(),3);
  for(size_t i = 0; i < debug_reflex.size(); i++) {
    point2f bo = std::get<0>(debug_reflex[i]);
    bool ref = std::get<1>(debug_reflex[i]);
    debug_ref(i,0) = bo.x();
    debug_ref(i,1) = bo.y();
    debug_ref(i,2) = ref;
  }

  List all_data = List::create(Named("ss") = FinalList, _["dp"] = dp,
                               _["lavs"] = ilav, _["int_debug_data"] = int_debug_data,
                               _["split_debug_data"] = split_debug_data,
                               _["original_edges"] = oe, _["bisector_debug"] = bisector_debug,
                               _["debug_reflex"] = debug_ref, _["simple"] = is_simple);
  return(all_data);
}

//end namespace RaySkeleton
// }
