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
      } else {
        Rcpp::Rcout << "Vertex " << i << " not normalized\n";
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
      } else {
        Rcpp::Rcout << "Vertex " << i << " not normalized\n";
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
      } else {
        Rcpp::Rcout << "Vertex " << i << " not normalized\n";
      }
    }
  }
  return(verts);
}

bool compareEqualEvents(Event e1, Event e2) {
  if(!e1.split && !e2.split) {
    if(e1.vertex_a->vertex_id == e2.vertex_a->vertex_id) {
      return (e1.vertex_b->vertex_id < e2.vertex_b->vertex_id);
    }
  }
  return(e1.vertex_a->vertex_id < e2.vertex_a->vertex_id);
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
  // Rcpp::Rcout << "Adding initial events:\n";
  for(size_t i = 0; i < slav.len(); i++) {
    LAV* lav = slav[i];
    LAVertex* cur = lav->head;
    for(size_t j = 0; j < lav->len; j++) {
      Event e = cur->next_event();
      if(!e.none) {
        EventQueue.push(e);
      }
      cur = cur->next;
    }
  }
  std::vector<std::tuple<point2f, int, int, int>> intermediate_lavs;
  std::vector<Subtree> arcs;

  //Keep track of intermediate lavs
  int counter = 0 ;
  for(size_t i = 0; i < slav.len(); i++) {
    if(slav.lav_valid[i]) {
      LAVertex* head = slav[i]->head;
      for(size_t j = 0; j < slav[i]->len; j++) {
        intermediate_lavs.push_back(std::make_tuple(head->vertex,
                                                    i,
                                                    counter,
                                                    head->is_valid));
        head = head->next;
      }
    }
  }
  counter++;
  // # While the priority queue or SLAV is not empty, compute the intersection of
  // # bisectors at edge. The 'handle_edge_event' method computes the next event
  // # from the new intersection via the next_event method. Thus, this while loop
  // # iteratively adds new events without recursion.
  // Rcpp::Rcout << "Skeletonizing:\n";
  int iter = 0;
  while(!(EventQueue.empty() || slav.empty()) && !slav.error_return) {
    iter++;
    Event first_top = EventQueue.top(); // # vertex a, b is self or next vertex
    EventQueue.pop();

    std::vector<Event> equalDistanceEvents;
    equalDistanceEvents.push_back(first_top);
    // while((std::fabs(EventQueue.top().distance - first_top.distance) < tol) &&
    //       !EventQueue.empty() &&
    //       first_top.intersection.is_equivalent(EventQueue.top().intersection, tol)) {
    //   if(EventQueue.top().split) {
    //     Rcpp::Rcout << "Equal Split Event with vert#" << EventQueue.top().vertex_a->vertex_id << "\n";
    //   } else {
    //     Rcpp::Rcout << "Equal Edge Event with vert#" << EventQueue.top().vertex_a->vertex_id <<
    //       " and vert#" << EventQueue.top().vertex_b->vertex_id << "\n";
    //   }
    //   equalDistanceEvents.push_back(EventQueue.top());
    //   EventQueue.pop();
    // }
    // sort(equalDistanceEvents.begin(), equalDistanceEvents.end(), compareEqualEvents);
    // for(size_t i = 0; i < equalDistanceEvents.size(); i++) {
    //   Rcpp::Rcout << equalDistanceEvents[i].vertex_a->vertex_id << "\n";
    // }
    point2f first_intersection = first_top.intersection;
    for(size_t k = 0; k < equalDistanceEvents.size(); k++) {
      Event& top = equalDistanceEvents[k];
      std::pair<Subtree, std::vector<Event>> event_output;
      // # Handle edge or split events.
      // # arc: subtree(event.intersection_point, event.distance, sinks)
      // # events: updated events with new vertex

      if(!top.split) {
        if(!top.vertex_a->is_valid || !top.vertex_b->is_valid) {
          Rcpp::Rcout << "Discarded outdated edge event " << top.distance << "\n";
          continue;
        }
        event_output = slav.handle_edge_event(top);

      } else if (top.split) {
        if(!top.vertex_a->is_valid) {
          Rcpp::Rcout << "Discarded outdated split event " << top.distance << "\n";
          continue;
        }
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
      for(size_t i = 0; i < slav.invalidate_verts.size(); i++) {
        slav.invalidate_verts[i]->invalidate();
      }
    }
    // if(equalDistanceEvents.size() > 2) {
    //   for(size_t i = 0; i < slav.new_verts.size(); i++) {
    //     for(size_t j = i+1; j < slav.new_verts.size(); j++) {
    //       Rcpp::Rcout << "Trying to merge #" << slav.new_verts[i]->vertex_id << " and #" << slav.new_verts[j]->vertex_id;
    //       if(slav.new_verts[j]->is_valid && slav.new_verts[i]->is_valid &&
    //          slav.new_verts[i]->vertex.is_equivalent(slav.new_verts[j]->vertex, tol) &&
    //          slav.new_verts[j]->lav == slav.new_verts[i]->lav) {
    //         LAVertex* merged_v = slav.new_verts[j]->lav->unify(slav.new_verts[i],
    //                                                            slav.new_verts[j],
    //                                                            slav.new_verts[j]->vertex,
    //                                                            true);
    //         slav.new_verts[i]->invalidate();
    //         slav.new_verts[j]->invalidate();
    //         slav.new_verts.push_back(merged_v);
    //         Rcpp::Rcout << " : Success\n";
    //       } else {
    //         Rcpp::Rcout << " : Failure because " <<
    //           slav.new_verts[j]->is_valid << " " <<
    //           slav.new_verts[i]->is_valid << " " <<
    //             slav.new_verts[i]->vertex.is_equivalent(slav.new_verts[j]->vertex, tol) <<
    //               (slav.new_verts[j]->lav == slav.new_verts[i]->lav) <<  "\n";
    //       }
    //     }
    //   }
    // }
    // slav.new_verts.clear();

    slav.invalidate_verts.clear();
    for(size_t i = 0; i < slav.len(); i++) {
      if(slav.lav_valid[i]) {
        LAVertex* head = slav[i]->head;
        for(size_t j = 0; j < slav[i]->len; j++) {
          intermediate_lavs.push_back(std::make_tuple(head->vertex,
                                                      i,
                                                      counter,
                                                      head->is_valid));
          head = head->next;
        }
      }
    }
    counter++;
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
    dp(i,6) = reflex;
  }
  Rcpp::NumericMatrix ilav(intermediate_lavs.size(),5);
  for(size_t i = 0; i < intermediate_lavs.size(); i++) {
    point2f pt = std::get<0>(intermediate_lavs[i]);
    int lav_id = std::get<1>(intermediate_lavs[i]);
    int counter = std::get<2>(intermediate_lavs[i]);
    int valid = std::get<3>(intermediate_lavs[i]);

    ilav(i,0) = pt.x();
    ilav(i,1) = pt.y();
    ilav(i,2) = lav_id;
    ilav(i,3) = counter;
    ilav(i,4) = valid;

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
  Rcpp::NumericMatrix split_debug_data(splitinfo.size(),19);

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
  }

  List all_data = List::create(Named("ss") = FinalList, _["dp"] = dp,
                               _["lavs"] = ilav, _["int_debug_data"] = int_debug_data,
                               _["split_debug_data"] = split_debug_data);
  return(all_data);
}

//end namespace RaySkeleton
// }
