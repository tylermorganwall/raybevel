#include "lavertex.h"
#include "lav.h"
#include "slav.h"
#include "point2.h"
#include "event.h"

#include "Rcpp.h"


void intersect_ray_ray(Ray ray1, Ray ray2, point2f& intersection, bool& found, Float tol,
                       RayRayInterectionInfo& event_rays, int vertex_id, int edge_id) {
  Float d = DifferenceOfProducts(ray2.d.x(), ray1.d.y(),ray2.d.y(), ray1.d.x());

  if(abs(d) == 0) {
    found = false;
  } else {
    Float dy = ray2.o.y() - ray1.o.y();
    Float dx = ray2.o.x() - ray1.o.x();
    Float ua = DifferenceOfProducts(ray2.d.x(), dy, ray2.d.y(), dx) / d;
    Float va = DifferenceOfProducts(ray1.d.x(), dy, ray1.d.y(), dx) / d;

    if(ua < 0.0 || va < 0.0) {
      found = false;
    } else {
      found = true;
      intersection = ray1(ua);
      // event_rays.push_back(std::make_tuple(ray1.o,ray1.d, ray2.o, ray2.d, intersection, true, vertex_id, edge_id));
    }
  }
}

void intersect_ray_line(Ray ray1, LineSegment ray2, point2f& intersection, bool& found, Float tol,
                       RayRayInterectionInfo& event_rays, int vertex_id, int edge_id) {
  Float d = DifferenceOfProducts(ray2.d.x(), ray1.d.y(),ray2.d.y(), ray1.d.x());

  if(abs(d) == 0) {
    found = false;
  } else {
    Float dy = ray2.origin.y() - ray1.o.y();
    Float dx = ray2.origin.x() - ray1.o.x();
    Float ua = DifferenceOfProducts(ray2.d.x(), dy, ray2.d.y(), dx) / d;
    // Float va = DifferenceOfProducts(ray1.d.x(), dy, ray1.d.y(), dx) / d;

    if(ua < 0.0) {
      found = false;
      // intersection = ray1(ua);
      // event_rays.push_back(std::make_tuple(ray1.o,ray1.d, ray2.origin, ray2.d, intersection, true, vertex_id, edge_id));
    } else {
      found = true;
      intersection = ray1(ua);
      event_rays.push_back(std::make_tuple(ray1.o,ray1.d, ray2.origin, ray2.d, intersection, true, vertex_id, edge_id));
    }
  }
}

//This assumes infinite line (ray starting from origin) //This is fine
void intersect_line_line(LineSegment line1, LineSegment line, point2f& intersection, bool& found, Float tol,
                         RayRayInterectionInfo& event_rays, int vertex_id, int edge_id) {
  Float d = DifferenceOfProducts(line.d.x(), line1.d.y(),line.d.y(), line1.d.x());

  if(abs(d) == 0) {
    found = false;
  } else {
    Float dy = line.origin.y() - line1.origin.y();
    Float dx = line.origin.x() - line1.origin.x();
    Float ua = DifferenceOfProducts(line.d.x(), dy, line.d.y(), dx) / d;
    // Float va = DifferenceOfProducts(line1.d.x(), dy, line1.d.y(), dx) / d;

    found = true;
    intersection = point2f(line1.origin.x() + ua * line1.d.x(),
                           line1.origin.y() + ua * line1.d.y());
    event_rays.push_back(std::make_tuple(line1.origin,line1.d, line.origin, line.d, intersection, true, vertex_id, edge_id));
  }
}


bool LAVertex::check_if_equivilent(LAVertex* vert) {
  return(vertex.is_equivalent(vert->vertex, tol) &&
         edge_left.is_equivalent(vert->edge_left, tol) &&
         edge_right.is_equivalent(vert->edge_right, tol) &&
         next->vertex.is_equivalent(vert->next->vertex, tol) &&
         prev->vertex.is_equivalent(vert->prev->vertex, tol));
}

//This is fine
std::vector<OriginalEdge>& LAVertex::OriginalEdges() {
  return(lav->slav->OriginalEdges);
}

Event LAVertex::next_event() {
  std::vector<Event> events;
  std::vector<OriginalEdge> original_edges = OriginalEdges();
  if(is_reflex) {
    // Rcpp::Rcout << "looking for split candidates for vertex #" << vertex_id << ", " << vertex << "\n";
    for(size_t i = 0; i < original_edges.size(); i++) {
      OriginalEdge& edge = original_edges[i];
      // Rcpp::Rcout << "\tconsidering EDGE " << edge.Edge.id << "\n";
      if (edge.Edge.is_equivalent(edge_left, tol) ||
          edge.Edge.is_equivalent(edge_right, tol)) {
        // Rcpp::Rcout << "Co-incident edge, ignoring\n";
        continue;
      }
      bool found_reject;
      point2f point1;
      intersect_ray_line(bisector, edge.Edge, point1, found_reject,  tol, lav->slav->event_rays, vertex_id, edge.Edge.id);
      if(!found_reject) {
        continue;
      }

      // Make normalized copies of vectors
      vec2f norm_edge_left_v = unit_vector(edge_left.d);
      vec2f norm_edge_right_v = unit_vector(edge_right.d);
      vec2f norm_edge_v = unit_vector(edge.Edge.d);

      // Compute dot
      Float leftdot = std::fabs(dot(norm_edge_left_v, norm_edge_v));
      Float rightdot = std::fabs(dot(norm_edge_right_v, norm_edge_v));
      LineSegment selfedge = leftdot < rightdot ? edge_left : edge_right;

      // Ray line intersection

      bool found;
      point2f intersection_point;
      intersect_line_line(edge.Edge, selfedge, intersection_point, found, tol, lav->slav->event_rays, vertex_id, edge.Edge.id);


      if(found && intersection_point.is_equivalent(vertex, tol)) {
        // Rcpp::Rcout << "Found vertex at " << intersection_point << " but is equal to current point\n";
      }
      if(found && !intersection_point.is_equivalent(vertex, tol)) {
        //# locate candidate b
        vec2f lineVec = unit_vector(vertex - intersection_point);
        vec2f edgeVec = unit_vector(edge.Edge.d);
        // if(std::fabs(determinant(bisector.d, lineVec) - 1.0) < tol) {
        //   lineVec = unit_vector(vertex - intersection_point + edgeVec*0.01 );
        // }
        //
        if(determinant(bisector.d, lineVec) < 0) { //When angle between bisector and linevec is greater than 180 degrees,
          edgeVec = -edgeVec;
        }

        bool oriented_correctly;
        vec2f bisecvec = (edgeVec + lineVec); //Tyler change

        if(abs(bisecvec.length()) < tol) {
          continue;
        }
        Ray new_bisector(intersection_point, bisecvec);
        point2f b, b2;
        bool found2;

        // intersect_ray_ray(bisector, new_bisector, b2, found2, tol, lav->slav->event_rays, vertex_id, edge.Edge.id);
        intersect_ray_line(new_bisector, LineSegment(vertex, bisector.d), b, found2, tol, lav->slav->event_rays, vertex_id, edge.Edge.id);
        // Rcpp::Rcout << b << " " << b2 << "\n";
        if(!found2) {
          // Rcpp::Rcout << "Not found due to no intersection between bisectors \n";
          continue;
        }
        // check eligibility of b
        // a valid b should lie within the area limited by the edge and the
        // bisectors of its two vertices:
        vec2f _left_bisector_norm = unit_vector(edge.PrevBisector.d);
        vec2f _left_to_b_norm = unit_vector(b - edge.PrevBisector.o);
        bool xleft = determinant(_left_bisector_norm, _left_to_b_norm) < tol;

        vec2f _right_bisector_norm = unit_vector(edge.Bisector.d);
        vec2f _right_to_b_norm = unit_vector(b - edge.Bisector.o);
        bool xright = determinant(_right_bisector_norm, _right_to_b_norm) > -tol;

        point2f bisector_intersect;
        bool found3;
        intersect_ray_ray(edge.PrevBisector, edge.Bisector, bisector_intersect, found3,
                          tol, lav->slav->event_rays, vertex_id, edge.Edge.id);


        vec2f _edge_edge_norm = unit_vector(edge.Edge.d);
        vec2f _b_to_edge_norm = unit_vector(b - edge.Edge.origin);
        bool xedge = determinant(_edge_edge_norm, _b_to_edge_norm) > -tol;
        if(found3) {
          lav->slav->split_points.push_back(std::make_tuple(edge.PrevBisector.o,
                                                            edge.PrevBisector.d,
                                                            edge.Bisector.o,
                                                            b,
                                                            xleft,
                                                            xright,
                                                            xedge,
                                                            vertex_id,
                                                            edge.Edge.id,
                                                            vertex,
                                                            edge.Bisector.d,
                                                            found3,
                                                            bisector_intersect));
        }
        if (!(xleft && xright && xedge)) {
          // Rcpp::Rcout << "\t\tDiscarded candidate " << b << " (" << xleft << "-" << xright << "-" << xedge << ")\n";
          continue;
        }
        // Rcpp::Rcout << "\t\tFound valid candidate " << b << "at distance " << edge.Edge.distance_to_point(b) << "\n";
        Float edge_to_b_distance = edge.Edge.distance_to_point(b);
        if(edge_to_b_distance < tol) {
          edge_to_b_distance = 0.0;
        }
        events.push_back(Event(edge_to_b_distance, b, this, edge.Edge, true));
      } else {
        // Rcpp::Rcout << "No intersection found with edge " << i << "\n";
      }
    }
  }
  // Intersect ray with ray
  point2f i_prev, i_next;
  bool found_prev, found_next;
  // Rcpp::Rcout << vertex_id << " " << prev->vertex_id << " " << next->vertex_id << "\n";
  intersect_ray_ray(prev->bisector, bisector, i_prev, found_prev, tol, lav->slav->event_rays, vertex_id, edge_left.id);
  intersect_ray_ray(next->bisector, bisector, i_next, found_next, tol, lav->slav->event_rays, vertex_id, edge_right.id);
  //# Make EdgeEvent and append to events

  if(found_prev) {
    Float dist_to_i_prev = edge_left.distance_to_point(i_prev);
    if(dist_to_i_prev < tol) {
      dist_to_i_prev = 0.0;
    }
    events.push_back(Event(dist_to_i_prev, i_prev, prev, this, false));
    // Rcpp::Rcout << "Generated new prev event: " << events[events.size()-1] << " From " << edge_left.origin << " to " << edge_left.adjacent << "\n";
  } else {
    // Rcpp::Rcout << "Didn't find prev\n";
  }
  if(found_next) {
    Float dist_to_i_next = edge_right.distance_to_point(i_next);
    if(dist_to_i_next < tol) {
      dist_to_i_next = 0.0;
    }
    events.push_back(Event(dist_to_i_next, i_next, this, next, false));
    // Rcpp::Rcout << "Generated new next event: " << events[events.size()-1] << " From " << edge_right.origin << " to " << edge_right.adjacent << "\n";

  } else {
    // Rcpp::Rcout << "Didn't find next\n";
  }

  if(!events.empty()) {
    size_t min_i = -1;
    Float distance = std::numeric_limits<Float>::infinity();
    for(size_t i = 0; i < events.size(); i++) {
      Float new_dist = (events[i].intersection - vertex).squared_length();
      // Rcpp::Rcout << "Temp: " << events[i].distance << " " << events[i].split << "\n";
      if(new_dist < distance) {
        min_i = i;
        distance = new_dist;
      }
    }
    if(min_i == -1) {
      throw std::runtime_error("rayskeleton: No candidate events found");
    }
    lav->slav->discarded_points.push_back(std::make_tuple(events[min_i].intersection,
                                                          vertex,
                                                          vertex_id,
                                                          is_reflex,
                                                          events[min_i].split));
    // Rcpp::Rcout << "Generated new event for Vertex " << vertex_id << " @ " << vertex << " Event Info: " << events[min_i] << "\n";
    return(events[min_i]);
  } else {
    // Rcpp::Rcout << "next_event() for vertex #" << vertex_id << " produced no events\n";
    return(Event());
  }
}

void LAVertex::invalidate() {
  if(lav) {
    // Rcpp::Rcout << "Invalidating Vertex #" << vertex_id << "\n";
    lav->invalidate(this);
  } else {
    is_valid = false;
  }
}
