#include "lavertex.h"
#include "lav.h"
#include "slav.h"
#include "point2.h"
#include "event.h"

#include "Rcpp.h"

void intersect_ray_line(LineSegment line, Ray ray, point2f& intersection, bool& found) {
  Float d = ray.d.y() * line.d.x() - ray.d.x() * line.d.y();
  if(d == 0) {
    found = false;
  } else {
    Float dy = line.origin.y() - ray.o.y();
    Float dx = line.origin.x() - ray.o.x();
    Float ua = (ray.d.x() * dy - ray.d.y() * dx) / d;
    if(ua <= 0.0 || ua >= 1.0) {
      found = false;
    } else {
      found = true;
      intersection = point2f(line.origin.x() + ua * line.d.x(),
                             line.origin.y() + ua * line.d.y());
    }
  }
}

void intersect_ray_ray(Ray ray1, Ray ray2, point2f& intersection, bool& found) {
  Float d = ray2.d.y() * ray1.d.x() - ray2.d.x() * ray1.d.y();
  if(d == 0) {
    found = false;
  } else {
    Float dy = ray1.o.y() - ray2.o.y();
    Float dx = ray1.o.x() - ray2.o.x();
    Float ua = (ray2.d.x() * dy - ray2.d.y() * dx) / d;
    if(ua <= 0.0) {
      found = false;
    } else {
      found = true;
      intersection = point2f(ray1.o.x() + ua * ray1.d.x(),
                             ray1.o.y() + ua * ray1.d.y());
    }
  }
}

void intersect_line_line(LineSegment line1, LineSegment line, point2f& intersection, bool& found) {
  Float d = line.d.y() * line1.d.x() - line.d.x() * line1.d.y();
  if(d == 0) {
    found = false;
  } else {
    Float dy = line.origin.y() - line1.origin.y();
    Float dx = line.origin.x() - line1.origin.x();
    Float ua = (line1.d.x() * dy - line.d.y() * dx) / d;
    if(ua <= 0.0 || ua >= 1.0) {
      found = false;
    } else {
      found = true;
      intersection = point2f(line.origin.x() + ua * line.d.x(),
                             line.origin.y() + ua * line.d.y());
    }
  }
}


bool LAVertex::check_if_equivilent(LAVertex* vert) {
  return(vertex.is_equivalent(vert->vertex, tol) &&
         edge_left.is_equivalent(vert->edge_left, tol) &&
         edge_right.is_equivalent(vert->edge_right, tol) &&
         next->vertex.is_equivalent(vert->next->vertex, tol) &&
         prev->vertex.is_equivalent(vert->prev->vertex, tol));
}

std::vector<OriginalEdge>& LAVertex::OriginalEdges() {
  return(lav->slav->OriginalEdges);
}

Event LAVertex::next_event() {
  std::vector<Event> events;
  std::vector<OriginalEdge> original_edges = OriginalEdges();
  Rcpp::Rcout << "is_straight: " << (is_straight ? "true" : "false") << "\n";
  if(is_straight) {
    return(Event());
  }
  if(is_reflex) {
    for(size_t i = 0; i < original_edges.size(); i++) {
      OriginalEdge& edge = original_edges[i];
      if (edge.Edge.is_equivalent(edge_left, tol) ||
          edge.Edge.is_equivalent(edge_right, tol)) {
        Rcpp::Rcout << "Co-incident edge, ignoring\n";
        continue;
      }
      Rcpp::Rcout << "Vert " << vertex_id << " : " << (is_reflex ? "Reflex" : "Nonreflex") << " Edge " << i << " " <<
        edge.Edge.origin << " to " << edge.Edge.adjacent << "\n";
      // Make normalized copies of vectors
      vec2f norm_edge_left_v = unit_vector(edge_left.d);
      vec2f norm_edge_right_v = unit_vector(edge_right.d);
      vec2f norm_edge_v = unit_vector(edge.Edge.d);

      // Compute dot
      Float leftdot = abs(dot(norm_edge_left_v, norm_edge_v));
      Float rightdot = abs(dot(norm_edge_right_v, norm_edge_v));
      LineSegment selfedge = leftdot < rightdot ? edge_left : edge_right;

      // Ray line intersection
      bool found;
      point2f intersection_point;
      intersect_line_line(edge.Edge, selfedge, intersection_point, found);
      if(found && intersection_point.is_equivalent(vertex, tol)) {
        Rcpp::Rcout << "Found vertex at " << intersection_point << " but is equal to current point\n";
      }
      if (found && !intersection_point.is_equivalent(vertex, tol)) {
        //# locate candidate b
        vec2f linvec = unit_vector(vertex - intersection_point);
        vec2f edvec = unit_vector(edge.Edge.d);
        if(dot(linvec, edvec) < 0) {
          edvec = -edvec;
        }
        vec2f bisecvec = edvec + linvec;
        if(abs(bisecvec.length()) < tol) {
          Rcpp::Rcout << "Not found\n";

          continue;
        }
        Ray new_bisector = Ray(intersection_point, bisecvec);
        point2f b;
        bool found2;
        intersect_ray_ray(bisector, new_bisector, b, found2);
        if(!found2) {
          Rcpp::Rcout << "Not found due to \n";
          continue;
        }
        // check eligibility of b
        // a valid b should lie within the area limited by the edge and the
        // bisectors of its two vertices:
        vec2f _left_bisector_norm = unit_vector(edge.PrevBisector.d);
        vec2f _left_to_b_norm = unit_vector(b - edge.PrevBisector.o);
        bool xleft = determinant(_left_bisector_norm, _left_to_b_norm) > 0;

        vec2f _right_bisector_norm = unit_vector(edge.Bisector.d);
        vec2f _right_to_b_norm = unit_vector(b - edge.Bisector.o);
        bool xright = determinant(_right_bisector_norm, _right_to_b_norm) < 0;

        vec2f _edge_edge_norm = unit_vector(edge.Edge.d);
        vec2f _b_to_edge_norm = unit_vector(b - edge.Edge.origin);
        bool xedge = determinant(_edge_edge_norm, _b_to_edge_norm) < 0;

        if (!(xleft and xright and xedge)) {
          Rcpp::Rcout << "Discarded candidate " << b << "\n";
          // log.debug(
          //   '\t\tDiscarded candidate %s (%s-%s-%s)',
          //   b, xleft, xright, xedge)
          continue;
        }

        // log.debug('\t\tFound valid candidate %s', b)
        Float _dist_line_to_b = edge.Edge.distance_to_point(b);
        if(_dist_line_to_b < tol) {
          _dist_line_to_b = 0.0;
        }
        Event split_event = Event(_dist_line_to_b, b, this, edge.Edge, true);
        events.push_back(split_event);
        Rcpp::Rcout << "Adding Split Event at " << b << "\n";
      } else {
        Rcpp::Rcout << "No intersection found\n";
      }
    }
  }
  // Intersect line2d with line2d (does not assume lines are infinite)
  point2f i_prev, i_next;
  bool found_prev, found_next;
  intersect_ray_ray(prev->bisector, bisector, i_prev, found_prev);
  intersect_ray_ray(next->bisector, bisector, i_next, found_next);
  //# Make EdgeEvent and append to events
  if(found_prev) {
    Float dist_to_i_prev = edge_left.distance_to_point(i_prev);
    if(dist_to_i_prev < tol) {
      dist_to_i_prev = 0.0;
    }
    events.push_back(Event(dist_to_i_prev, i_prev, prev.get(), this, false));
    Rcpp::Rcout << "Adding Edge Event (prev) at " << i_prev << "\n";
  }
  if(found_next) {
    Float dist_to_i_next = edge_right.distance_to_point(i_next);
    if(dist_to_i_next < tol) {
      dist_to_i_next = 0.0;
    }
    events.push_back(Event(dist_to_i_next, i_next, this, next.get(), false));
    Rcpp::Rcout << "Adding Edge Event (next) at " << i_next << "\n";
  }
  if(!events.empty()) {
    size_t min_i = -1;
    Float distance = std::numeric_limits<double>::infinity();
    for(size_t i = 0; i < events.size(); i++) {
      Float new_dist = (events[i].intersection).length();
      if(new_dist < distance) {
        min_i = i;
        distance = new_dist;
      }
    }
    if(min_i == -1) {
      throw std::runtime_error("rayskeleton: No candidate events found");
    }
    return(events[min_i]);
  } else {
    return(Event());
  }
}

void LAVertex::invalidate() {
  if(!lav->none) {
    lav->invalidate(this);
  } else {
    is_valid = false;
  }
}
