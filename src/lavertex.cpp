#include "lavertex.h"
#include "lav.h"
#include "slav.h"
#include "point2.h"
#include "event.h"
#include "format.h"

void writeInfoLine(std::ofstream* debugfile, std::string msg) {
  // if(debugfile) {
  //   (*debugfile) << "INFO:__name__:" <<  msg << "\n";
  // }
}

LAVertex::LAVertex(point2f vertex,
                   // LineSegment edge_right, //These are in the original ordering from the polygon

         LineSegment edge_left,  //These are in the original ordering from the polygon
         LineSegment edge_right, //These are in the original ordering from the polygon
         LAV *lav,
         int& id,
         std::ofstream* debugfile,
         Float tol = 1e-10) :
  vertex(vertex), edge_left(edge_left), edge_right(edge_right), lav(lav),
  vertex_id(id), tol(tol)
{
  next = nullptr;
  prev = nullptr;
  vec2f left_v  =  unit_vector(edge_left.d * -1);
  vec2f right_v =  unit_vector(edge_right.d);
  Float det = determinant(left_v, right_v);
  is_reflex = det < 0.0 ; //Polygon is CCW
  // Deal with straight vertices
  // if(abs(det) < tol) {
  //   std::snprintf(buff,256,"Straight edge found with det = %0.16f for vertex %i (between edges %i and %i)--shouldn't have happened", abs(det), id, edge_left.id,edge_right.id);
  //   throw std::runtime_error(buff);
  // } else {
  is_straight = false;
  // }
  is_valid = true;
  bisector = !is_reflex ? Ray(vertex, left_v + right_v) : Ray(vertex, -(left_v + right_v));
  writeInfoLine(debugfile, "Created vertex " + PrintDetailedInfo());
  // Rcpp::Rcout << "Constructing " << (is_reflex ? "Reflex" : "Convex") << " Vertex" << vertex_id << " with bisector " <<bisector.d <<"\n";
  // Rcpp::Rcout << "Constructing " << (is_reflex ? "Reflex" : "Convex") << " Vertex" << id << " at pos " << vertex << " with det = " << det << "\n" ;
  // Rcpp::Rcout << "Reflex: " << (is_reflex ? "Yes" : "No ") <<  "\n edge_left.o " << edge_left.origin << " edge_left.d " << edge_left.d << "\n edge_right.o " << edge_right.origin << " edge_right.d " << edge_right.d << "\n" ;
  id++;
};

//Determine reflex from direction vectors if given
LAVertex::LAVertex(point2f vertex,
         // LineSegment edge_right, //These are in the original ordering from the polygon
         LineSegment edge_left,  //These are in the original ordering from the polygon
         LineSegment edge_right, //These are in the original ordering from the polygon

         LAV *lav,
         vec2f dir1, vec2f dir2,
         int& id,
         std::ofstream* debugfile,
         Float tol = 1e-10) :
  vertex(vertex), edge_left(edge_left), edge_right(edge_right), vertex_id(id) , lav(lav), tol(tol) {
  next = nullptr;
  prev = nullptr;
  vec2f left_v  =  unit_vector(edge_left.d * -1);
  vec2f right_v =  unit_vector(edge_right.d);
  Float det = determinant(dir1, dir2);
  is_reflex = det < 0.0; //Polygon is CCW
  // if(abs(det) < tol) {
  //   std::snprintf(buff,256,"Straight edge found with det = %0.16f for vertex %i (between edges %i and %i)--shouldn't have happened", abs(det), id, edge_left.id,edge_right.id);
  //   throw std::runtime_error(buff);
  // } else {
  is_straight = false;
  // }
  is_valid = true;
  bisector = !is_reflex ? Ray(vertex, left_v + right_v) : Ray(vertex, -(left_v + right_v));
  writeInfoLine(debugfile, "Created vertex " + PrintDetailedInfo());
  // Rcpp::Rcout << "Constructing " << (is_reflex ? "Reflex" : "Convex") << " Vertex" << vertex_id << " with bisector " <<bisector.d <<"\n";

  // Rcpp::Rcout << "Constructing " << (is_reflex ? "Reflex" : "Convex") << " Vertex" << id << " at pos " << vertex << " with det = " << det << "\n" ;
  // Rcpp::Rcout << "Reflex: " << (is_reflex ? "Yes" : "No ") <<  "\n edge_left.o " << edge_left.origin << " edge_left.d " << edge_left.d << "\n edge_right.o " << edge_right.origin << " edge_right.d " << edge_right.d << "\n" ;

  id++;
};

LAVertex::LAVertex(point2f vertex,
         // LineSegment edge_right, //These are in the original ordering from the polygon

         LineSegment edge_left,  //These are in the original ordering from the polygon
         LineSegment edge_right, //These are in the original ordering from the polygon
         int& id,
         std::ofstream* debugfile,
         Float tol = 1e-10) :
  vertex(vertex), edge_left(edge_left), edge_right(edge_right) ,lav(nullptr),
  vertex_id(id), tol(tol)
{
  next = nullptr;
  prev = nullptr;
  vec2f left_v  =  unit_vector(edge_left.d * -1);
  vec2f right_v =  unit_vector(edge_right.d);
  Float det = determinant(left_v, right_v);
  is_reflex = det < 0.0; //Polygon is CCW
  // Deal with straight vertices
  // if(abs(det) < tol) {
  //   std::snprintf(buff,256,"Straight edge found with det = %0.16f for vertex %i (between edges %i and %i)--shouldn't have happened", abs(det), id, edge_left.id,edge_right.id);
  //   throw std::runtime_error(buff);
  // } else {
  is_straight = false;
  // }
  is_valid = true;
  bisector = !is_reflex ? Ray(vertex, left_v + right_v) : Ray(vertex, -(left_v + right_v));
  writeInfoLine(debugfile, "Created vertex " + PrintDetailedInfo());
  // Rcpp::Rcout << "Constructing " << (is_reflex ? "Reflex" : "Convex") << " Vertex" << id << " with bisector " <<bisector.d <<"\n";

  // Rcpp::Rcout << "Constructing " << (is_reflex ? "Reflex" : "Convex") << " Vertex" << id << " at pos " << vertex << " with det = " << det << "\n" ;
  // Rcpp::Rcout << "Reflex: " << (is_reflex ? "Yes" : "No ") <<  "\n edge_left.o " << edge_left.origin << " edge_left.d " << edge_left.d << "\n edge_right.o " << edge_right.origin << " edge_right.d " << edge_right.d << "\n" ;
  id++;
};

std::string bool2str(bool b) {
  return(b ? "True" : "False");
}

void intersect_ray_ray(Ray ray1, Ray ray2, point2f& intersection, bool& found, Float tol,
                       RayRayInterectionInfo& event_rays, int vertex_id, int edge_id) {
  Float d = DifferenceOfProducts(ray2.d.y(), ray1.d.x(),ray2.d.x(), ray1.d.y());

  if(abs(d) == 0) {
    found = false;
  } else {
    Float dy = ray1.o.y() - ray2.o.y();
    Float dx = ray1.o.x() - ray2.o.x();
    Float ua = DifferenceOfProducts(ray2.d.x(), dy, ray2.d.y(), dx) / d;
    Float va = DifferenceOfProducts(ray1.d.x(), dy, ray1.d.y(), dx) / d;

    if(ua < 0.0 || va < 0.0) {
      found = false;
    } else {
      found = true;
      intersection = ray1(ua);
      event_rays.push_back(std::make_tuple(ray1.o,ray1.d, ray2.o, ray2.d, intersection, true, vertex_id, edge_id));
    }
  }
}

void intersect_ray_line(Ray ray1, LineSegment ray2, point2f& intersection, bool& found, Float tol,
                       RayRayInterectionInfo& event_rays, int vertex_id, int edge_id) {
  Float d = DifferenceOfProducts(ray2.d.y(), ray1.d.x(),ray2.d.x(), ray1.d.y());

  if(abs(d) == 0) {
    found = false;
  } else {
    Float dy = ray1.o.y() - ray2.origin.y();
    Float dx = ray1.o.x() - ray2.origin.x();
    Float ua = DifferenceOfProducts(ray2.d.x(), dy, ray2.d.y(), dx) / d;
    Float va = DifferenceOfProducts(ray1.d.x(), dy, ray1.d.y(), dx) / d;

    if(ua < 0.0) {
      found = false;
    } else {
      found = true;
      // intersection = point2f(ray2.origin.x() + va * ray2.d.x(),
      //                        ray2.origin.y() + va * ray2.d.y());
      intersection = ray1(ua);
      event_rays.push_back(std::make_tuple(ray1.o,ray1.d, ray2.origin, ray2.d, intersection, true, vertex_id, edge_id));
    }
  }
}

//This assumes infinite line (ray starting from origin) //This is fine
void intersect_line_line(LineSegment line1, LineSegment line, point2f& intersection, bool& found, Float tol,
                         RayRayInterectionInfo& event_rays, int vertex_id, int edge_id) {
  Float d = DifferenceOfProducts(line.d.y(), line1.d.x(),line.d.x(), line1.d.y());

  if(abs(d) == 0) {
    found = false;
  } else {
    Float dy = line1.origin.y() - line.origin.y();
    Float dx = line1.origin.x() - line.origin.x();
    Float ua = DifferenceOfProducts(line1.d.x(), dy, line1.d.y(), dx) / d;

    found = true;
    intersection = point2f(line.origin.x() + ua * line.d.x(),
                           line.origin.y() + ua * line.d.y());
    event_rays.push_back(std::make_tuple(line1.origin,line1.d, line.origin, line.d, intersection, true, vertex_id, edge_id));
  }
}

vec2f OrthogonalLeft(vec2f v) {
  return(vec2f(-v.y(), v.x()));
}

vec2f BisectorNormalized(vec2f norm1, vec2f norm2) {
  vec2f e1v = OrthogonalLeft(norm1);
  vec2f e2v = OrthogonalLeft(norm2);

  // 90 - 180 || 180 - 270
  if (dot(norm1, norm2) > 0) {
    return e1v + e2v;
  }

  // 0 - 180
  vec2f ret = -norm1;
  ret += norm2;

  // 270 - 360
  if (dot(e1v, norm2) < 0) {
    ret = -ret;
  }

  return(ret);
}


Ray CorrectBisectorDirection(Ray bisector, LAVertex* beginNextVertex,
                             LAVertex* endPreviousVertex, LineSegment beginEdge, LineSegment endEdge,
                             Float tol) {
  // New bisector for vertex is created using connected edges. For
  // parallel edges numerical error may appear and direction of created
  // bisector is wrong. It for parallel edges direction of edge need to be
  // corrected using location of vertex.
  LineSegment& beginEdge2 = beginNextVertex->edge_left;
  LineSegment& endEdge2 = endPreviousVertex->edge_right;

  if (!beginEdge.is_equivalent(beginEdge2, tol) || !endEdge.is_equivalent(endEdge2, tol)) {
    throw std::runtime_error("Edges not equivalent when correcting bisector");
  }

  // Check if edges are parallel and in opposite direction to each other.
  if (dot(unit_vector(beginEdge.d),unit_vector(endEdge.d)) < -0.99) {
    vec2f n1 = unit_vector(endPreviousVertex->vertex - bisector.o);
    vec2f n2 = unit_vector(bisector.o - beginNextVertex->vertex);
    vec2f bisectorPrediction = BisectorNormalized(n1, n2);

    // Bisector is calculated in opposite direction to edges and center.
    if (dot(bisector.d, bisectorPrediction) < 0) {
      bisector.d = -bisector.d;
    }
  }
  return(bisector);
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
    lav->slav->writeDebugLine("looking for split candidates for vertex " + PrintInfo());
    for(size_t i = 0; i < original_edges.size(); i++) {
      OriginalEdge& edge = original_edges[i];
      lav->slav->writeDebugLine(" considering EDGE " + edge.PrintInfo());

      if (edge.Edge.is_equivalent(edge_left, tol) ||
          edge.Edge.is_equivalent(edge_right, tol)) {
        // Rcpp::Rcout << "Co-incident edge, ignoring\n";
        continue;
      }
      // bool found_reject;
      // point2f point1;
      // intersect_ray_line(bisector, edge.Edge, point1, found_reject,  tol, lav->slav->event_rays, vertex_id, edge.Edge.id);
      // if(!found_reject) {
      //   continue;
      // }

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
        if(dot(edgeVec, lineVec) < 0) { //When angle between bisector and linevec is greater than 180 degrees,
          edgeVec = -edgeVec;
        }

        // if(determinant(bisector.d, lineVec) < 0) { //When angle between bisector and linevec is greater than 180 degrees,
        //   edgeVec = -edgeVec;
        // }

        bool oriented_correctly;
        vec2f bisecvec = (edgeVec + lineVec); //Tyler change

        if(abs(bisecvec.length()) < tol) {
          continue;
        }
        LineSegment new_bisector(intersection_point, bisecvec); //This changes matches polyskel
        // Ray new_bisector(intersection_point, bisecvec);

        point2f b, b2;
        bool found2;

        // intersect_ray_ray(bisector, new_bisector, b2, found2, tol, lav->slav->event_rays, vertex_id, edge.Edge.id);
        intersect_ray_line(bisector, new_bisector, b, found2, tol, lav->slav->event_rays, vertex_id, edge.Edge.id);
        // intersect_line_line(new_bisector, LineSegment(vertex, bisector.d), b, found2, tol, lav->slav->event_rays, vertex_id, edge.Edge.id);

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
        bool xleft = determinant(_left_bisector_norm, _left_to_b_norm) > -tol; //polyskel change

        vec2f _right_bisector_norm = unit_vector(edge.Bisector.d);
        vec2f _right_to_b_norm = unit_vector(b - edge.Bisector.o);
        bool xright = determinant(_right_bisector_norm, _right_to_b_norm) < tol; //polyskel change
        // Rcpp::Rcout << tol << "\n";

        point2f bisector_intersect;
        bool found3;
        intersect_ray_ray(edge.PrevBisector, edge.Bisector, bisector_intersect, found3,
                          tol, lav->slav->event_rays, vertex_id, edge.Edge.id);


        vec2f _edge_edge_norm = unit_vector(edge.Edge.d);
        vec2f _b_to_edge_norm = unit_vector(b - edge.Edge.origin);
        bool xedge = determinant(_edge_edge_norm, _b_to_edge_norm) < tol;
        if(found3 && xleft && xright && xedge) {
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
                                                            bisector_intersect,
                                                            intersection_point,
                                                            bisecvec,
                                                            edge.Edge.origin,
                                                            edge.Edge.d,
                                                            selfedge.d));
        }
        if (!(xleft && xright && xedge)) {
          std::string xlefts = bool2str(xleft);
          std::string xrights = bool2str(xright);
          std::string xedges = bool2str(xedge);

          lav->slav->writeDebugLine("\t\tDiscarded candidate " + b.PrintInfo() +
                                    vformat(" (%s-%s-%s)",
                                            xlefts.c_str(),
                                            xrights.c_str(),
                                            xedges.c_str()));

          // Rcpp::Rcout << "\t\tDiscarded candidate " << b << " (" << xleft << "-" << xright << "-" << xedge << ")\n";
          continue;
        }
        lav->slav->writeDebugLine("\t\tFound valid candidate " + b.PrintInfo());

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
    lav->slav->writeInfoLine(vformat("Generated new prev event for %s: %s, with edge %s",
                                      PrintInfo().c_str(),
                                      events.back().PrintInfo().c_str(),
                                      edge_left.PrintInfo().c_str()));

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

    lav->slav->writeInfoLine(vformat("Generated new next event for %s: %s, with edge %s",
                                     PrintInfo().c_str(),
                                     events.back().PrintInfo().c_str(),
                                     edge_left.PrintInfo().c_str()));
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
    lav->slav->writeInfoLine(vformat("Generated new event for %s: %s",
                                     PrintInfo().c_str(),
                                     events[min_i].PrintInfo().c_str()));
    // Rcpp::Rcout << "Generated new event for Vertex " << vertex_id << " @ " << vertex << " Event Info: " << events[min_i] << "\n";
    return(events[min_i]);
  } else {
    // Rcpp::Rcout << "next_event() for vertex #" << vertex_id << " produced no events\n";
    return(Event());
  }
}

void LAVertex::invalidate() {
  if(lav) {
    lav->slav->writeDebugLine(vformat("Invalidating %s",
                                      PrintInfo().c_str()));
    lav->invalidate(this);
  } else {
    is_valid = false;
  }
}

std::string LAVertex::PrintInfo() {
  return(vformat("Vertex (%.2f;%.2f)", vertex.x(), vertex.y()));
}

std::string LAVertex::PrintDetailedInfo() {
  return(vformat("Vertex (%s) (%.2f;%.2f), bisector %s, edges %s %s",
                 (is_reflex ? "reflex"  : "convex"),
                 vertex.x(),
                 vertex.y(),
                 bisector.PrintInfo().c_str(),
                 edge_left.PrintInfo().c_str(),
                 edge_right.PrintInfo().c_str()));
}
