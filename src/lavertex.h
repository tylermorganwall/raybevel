#ifndef LAVERTEXH
#define LAVERTEXH

#include "point2.h"
#include "mathutil.h"
#include "float.h"
#include "ray.h"
#include "linesegment.h"
#include "originaledge.h"
#include "event.h"
#include "Rcpp.h"
#include <cstdio>

void intersect_ray_ray(Ray ray1, Ray ray2, point2f& intersection, bool& found, Float tol);
void intersect_line_line(LineSegment line1, LineSegment line, point2f& intersection, bool& found, Float tol);

struct LAV;
struct Event;

static char buff[256];

struct LAVertex {
  LAVertex(point2f vertex,
           LineSegment edge_left,  //These are in the original ordering from the polygon
           LineSegment edge_right, //These are in the original ordering from the polygon
           LAV *lav,
           int& id,
           Float tol = 100 * std::numeric_limits<Float>::epsilon()) :
    vertex(vertex), edge_left(edge_left), edge_right(edge_right), lav(lav),
    vertex_id(id)
 {
    next = nullptr;
    prev = nullptr;
    vec2f left_v  =  unit_vector(edge_left.d * -1);
    vec2f right_v =  unit_vector(edge_right.d);
    // is_reflex = determinant(edge_left.d, edge_right.d) > 0.0;
    Float det = determinant(left_v, right_v);
    is_reflex = det > 0.0 ; //Polygon is CCW
    // Deal with straight vertices
    if(abs(det) < tol) {
      std::snprintf(buff,256,"Straight edge found with det = %0.16f for vertex %i (between edges %i and %i)--shouldn't have happened", abs(det), id, edge_left.id,edge_right.id);
      throw std::runtime_error(buff);
    } else {
      is_straight = false;
    }
    is_valid = true;
    bisector = !is_reflex ? Ray(vertex, left_v + right_v) : Ray(vertex, -(left_v + right_v));
    Rcpp::Rcout << "Constructing " << (is_reflex ? "Reflex" : "Convex") << " Vertex" << id << " at pos " << vertex << " with det = " << det << "\n" ;
    // Rcpp::Rcout << "Reflex: " << (is_reflex ? "Yes" : "No ") <<  "\n edge_left.o " << edge_left.origin << " edge_left.d " << edge_left.d << "\n edge_right.o " << edge_right.origin << " edge_right.d " << edge_right.d << "\n" ;
    id++;
  };
  //Determine reflex from direction vectors if given
  LAVertex(point2f vertex,
           LineSegment edge_left,  //These are in the original ordering from the polygon
           LineSegment edge_right, //These are in the original ordering from the polygon
           LAV *lav,
           vec2f dir1, vec2f dir2,
           int& id,
           Float tol = 100 * std::numeric_limits<Float>::epsilon()) :
    vertex(vertex), edge_left(edge_left), edge_right(edge_right), vertex_id(id) , lav(lav) {
    next = nullptr;
    prev = nullptr;
    vec2f left_v  =  unit_vector(edge_left.d * -1);
    vec2f right_v =  unit_vector(edge_right.d);
    Float det = determinant(dir1, dir2);
    is_reflex = det > 0.0; //Polygon is CCW
    if(abs(det) < tol) {
      std::snprintf(buff,256,"Straight edge found with det = %0.16f for vertex %i (between edges %i and %i)--shouldn't have happened", abs(det), id, edge_left.id,edge_right.id);
      throw std::runtime_error(buff);
    } else {
      is_straight = false;
    }
    is_valid = true;
    bisector = !is_reflex ? Ray(vertex, left_v + right_v) : Ray(vertex, -(left_v + right_v));
    Rcpp::Rcout << "Constructing " << (is_reflex ? "Reflex" : "Convex") << " Vertex" << id << " at pos " << vertex << " with det = " << det << "\n" ;
    // Rcpp::Rcout << "Reflex: " << (is_reflex ? "Yes" : "No ") <<  "\n edge_left.o " << edge_left.origin << " edge_left.d " << edge_left.d << "\n edge_right.o " << edge_right.origin << " edge_right.d " << edge_right.d << "\n" ;

    id++;
  };
  LAVertex(point2f vertex,
           LineSegment edge_left,  //These are in the original ordering from the polygon
           LineSegment edge_right, //These are in the original ordering from the polygon
           int& id,
           Float tol = 100 * std::numeric_limits<Float>::epsilon()) :
    vertex(vertex), edge_left(edge_left), edge_right(edge_right) ,lav(nullptr),
    vertex_id(id)
  {
    next = nullptr;
    prev = nullptr;
    vec2f left_v  =  unit_vector(edge_left.d * -1);
    vec2f right_v =  unit_vector(edge_right.d);
    Float det = determinant(left_v, right_v);
    is_reflex = det > 0.0; //Polygon is CCW
    // Deal with straight vertices
    if(abs(det) < tol) {
      std::snprintf(buff,256,"Straight edge found with det = %0.16f for vertex %i (between edges %i and %i)--shouldn't have happened", abs(det), id, edge_left.id,edge_right.id);
      throw std::runtime_error(buff);
    } else {
      is_straight = false;
    }
    is_valid = true;
    bisector = !is_reflex ? Ray(vertex, left_v + right_v) : Ray(vertex, -(left_v + right_v));
    Rcpp::Rcout << "Constructing " << (is_reflex ? "Reflex" : "Convex") << " Vertex" << id << " at pos " << vertex << " with det = " << det << "\n" ;
    // Rcpp::Rcout << "Reflex: " << (is_reflex ? "Yes" : "No ") <<  "\n edge_left.o " << edge_left.origin << " edge_left.d " << edge_left.d << "\n edge_right.o " << edge_right.origin << " edge_right.d " << edge_right.d << "\n" ;
    id++;
  };
  ~LAVertex() {};

  bool check_if_equivilent(LAVertex* vert);
  std::vector<OriginalEdge>& OriginalEdges();
  Event next_event();
  void invalidate();
  point2f vertex;
  LineSegment edge_left; //Changing edge to neighbor
  LineSegment edge_right; //Changing edge to neighbor
  Float tol;
  bool is_reflex;
  bool is_valid;
  bool is_straight;
  Ray bisector; //Changing bisector ray
  LAV* lav;
  LAVertex* next; //Changing adjacent vertex
  LAVertex* prev; //Changing adjacent vertex
  int vertex_id;
};

#endif
