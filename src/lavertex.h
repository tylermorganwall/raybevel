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
#include "slav.h"

void intersect_ray_ray(Ray ray1, Ray ray2, point2f& intersection, bool& found, Float tol);
void intersect_line_line(LineSegment line1, LineSegment line, point2f& intersection, bool& found, Float tol);
vec2f OrthogonalLeft(vec2f v);
vec2f BisectorNormalized(vec2f norm1, vec2f norm2);
Ray CorrectBisectorDirection(Ray bisector, LAVertex* beginNextVertex,
                             LAVertex* endPreviousVertex, LineSegment beginEdge, LineSegment endEdge,
                             Float tol);

struct LAV;
struct Event;

void writeInfoLine(std::ofstream debugfile, std::string msg);

static char buff[256];

struct LAVertex {
  LAVertex(point2f vertex,
           // LineSegment edge_right, //These are in the original ordering from the polygon

           LineSegment edge_left,  //These are in the original ordering from the polygon
           LineSegment edge_right, //These are in the original ordering from the polygon
           LAV *lav,
           int& id,
           std::ofstream* debugfile,
           Float tol);
  //Determine reflex from direction vectors if given
  LAVertex(point2f vertex,
           // LineSegment edge_right, //These are in the original ordering from the polygon

           LineSegment edge_left,  //These are in the original ordering from the polygon
           LineSegment edge_right, //These are in the original ordering from the polygon
           LAV *lav,
           vec2f dir1, vec2f dir2,
           int& id,
           std::ofstream* debugfile,
           Float tol);
  LAVertex(point2f vertex,
           // LineSegment edge_right, //These are in the original ordering from the polygon

           LineSegment edge_left,  //These are in the original ordering from the polygon
           LineSegment edge_right, //These are in the original ordering from the polygon
           int& id,
           std::ofstream* debugfile,
           Float tol);
  ~LAVertex() {};

  bool check_if_equivilent(LAVertex* vert);
  std::string PrintInfo();
  std::string PrintDetailedInfo();
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
