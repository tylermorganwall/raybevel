#ifndef SLAVH
#define SLAVH

// DONE

#include "float.h"
#include "point2.h"
#include "vec2.h"
#include "originaledge.h"
#include "subtree.h"
#include <utility>
#include <iostream>
#include <fstream>
#include "event.h"

struct LAVertex;
struct LAV;

typedef std::vector<std::tuple<point2f, vec2f, point2f, vec2f, point2f, bool, int, int>> RayRayInterectionInfo;
typedef std::vector<std::tuple<point2f, vec2f, point2f, point2f, int, int, int, int, int, point2f, vec2f, bool, point2f, point2f, vec2f, point2f, vec2f, vec2f>> SplitInfo;
typedef std::vector<std::tuple<point2f, point2f, point2f, point2f, int, int, int, int, int, point2f>> EdgeInfo;
typedef std::vector<std::tuple<point2f, point2f, point2f, point2f, bool>> EdgeEventInfo;

struct SLAV {
  SLAV(std::vector<std::vector<point2f> > contours, Float _tol);
  ~SLAV();
  size_t len();
  bool empty();
  LAV* operator[](size_t i);
  int TotalValidVertices();
  void PrintVertices();
  void remove(LAV* lav);
  void writeDebugLine(std::string msg);
  void writeInfoLine(std::string msg);
  std::pair<Subtree, std::vector<Event>> handle_split_event(Event event);
  std::pair<Subtree, std::vector<Event>> handle_edge_event(Event event);
  std::vector<std::pair<Subtree, std::vector<Event>>> handle_multi_event(std::vector<Event> events);

  Float tol;
  std::vector<std::shared_ptr<LAV> > lavs;

  std::vector<OriginalEdge> OriginalEdges;
  std::vector<std::tuple<point2f, point2f, int, int, int>> discarded_points;
  SplitInfo split_points;
  bool error_return;

  RayRayInterectionInfo event_rays;
  std::vector<std::shared_ptr<LAVertex> > all_vertices;
  std::vector<LAVertex* > invalidate_verts;

  int current_vertex_id;
  int current_edge_id;

  std::ofstream debugfile;
};


#endif
