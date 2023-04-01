#ifndef SLAVH
#define SLAVH

// DONE

#include "float.h"
#include "point2.h"
#include "vec2.h"
#include "lavertex.h"
#include "lav.h"
#include "originaledge.h"
#include "subtree.h"
#include <utility>

typedef std::vector<std::tuple<point2f, vec2f, point2f, vec2f, point2f, bool, int, int>> RayRayInterectionInfo;
typedef std::vector<std::tuple<point2f, vec2f, point2f, point2f, int, int, int, int, int, point2f, vec2f, bool, point2f>> SplitInfo;
typedef std::vector<std::tuple<point2f, point2f, point2f, point2f, int, int, int, int, int, point2f>> EdgeInfo;

struct SLAV {
  SLAV(std::vector<std::vector<point2f> > contours, Float tol) {
    current_vertex_id = 0;
    for(size_t i = 0; i < contours.size(); i++) {
      lavs.push_back(std::make_shared<LAV>(contours[i], tol, this, current_vertex_id));
      lav_valid.push_back(true);
    }
    //We need the original edges because they drive the bisector angle
    int edge_count = 0;
    for(size_t i = 0; i < lavs.size(); i++) {
      LAVertex* tmp = lavs[i]->head;
      for(size_t j = 0; j < lavs[i]->len; j++) {
        OriginalEdges.push_back(OriginalEdge(tmp->vertex,
                                             tmp->prev->vertex,
                                             tmp->bisector,
                                             tmp->prev->bisector,
                                             edge_count));
        edge_count++;
        tmp = tmp->next;
      }
    }
    error_return = false;
  };
  size_t len() {
    size_t n = 0;
    for(size_t i = 0; i < lav_valid.size(); i++) {
      n += lav_valid[i] ? 1 : 0;
    }
    return(n);
  }
  bool empty() {
    return(len() == 0);
  }
  LAV* operator[](size_t i) {
    return(lavs[i].get());
  }
  int TotalValidVertices() {
    int n_valid = 0;
    for(size_t i = 0; i < lavs.size(); i++) {
      if(lav_valid[i]) {
        LAVertex* tmp = lavs[i]->head;
        for(size_t j = 0; j < lavs[i]->len; j++) {
          if(tmp->is_valid) {
            n_valid++;
          }
          tmp = tmp->next;
        }
      }
    }
    return(n_valid);
  }
  void PrintVertices() {
    for(size_t i = 0; i < lavs.size(); i++) {
      if(lav_valid[i]) {
        LAVertex* tmp = lavs[i]->head;
        for(size_t j = 0; j < lavs[i]->len; j++) {
          if(tmp->is_valid) {
            Rcpp::Rcout << "Vert: " << tmp->vertex_id << " Valid: " << tmp->is_valid << "\n";
          }
          tmp = tmp->next;
        }
      }
    }
  }
  void remove(LAV* lav);
  std::pair<Subtree, std::vector<Event>> handle_split_event(Event event);
  std::pair<Subtree, std::vector<Event>> handle_edge_event(Event event);
  // std::pair<Subtree, std::vector<Event>> handle_simultaneous_event(Event event, );

  Float tol;
  std::vector<std::shared_ptr<LAV> > lavs;
  std::vector<bool> lav_valid;

  std::vector<OriginalEdge> OriginalEdges;
  std::vector<std::tuple<point2f, point2f, int, int, int>> discarded_points;
  SplitInfo split_points;
  bool error_return;

  RayRayInterectionInfo event_rays;
  std::vector<std::shared_ptr<LAVertex> > all_vertices;
  std::vector<LAVertex* > invalidate_verts;
  std::vector<LAVertex* > new_verts;

  int current_vertex_id;
};


#endif
