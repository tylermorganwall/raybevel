#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <RProgress.h>
#include "RcppThread.h"
#include "math.h"
#include <iostream>
#include <vector>
#include <unordered_set>
#include <algorithm>
#include <functional>

using namespace Rcpp;

// Custom hash function for a vector of integers
std::size_t vec_hash(const std::vector<int>& vec) {
  std::size_t seed = vec.size();
  for(auto& i : vec) {
    seed ^= i + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  }
  return seed;
}

struct Node {
  int id;
  double x;
  double y;
  double time;
  int edge;
};

struct Link {
  int source;
  int destination;
  int edge;
  double source_time;
  double destination_time;
  int visited;
};

static std::vector<double> getNodePositionById(const std::vector<Node>& nodes, int id) {
  for(const auto& node : nodes) {
    if(node.id == id) {
      return {node.x, node.y};
    }
  }
  return {}; // Empty vector if node ID is not found
}

// Helper function definitions
std::vector<double> getNodePositionById(const std::vector<Node>& nodes, int id);
double determinant2x2(const std::vector<double>& v1, const std::vector<double>& v2);
double dot(const std::vector<double>& v1, const std::vector<double>& v2);

// [[Rcpp::export]]
List convert_ss_to_polygons_rcpp(List ss, int numbercores, bool progress) {
  List nodesDF = ss["nodes"];
  List linksDF = ss["links"];


  // Convert nodes to C++ vector of structs
  IntegerVector node_ids = nodesDF["id"];
  NumericVector node_x = nodesDF["x"];
  NumericVector node_y = nodesDF["y"];
  NumericVector node_time = nodesDF["time"];
  LogicalVector node_edge = nodesDF["edge"];

  std::vector<Node> nodes;
  for(int i = 0; i < node_ids.size(); ++i) {
    Node n = {node_ids[i], node_x[i], node_y[i], node_time[i], node_edge[i]};
    nodes.push_back(n);
  }

  // Convert links to C++ vector of structs
  IntegerVector link_source = linksDF["source"];
  IntegerVector link_dest = linksDF["destination"];
  LogicalVector link_edge = linksDF["edge"];
  NumericVector link_source_time = linksDF["source_time"];
  NumericVector link_dest_time = linksDF["destination_time"];

  std::vector<Link> links;
  for(int i = 0; i < link_source.size(); ++i) {
    Link l = {link_source[i], link_dest[i], link_edge[i], link_source_time[i], link_dest_time[i], false};
    links.push_back(l);
  }
  RcppThread::ProgressCounter pb(link_source.size(), 1, "Polygonizing: ");
  std::vector<std::vector<int>> list_all_polygons;
  list_all_polygons.resize(links.size());
  RcppThread::ThreadPool pool(numbercores);
  // for(int k = 0; k< links.size(); k++) {
  RcppThread::parallelFor(0, links.size(), [&links, &nodes, &list_all_polygons, &pb, &progress] (int k) {
    bool first = true;
    int first_node = links[k].source;
    int tmp_source = first_node;
    int first_dest = links[k].destination;
    int tmp_dest = first_dest;
    std::vector<int> single_polygon_indices;

    while(true) {
      if(tmp_source == first_node && tmp_dest == first_dest && !first) {
        break;
      }
      first = false;

      std::vector<double> node1_position = getNodePositionById(nodes, tmp_source);
      std::vector<double> node2_position = getNodePositionById(nodes, tmp_dest);

      std::vector<double> node1_position_first = getNodePositionById(nodes, first_node);
      std::vector<double> node2_position_first = getNodePositionById(nodes, first_dest);

      // RcppThread::Rcout << first_node << " " << tmp_source << " " << first_dest << " " << tmp_dest << "\n";
      // RcppThread::Rcout << node1_position_first[0] << " " << node1_position_first[1] << " " <<
      //   node1_position[0] << " " << node1_position[1] << " | " << node2_position_first[0] << " " << node2_position_first[1] << " " <<
      //     node2_position[0] << " " << node2_position[1] <<"\n";


      // Calculate the direction vector v1 for the current link
      std::vector<double> v1 = {node2_position[0] - node1_position[0], node2_position[1] - node1_position[1]};

      double best_angle = 180.0;
      int best_dest = -1;
      int best_source = -1;
      double best_len = std::numeric_limits<double>::infinity();

      // Loop over all candidate next links
      for(const auto& link : links) {
        if((link.destination == tmp_dest && link.source == tmp_source) ||
           (link.source == tmp_dest && link.destination == tmp_source)) {
          continue;
        }

        if(link.source != tmp_dest && link.destination != tmp_dest) {
          continue;
        }

        int candidate_source, candidate_dest;
        if(link.destination == tmp_dest) {
          candidate_source = link.destination;
          candidate_dest = link.source;
        } else {
          candidate_source = link.source;
          candidate_dest = link.destination;
        }

        // Calculate the direction vector v2 for the candidate link
        std::vector<double> cs_position = getNodePositionById(nodes, candidate_source);
        std::vector<double> cd_position = getNodePositionById(nodes, candidate_dest);
        std::vector<double> v2 = {cd_position[0] - cs_position[0], cd_position[1] - cs_position[1]};

        // Calculate angle between v1 and v2
        double det_val = determinant2x2(v1, v2);
        double dot_val = dot(v1, v2);
        double angle = std::atan2(det_val, dot_val) * 180.0 / M_PI;

        // Check if this angle is the smallest so far
        double length_v2 = dot(v2, v2);
        if((angle < best_angle) || (angle == best_angle && best_len > length_v2)) {
          best_angle = angle;

          best_dest = candidate_dest;
          best_source = candidate_source;
          best_len = length_v2;
        }
      }
      if(best_source < 0 || best_dest < 0) {
        throw std::runtime_error("Didn't find candidate");
      }

      // Update tmp_source and tmp_dest for the next iteration
      tmp_source = best_source;
      tmp_dest = best_dest;
      single_polygon_indices.push_back(best_dest);
    }
    if(progress) {
      pb++;
    }
    list_all_polygons[k] = single_polygon_indices;
  });
  // }

  // Here I'm doing the sorting and hashing in C++
  std::unordered_map<std::size_t, bool> seen_hashes;
  std::vector<std::vector<int>> unique_polygons;

  for (const auto& polygon : list_all_polygons) {
    std::vector<int> sorted_polygon = polygon;
    std::sort(sorted_polygon.begin(), sorted_polygon.end());
    std::size_t hash = vec_hash(sorted_polygon);

    if (seen_hashes.find(hash) == seen_hashes.end()) {
      // This is a unique polygon, save it and mark it as seen
      unique_polygons.push_back(polygon);
      seen_hashes[hash] = true;
    }
  }

  List all_polygons;
  for(int i = 0; i < unique_polygons.size(); i++) {
    IntegerVector one_polygon;
    for(int j = 0; j < unique_polygons[i].size(); j++) {
      one_polygon.push_back(unique_polygons[i][j]);
    }
    all_polygons.push_back(one_polygon);
  }

  return all_polygons;
}
