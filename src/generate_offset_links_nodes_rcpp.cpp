#include <Rcpp.h>
#include <vector>
#include <cmath>
#include "math.h"
#include "RProgress.h"

using namespace Rcpp;

struct Node {
  int id;
  double x;
  double y;
  double time;
  int edge;
  int local_maxima;
};

struct Link {
  int source;
  int destination;
  int edge;
  int maxima_source;
  int maxima_dest;
  int visited;
  double source_time;
  double destination_time;
};

struct NewNode {
  int polygon_id;
  int source;
  int destination;
  int new_id;
  double x;
  double y;
  double time;
  double x1;
  double y1;
  double x2;
  double y2;
  int new_node;
};

static std::vector<double> getNodePositionById(const std::vector<Node>& nodes, int id) {
  for(const auto& node : nodes) {
    if(node.id == id) {
      return {node.x, node.y};
    }
  }
  return {}; // Empty vector if node ID is not found
}

double getNodeTimeById(const std::vector<Node>& nodes, int id) {
  for(const auto& node : nodes) {
    if(node.id == id) {
      return node.time;
    }
  }
  return -1; // Empty vector if node ID is not found
}

// Convert DataFrame to std::vector of Nodes
std::vector<Node> convertDataFrameToNodes(DataFrame df) {
  size_t nrows = df.nrow();
  IntegerVector id = Rcpp::as<IntegerVector>(df["id"]);
  NumericVector x = Rcpp::as<NumericVector>(df["x"]);
  NumericVector y = Rcpp::as<NumericVector>(df["y"]);
  NumericVector time = Rcpp::as<NumericVector>(df["time"]);
  IntegerVector edge = Rcpp::as<IntegerVector>(df["edge"]);
  IntegerVector maxima = Rcpp::as<IntegerVector>(df["local_maxima"]);

  std::vector<Node> nodes;
  nodes.resize(nrows);
  for (size_t i = 0; i < nrows; ++i) {
    nodes[i].id = id(i);
    nodes[i].x = x(i);
    nodes[i].y = y(i);
    nodes[i].time = time(i);
    nodes[i].edge = edge(i);
    nodes[i].local_maxima = maxima(i);
  }
  return nodes;
}

std::vector<double> interpolate_location(const std::vector<double>& node_start,
                                         const std::vector<double>& node_end,
                                         double height_start,
                                         double height_end,
                                         double height) {
  // Initialize result vector
  std::vector<double> result(2, 0.0);

  // Handling edge cases
  if (height_end - height_start == 0.0) {
    return node_end;
  }
  if (height == height_end) {
    return node_end;
  }
  if (height == height_start) {
    return node_start;
  }

  // Calculate interpolation factor t
  double t = (height - height_start) / (height_end - height_start);

  // Perform the interpolation
  result[0] = node_start[0] * (1 - t) + node_end[0] * t;
  result[1] = node_start[1] * (1 - t) + node_end[1] * t;

  return result;
}

// Convert DataFrame to std::vector of Links
std::vector<Link> convertDataFrameToLinks(DataFrame df) {
  size_t nrows = df.nrow();
  IntegerVector source = Rcpp::as<IntegerVector>(df["source"]);
  IntegerVector destination = Rcpp::as<IntegerVector>(df["destination"]);
  LogicalVector edge = Rcpp::as<LogicalVector>(df["edge"]);
  NumericVector source_time = Rcpp::as<NumericVector>(df["source_time"]);
  NumericVector destination_time = Rcpp::as<NumericVector>(df["destination_time"]);
  LogicalVector local_maxima_destination = Rcpp::as<LogicalVector>(df["local_maxima_destination"]);
  LogicalVector local_maxima_source = Rcpp::as<LogicalVector>(df["local_maxima_source"]);

  std::vector<Link> links;
  links.resize(nrows);

  for (size_t i = 0; i < nrows; ++i) {
    links[i].source = (int)source(i);
    links[i].destination = (int)destination(i);
    links[i].edge = (int)edge(i);
    links[i].source_time = (double)source_time(i);
    links[i].destination_time = (double)destination_time(i);
    links[i].maxima_source = (int)local_maxima_source(i);
    links[i].maxima_dest = (int)local_maxima_destination(i);
    links[i].visited = (int)0;
  }
  return links;
}

// [[Rcpp::export]]
List generate_offset_links_nodes_rcpp(DataFrame ss_links, DataFrame ss_nodes, NumericVector offsets, bool return_polys = false, bool progress = false) {

  std::vector<Link> links = convertDataFrameToLinks(ss_links);
  std::vector<Node> nodes = convertDataFrameToNodes(ss_nodes);

  int max_node = -1;
  for(size_t ii = 0; ii < (size_t)nodes.size(); ii++) {
    max_node = std::max(nodes[ii].id,max_node);
  }
  int new_node_start = max_node + 1;

  List return_data;

  // Initialize containers for new links and nodes
  std::vector<Link> newLinks;
  std::vector<Node> newNodes;

  // Main loop to iterate over offsets
  RProgress::RProgress pb("Inserting links: :percent (:eta remaining)");
  if(progress) {
   pb.set_total(offsets.size());
   pb.tick(0);
  }
  for(size_t ii = 0; ii < (size_t)offsets.size(); ++ii) {
    if(progress) {
      pb.tick(ii);
    }
    double offset = offsets[ii];
    std::vector<bool> visited(links.size());
    std::vector<bool> away_from_offset(links.size());
    std::vector<bool> equal_to_offset(links.size());
    std::vector<NewNode> new_node_info;

    for(size_t jj = 0; jj < links.size(); jj++) {
      visited[jj] = (bool)links[jj].edge;
      away_from_offset[jj] = (links[jj].source_time < offset && links[jj].destination_time < offset) ||
                             (links[jj].source_time > offset && links[jj].destination_time > offset);
      equal_to_offset[jj] = links[jj].source_time == offset;
    }

    int first_node = -1;
    int first_dest = -1;
    int tmp_source = -1;
    int tmp_dest = -1;
    int num_polygons = 1;
    bool first = true;
    bool any_picked = false;
    bool new_poly = true;

    for(size_t jj = 0; jj < links.size(); jj++) {
      if(links[jj].maxima_dest && links[jj].destination_time >= offset) {
        first_node = links[jj].source;
        first_dest = links[jj].destination;
        tmp_source = first_node;
        tmp_dest = first_dest;
        break;
      }
    }

    while(true) {
      Rcpp::checkUserInterrupt();
      // if(tmp_source == first_node && tmp_dest == first_dest && !first) {
      //   break;
      // }
      std::vector<Link> non_visited_links;
      for(size_t jj = 0; jj < links.size(); ++jj) {
        if(!visited[jj]) {
          non_visited_links.push_back(links[jj]);
        }
        if(non_visited_links.size() > links.size()) {
          throw std::runtime_error("Memory corruption");
        }
      }
      std::vector<Link> remaining_links;
      if(!first || any_picked) {
        if(tmp_source < 0 || tmp_dest < 0) {
          throw std::runtime_error("Bug or malformed input: next node not found");
        }
        if(tmp_source == first_node && tmp_dest == first_dest) {
          new_poly = true;
          for(size_t jj = 0; jj < links.size(); ++jj) {
            if(!visited[jj] && !away_from_offset[jj] && !links[jj].edge) {
              remaining_links.push_back(links[jj]);
            }
          }
          if(remaining_links.size() == 0) {
            break;
          }
          first_node = remaining_links[0].source;
          first_dest = remaining_links[0].destination;
          tmp_source = first_node;
          tmp_dest = first_dest;
          num_polygons++;
        }
      }
      // size_t remaining_links_count = std::count_if(visited.begin(), visited.end(), [](bool l) { return !l; });

      first = false;

      std::vector<double> node1_position = getNodePositionById(nodes, tmp_source);
      std::vector<double> node2_position = getNodePositionById(nodes, tmp_dest);
      double node1_time = getNodeTimeById(nodes, tmp_source);
      double node2_time = getNodeTimeById(nodes, tmp_dest);

      for(size_t jj = 0; jj < links.size(); jj++) {
        if((links[jj].source == tmp_source &&
            links[jj].destination == tmp_dest) ||
           (links[jj].destination == tmp_source &&
            links[jj].source == tmp_dest)) {
          visited[jj] = true;
        }
      }
      std::vector<double> node_position_new;
      if(offset >= node1_time && offset < node2_time) {
        if(new_poly) {
          first_node = tmp_source;
          first_dest = tmp_dest;
        }
        int node_val;
        bool make_new_node = true;
        if(node1_time == offset) {
          make_new_node = false;
          node_position_new = node1_position;
          node_val = tmp_source;
        } else if (node2_time == offset) {
          make_new_node = false;
          node_position_new = node2_position;
          node_val = tmp_dest;
        } else {
          node_position_new = interpolate_location(node1_position,node2_position,
                                                   node1_time,node2_time,offset);
          node_val = new_node_start;
        }

        NewNode new_node = {num_polygons, tmp_source, tmp_dest, node_val,
                            node_position_new[0], node_position_new[1],
                            offset, node1_position[0], node1_position[1],
                            node2_position[0],node2_position[1],
                            make_new_node};
        new_node_info.push_back(new_node);

        if(make_new_node) {
          new_node_start++;
        }
        new_poly = false;
      } else {
        if(new_poly) {
          first_node = tmp_source;
          first_dest = tmp_dest;
        }
      }

      // bool all_remaining_at_offset = true;
      // for(int jj = 0; jj < non_visited_links.size(); jj++) {
      //   if(((non_visited_links[jj].destination_time == offset && non_visited_links[jj].maxima_dest) ||
      //      (non_visited_links[jj].source_time == offset && non_visited_links[jj].maxima_source)) &&
      //      new_poly) {
      //     Rcpp::Rcout << "All remaining offset \n";
      //   } else {
      //     all_remaining_at_offset = false;
      //   }
      // }
      // if(all_remaining_at_offset) {
      //   break;
      // }

      bool is_edge = false;
      for(size_t jj = 0; jj < nodes.size(); jj++) {
        if(nodes[jj].id == tmp_dest) {
          is_edge = nodes[jj].edge;
        }
      }
      // Rcpp::Rcout << offset << " " << node1_time << " " << node2_time << " " <<
      //   first_node << " " << first_dest << " " << tmp_source << " " << tmp_dest << " " << remaining_links_count << " " <<
      //     is_edge << " " << bool((node1_time - offset) == 0) <<  " " << bool((node2_time - offset) == 0) << "\n";
      // Rcpp::Rcout << tmp_source << " " << tmp_dest << " " << offset << " " <<
      //   remaining_links_count << "\n";
      //
      if((offset < node1_time && offset >= node2_time) ||
          is_edge) {
        int new_source = tmp_dest;
        int new_dest = tmp_source;
        tmp_source = new_source;
        tmp_dest = new_dest;
        continue;
      }

      // Calculate the direction vector v1 for the current link
      std::vector<double> v1 = {node2_position[0] - node1_position[0], node2_position[1] - node1_position[1]};

      double best_angle = 180.0;
      int best_dest = -1;
      int best_source = -1;
      double best_len = std::numeric_limits<double>::infinity();
      any_picked = false;

      // Loop over all candidate next links
      for(const auto& link : links) {
        if((link.destination == tmp_dest && link.source == tmp_source) ||
           (link.source == tmp_dest && link.destination == tmp_source)) {
          continue;
        }

        if(link.source != tmp_dest && link.destination != tmp_dest) {
          continue;
        }

        if(link.edge) {
          continue;
        }
        if(node2_time == offset) {
          if(!(tmp_dest == link.source && link.destination_time >= offset) ||
             !(tmp_dest == link.destination && link.source_time >= offset)) {
            continue;
          }
        }
        // if(node2_time == offset) {
        //   if((tmp_dest == link.source && link.destination_time >= offset) ||
        //      (tmp_dest == link.destination && link.source_time >= offset)) {
        //     continue;
        //   }
        // }
        if(link.destination == tmp_dest && link.maxima_dest && link.destination_time == offset) {
          continue;
        }
        if(tmp_dest == link.source && link.maxima_source && link.source_time == offset) {
          continue;
        }
        any_picked = true;

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
      if(any_picked) {
        tmp_source = best_source;
        tmp_dest = best_dest;
      }
      bool all_remaining_at_offset = true;
      for(size_t jj = 0; jj < non_visited_links.size(); jj++) {
        if(((non_visited_links[jj].destination_time == offset && non_visited_links[jj].maxima_dest) ||
           (non_visited_links[jj].source_time == offset && non_visited_links[jj].maxima_source)) &&
           new_poly) {
          // Rcpp::Rcout << "All remaining offset \n";
        } else {
          all_remaining_at_offset = false;
        }
      }
      if(all_remaining_at_offset) {
        break;
      }
      // if(best_source < 0 || best_dest < 0) {
      //   throw std::runtime_error("Didn't find candidate");
      // }
    }
    List single_offset_nodes;
    for(size_t jj = 0; jj < new_node_info.size(); jj++) {
      NewNode& x = new_node_info[jj];
      NumericVector node_data = {(double)x.polygon_id,
                                 (double)x.source,
                                 (double)x.destination,
                                 (double)x.new_id,
                                 x.x,
                                 x.y,
                                 x.time,
                                 x.x1,
                                 x.y1,
                                 x.x2,
                                 x.y2,
                                 (double)x.new_node};
      single_offset_nodes.push_back(node_data);
    }
    return_data.push_back(single_offset_nodes);
  }
  return(return_data);
}
