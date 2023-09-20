#' Generate Offset Polygon
#'
#' @keywords internal
generate_offset_links_nodes = function(ss, offsets, return_polys = FALSE, progress = FALSE) {
  links = ss$links
  # links$hashes = apply(matrix(c(links$source,links$destination),ncol=2),1,digest::digest)
  nodes = ss$nodes
  # browser()

  node_maxima_ids = identify_maxima_nodes(ss)
  nodes$local_maxima = nodes$id %in% node_maxima_ids
  links$local_maxima_destination = links$destination %in% node_maxima_ids
  links$local_maxima_source = links$source %in% node_maxima_ids

  max_time = max(links$destination_time)
  if(all(offsets >= max_time)) {
    stop(sprintf("offset (%f) must not be greater than or equal to max time: %f", offset, max_time))
  }
  new_links_offset = list()
  offsets = offsets[order(offsets)]
  new_node_start = max(nodes$id) + 1
  pb = progress::progress_bar$new(
        format = ":current/:total generating internal links [:bar] eta: :eta",
        total = length(offsets), clear = TRUE, width = 60)
  for(ii in seq_len(length(offsets))) {
    if(progress) {
      pb$tick()
    }
    offset = offsets[ii]
    links$visited = links$edge

    links$away_from_offset = (links$source_time < offset & links$destination_time < offset) |
                             (links$source_time > offset & links$destination_time > offset)
    links$equal_to_offset = links$source_time == offset
    new_node_info = list()
    # first_node = links[which(!links$edge)[1],1]
    # tmp_source = first_node
    # first_dest = links[which(!links$edge)[1],2]
    # tmp_dest = first_dest

    first_node = links[which(links$local_maxima_destination & links$destination_time > offset)[1],1]
    tmp_source = first_node
    first_dest = links[which(links$local_maxima_destination & links$destination_time > offset)[1],2]
    tmp_dest = first_dest
    num_polygons = 1
    counter = 1
    first = TRUE
    new_poly = TRUE
    #This list contains any sub-polygons that the loop encounters
    sub_polygons = list()
    sub_polygon_counter = 1

    #This hashes new nodes to detect sub-polygons
    hashed_visited_nodes_single_poly = list()
    hash_ctr = 1
    links$split_poly = FALSE
    any_picked = FALSE

    original_first_source = first_node
    original_first_dest = first_dest
    changed_first = FALSE
    while(first || !(tmp_source == first_node && tmp_dest == first_dest) ||
          sum(!links$visited) != 0) {
      # if(is.na(tmp_source) || is.na(tmp_dest)) {
      #   browser()
      # }
      # if(ii == 74) {
      #   browser()
      # }
      # print(c(first_node, first_dest, tmp_source, tmp_dest, sum(!links$visited),
      #         offset, nodes[tmp_source,"time"], nodes[tmp_dest,"time"], ii, num_polygons, sub_polygon_counter ))
      if(!first || any_picked) {
        if(is.na(tmp_source) || is.na(tmp_dest)) {
          break
          browser()
        }
        if(tmp_source == first_node && tmp_dest == first_dest) {
          # print("new poly")
          new_poly = TRUE
          remaining_links = links[!links$visited &
                                  !links$edge &
                                  !links$away_from_offset,]
                                  # !links$split_poly, ]
          if(nrow(remaining_links) == 0) {
            break
          }
          tmp_source = remaining_links[1,1]
          tmp_dest   = remaining_links[1,2]
          first_node = tmp_source
          first_dest = tmp_dest
          num_polygons = max(num_polygons) + 1
          hash_ctr = 1
          hashed_visited_nodes_single_poly = list()
        }
      }

      # Here is where we detect if we've encountered a new polygon when visiting these nodes (which
      # will appear as a loop). If so, we'll split off that polygon separately and mark those nodes as visited,
      # and increment num_polygons.
      # hash_link = digest::digest(c(tmp_source, tmp_dest))
      # if(hash_link %in% hashed_visited_nodes_single_poly) {
      #   #Extract nodes that correspond to loop: should equal the list of nodes backtracking from this one
      #   sub_polygon_indices = seq(which(hash_link == hashed_visited_nodes_single_poly),length(hashed_visited_nodes_single_poly))
      #   sub_polygons[[sub_polygon_counter]] =  do.call("rbind", new_node_info[sub_polygon_indices])
      #
      #   new_node_info = new_node_info[-sub_polygon_indices]
      #   sub_poly_hashes = hashed_visited_nodes_single_poly[sub_polygon_indices]
      #
      #   hashed_visited_nodes_single_poly = hashed_visited_nodes_single_poly[-sub_polygon_indices]
      #   hash_ctr = length(hashed_visited_nodes_single_poly)
      #   links$split_poly = links$split_poly | links$hash %in% sub_poly_hashes
      #   # Get next link
      #   tmp_source = new_node_info[[length(new_node_info)]][1,2]
      #   tmp_dest = new_node_info[[length(new_node_info)]][1,3]
      #   # remaining_links = links[!links$visited &
      #   #                         !links$edge &
      #   #                         !links$away_from_offset &
      #   #                         !links$split_poly,]
      #   # if(nrow(remaining_links) == 0) {
      #   #   break
      #   # }
      #   sub_polygon_counter = sub_polygon_counter + 1
      #
      # }
      # browser()
      # browser()
      # equal_time_maxima_node = (links$local_maxima_destination & links$destination_time == offset) |
      #   (links$local_maxima_source & links$source_time == offset)
      first = FALSE
      node1_position = as.numeric(nodes[nodes$id == tmp_source,2:3])
      node2_position = as.numeric(nodes[nodes$id == tmp_dest,2:3])
      node1_time = nodes[nodes$id == tmp_source,4]
      node2_time = nodes[nodes$id == tmp_dest,4]
      link_idx = which((links$source == tmp_source &
                        links$destination == tmp_dest) |
                       (links$destination == tmp_source &
                        links$source == tmp_dest))
      # if(ii == 74) {
      #   # browser()
      #   plot_skeleton(ss)
      #   # cols = c("red","green","blue")
      #   polynodes = do.call("rbind",new_node_info)
      #   polygon(polynodes[polynodes[,1] == num_polygons,5:6], col="purple")
      #
      #   segments(node1_position[1],node1_position[2],node2_position[1],node2_position[2],
      #            col="green", lwd=3)
      #   Sys.sleep(0.1)
      # }

      #Need to see if all nodes heading away from single node are lower when equal--then no polygons left and mark as such
      #Otherwise, that node can be included in another polygon.
      # if(node1_time == offset) {
      #   links_from_source = which(links$source == tmp_source)
      #   links_to_source = which(links$destination == tmp_source)
      #   destination_times = links[links_from_source,"destination_time"]
      #   source_times = links[links_to_source,"source_time"]
      #   if(all(c(destination_times,source_times) < offset)) {
      #     links$visited[c(links_from_source,links_to_source)] = TRUE
      #   }
      # }
      stopifnot(length(link_idx) == 1)
      links$visited[link_idx] = TRUE
      if(offset >= node1_time && offset < node2_time) { ### Sep 19th, changed from <= to < to stop looping when loop ends on node exactly equal to offset
        if(new_poly) {
          first_node = tmp_source
          first_dest = tmp_dest
        }
        make_new_node = TRUE
        if(node1_time == offset) {
          make_new_node = FALSE
          node_position_new = node1_position
          node_val = tmp_source
        } else if (node2_time == offset) {
          make_new_node = FALSE
          node_position_new = node2_position
          node_val = tmp_dest
        } else {
          node_position_new = interpolate_location(node1_position,node2_position,
                                                   node1_time,node2_time,offset)
          node_val = new_node_start
        }
        new_node_info[[counter]] = matrix(c(num_polygons, tmp_source, tmp_dest, node_val,
                                            node_position_new, offset, node1_position, node2_position,
                                            hash_ctr, make_new_node),
                                          nrow=1,ncol=13)
        hashed_visited_nodes_single_poly[[hash_ctr]] = digest::digest(c(tmp_source, tmp_dest))
        hash_ctr = hash_ctr + 1

        if(make_new_node) {
          new_node_start = new_node_start + 1
        }

        colnames(new_node_info[[counter]]) = c("polygon_id", "source", "destination", "new_id",
                                               "x","y","time","x1","y1","x2","y2", "hash_ctr", "new_node")
        new_poly = FALSE
        counter = counter + 1
      } else {
        if(new_poly) {
          first_node = tmp_source
          first_dest = tmp_dest
        }
      }

      if((offset < node1_time && offset >= node2_time) ||
         nodes$edge[nodes$id == tmp_dest]){
        new_source = tmp_dest
        new_dest = tmp_source
        tmp_source = new_source
        tmp_dest = new_dest
        next
      }
      v1 = node2_position-node1_position

      # Remove all invalid links (links equal to the current link)
      # browser()
      no_origin_bool = with(links,
           !(source == tmp_source &
             destination == tmp_dest) &
           !(destination == tmp_source &
             source == tmp_dest)
      )
      no_origin_links = links[no_origin_bool,]
      connected_bool = with(no_origin_links,
                            destination == tmp_dest | source == tmp_dest)
      only_connected = no_origin_links[connected_bool,]
      no_edges = only_connected[!only_connected$edge,]
      # if(ii == 3.0 && num_polygons == 2) {
      #   browser()
      # }
      if(node2_time == offset) {
        bool_remove_lower_when_equal = with(no_edges,
            (tmp_dest == source & destination_time >= offset) |
            (tmp_dest == destination & source_time >= offset))

        next_links = no_edges[bool_remove_lower_when_equal,]
      } else {
        next_links = no_edges
      }
      # If node at the offset, don't go downhill
      # if(ii == 3.0 && num_polygons == 2) {
      #   browser()
      # }

      # with(no_edges,
      #      time == offset)
      next_links = no_edges

      # connected_current_node = with(no_origin_links)
                          # (links$destination[i] == tmp_dest | links$source[i] == tmp_dest |
                          #  links$destination[i] == tmp_source | links$source[i] == tmp_source) &
                         # !links$edge,]
      best_angle = 180
      best_dest = NA
      best_source = NA
      for(i in seq_len(nrow(next_links))) {
        # node1_position_link = as.numeric(nodes[nodes$id == next_links$source[i],2:3])
        # node2_position_link = as.numeric(nodes[nodes$id == next_links$destination[i],2:3])
        # arrows(node1_position_link[1],node1_position_link[2],node2_position_link[1],node2_position_link[2],
        #       col='orange', lwd=3, length=0.1)
        # Sys.sleep(0.1)
        # browser()

        if(next_links$destination[i] == tmp_dest) {
          candidate_source = which(next_links$destination[i] == nodes$id &
                                  !(next_links$local_maxima_destination[i] & next_links$destination_time[i] == offset))
          candidate_dest = which(next_links$source[i] == nodes$id &
                                !(next_links$local_maxima_source[i] & next_links$source_time[i] == offset))
        } else {
          candidate_source = which(next_links$source[i] == nodes$id &
                                  !(next_links$local_maxima_source[i] & next_links$source_time[i] == offset))
          candidate_dest = which(next_links$destination[i] == nodes$id &
                                !(next_links$local_maxima_destination[i] & next_links$destination_time[i] == offset))
        }
        if(length(candidate_source) == 0 || length(candidate_dest) == 0) {
          next
        }
        any_picked = TRUE
        v2 = as.numeric(nodes[candidate_dest,2:3] - nodes[candidate_source,2:3])

        det_val = determinant2x2(v1,v2)
        dot_val = dot(v1,v2)
        angle = atan2(det_val, dot_val)*180/pi
        if(angle < best_angle) {
          best_angle = angle
          best_dest = nodes$id[candidate_dest]
          best_source = nodes$id[candidate_source]
        }
      }
      if(any_picked) {
        tmp_source = best_source
        tmp_dest = best_dest
      } else {}
    }
    all_new_nodes = do.call("rbind",new_node_info)

    if(return_polys) {
      just_poly = all_new_nodes[,c("polygon_id", "x","y")]
      names(just_poly) = c("id","x","y")
      just_poly = split(as.data.frame(just_poly[,-1]), just_poly[,1])
      return(just_poly)
    }
    new_links_poly = split(as.data.frame(all_new_nodes[,-1,drop=FALSE]), all_new_nodes[,1,drop=FALSE])

    new_links_offset[[ii]] = new_links_poly
  }
  return(insert_polygon_links_nodes(ss, new_links_offset))

}


#' Insert Polygon Links/Nodes
#'
#' @keywords internal
insert_polygon_links_nodes = function(ss, new_links_all) {
  links = ss$links
  nodes = ss$nodes
  counter = 1
  new_link_list = list()
  new_link_list_poly = list()
  new_node_list = list()
  links_to_remove = list()
  for(ii in seq_len(length(new_links_all))) {
    new_links = new_links_all[[ii]]
    if(length(new_links) == 0) {
      next
    }
    new_nodes_single = do.call("rbind", new_links)[,c(3:6)]
    # new_nodes_single = do.call("rbind", new_links)[,c(3:6,12)]
    # new_nodes_single = new_nodes_single[new_nodes_single$new_node == 1,1:4, drop = FALSE]
    colnames(new_nodes_single)[1] = "id"
    new_nodes_single$edge = FALSE
    new_node_list[[ii]] = new_nodes_single[order(new_nodes_single$id),]
    for(i in seq_len(length(new_links))) {
      single_poly = new_links[[i]]
      for(j in seq_len(nrow(single_poly))) {
        new_id = single_poly$new_id[j]
        new_time = single_poly$time[j]
        old_source =  single_poly$source[j]
        old_destination =  single_poly$destination[j]
        if(j != nrow(single_poly)) {
          poly_id = single_poly$new_id[j + 1]
        } else {
          poly_id = single_poly$new_id[1]
        }
        link_remove_idx = which((links$source == old_source &
                                 links$destination == old_destination) |
                                (links$destination == old_source &
                                 links$source == old_destination))
        # plot_skeleton(ss)
        stopifnot(length(link_remove_idx) == 1)
        old_link = links[link_remove_idx,]
        old_source_time = old_link$source_time
        old_destination_time = old_link$destination_time

        links_to_remove[[counter]] = link_remove_idx
        new_link_list[[counter]] = data.frame(source = c(old_source,new_id),
                                          destination = c(new_id,old_destination),
                                          source_time = c(old_source_time, new_time),
                                          destination_time = c(new_time, old_destination_time),
                                          edge = FALSE,
                                          link_remove = link_remove_idx,
                                          old_source_id = old_source,
                                          old_dest_id = old_destination) #We will remove this variable before joining
        new_link_list_poly[[counter]] = data.frame(source = new_id,
                                                   destination = poly_id,
                                                   source_time = new_time,
                                                   destination_time = new_time,
                                                   edge = FALSE)
        counter = counter + 1
      }
    }
  }
  new_nodes = do.call("rbind", new_node_list)
  new_skeleton_links_raw = do.call("rbind",new_link_list)
  links_to_slice = split(new_skeleton_links_raw,new_skeleton_links_raw[,6])
  new_links = lapply(links_to_slice, process_sliced_links)
  new_skeleton_links = do.call("rbind",new_links)
  new_poly_links = do.call("rbind", new_link_list_poly)
  links = rbind(links[-unlist(links_to_remove),],
                new_skeleton_links,
                new_poly_links)
  nodes_updated = rbind(nodes,new_nodes)
  rownames(nodes_updated) = NULL
  new_ss = list(nodes = nodes_updated, links = links)
  class(new_ss) = "rayskeleton"
  attr(new_ss, "original_vertices") = attr(ss, "original_vertices")
  attr(new_ss, "original_holes") = attr(ss, "original_holes")
  # new_ss_inc = make_incremental_nodes(new_ss)
  new_ss_no_dupe = remove_node_duplicates(new_ss)
  return(new_ss_no_dupe)
}

#' Process sliced links
#'
#' @keywords internal
process_sliced_links = function(link_group) {
  n_new_nodes = nrow(link_group)/2
  new_link_info = link_group[seq(2,nrow(link_group),by=2),]
  old_source_time = link_group$source_time[1]
  old_dest_time = link_group$destination_time[2]
  if(old_source_time < old_dest_time) {
    start_node = link_group$old_source_id[1]
    end_node = link_group$old_dest_id[1]
    prev_time = old_source_time
    end_time = old_dest_time
  } else {
    end_node = link_group$old_source_id[1]
    start_node = link_group$old_dest_id[1]
    prev_time = old_dest_time
    end_time = old_source_time

  }
  new_links = list()
  current_node = start_node
  for(k in seq_len(n_new_nodes)) {
    new_node = new_link_info$source[k]
    new_time = new_link_info$source_time[k]
    if(new_time < prev_time) {
      stop(sprintf("New time (%f) is not greater than previous time (%f) ",new_time,prev_time))
    }
    if(new_time != prev_time) {
      new_links[[k]] = data.frame(source = current_node,
                                  destination = new_node,
                                  source_time = prev_time,
                                  destination_time = new_time,
                                  edge = FALSE)
    }
    current_node = new_node
    prev_time = new_time
  }
  new_links[[k+1]] = data.frame(source = current_node,
                                destination = end_node,
                                source_time = prev_time,
                                destination_time = end_time,
                                edge = FALSE)
  return(do.call("rbind",new_links))
}

#' Make incremental nodes
#'
#' @keywords internal
make_incremental_nodes = function(ss) {
  fac_id = factor(ss$nodes$id)
  levels_id = levels(factor(ss$nodes$id))
  ss$nodes$id = as.integer(fac_id)
  ss$links$source = as.integer(factor(ss$links$source, levels = levels_id))
  ss$links$destination = as.integer(factor(ss$links$destination, levels = levels_id))
  return(ss)
}
