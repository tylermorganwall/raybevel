#' Generate Offset Polygon
#'
#' @param ss
#' @param offset
#'
#' @return
#' @export
#'
#' @examples
generate_offset_links_nodes = function(ss, offset) {
  links = ss$links
  nodes = ss$nodes
  max_time = max(links$destination_time)
  if(all(offset >= max_time)) {
    stop(sprintf("offset (%f) must not be greater than or equal to max time: %f", offset, max_time))
  }
  links$visited = links$edge
  # links$partial_edge = (links$source_time != 0 & links$destination_time == 0) |
  #   (links$destination_time != 0 & links$source_time == 0)
  links$away_from_offset = (links$source_time < offset & links$destination_time < offset) |
    (links$source_time > offset & links$destination_time > offset)
  # first_node = links[1,1]
  new_node_info = list()
  first_node = links[which(!links$edge)[1],1]
  tmp_source = first_node
  first_dest = links[which(!links$edge)[1],2]
  # first_dest = links[1,2]
  # x11()
  # opar = par(no.readonly = TRUE)
  # on.exit(par(opar))
  # par(mfrow=c(1,1), mar=c(1,1,1,1), oma=c(0,0,0,0))
  # plot_skeleton(ss)
  tmp_dest = first_dest

  offset_polygon_verts = list()
  counter = 1
  num_polygons = 1
  new_node_start = max(nodes$id) + 1
  # cols = c("purple","dodgerblue","darkgreen")
  first = TRUE
  new_poly = TRUE
  # last_pair = c(-1,-1)
  while(first || !(tmp_source == first_node && tmp_dest == first_dest) ||
        sum(!links$visited) != 0) {

    if(tmp_source == first_node && tmp_dest == first_dest && !first) {
      new_poly = TRUE
      remaining_links = links[!links$visited &
                              !links$edge &
                              !links$away_from_offset,]
      if(nrow(remaining_links) == 0) {
        break
      }
      tmp_source = remaining_links[1,1]
      tmp_dest   = remaining_links[1,2]
      first_node = tmp_source
      first_dest = tmp_dest
      num_polygons = num_polygons + 1
    }
    # print(c(tmp_source,tmp_dest, sum(!links$visited &
    #                                    !links$edge &
    #                                    !links$away_from_offset)))

    first = FALSE
    node1_position = as.numeric(nodes[nodes$id == tmp_source,2:3])
    node2_position = as.numeric(nodes[nodes$id == tmp_dest,2:3])
    node1_time = nodes[nodes$id == tmp_source,4]
    node2_time = nodes[nodes$id == tmp_dest,4]
    link_idx = which((links$source == tmp_source &
                      links$destination == tmp_dest) |
                     (links$destination == tmp_source &
                      links$source == tmp_dest))
    # segments(node1_position[1],node1_position[2],node2_position[1],node2_position[2],
    #          col=cols[num_polygons %% 3 + 1], lwd=3)
    # Sys.sleep(0.1)

    stopifnot(length(link_idx) == 1)
    # if(links$visited[link_idx]) {
    #   already_visited = TRUE
    # } else {
    #   already_visited = FALSE
    # }
    links$visited[link_idx] = TRUE
    if(offset >= node1_time && offset <= node2_time) {
      if(new_poly) {
        first_node = tmp_source
        first_dest = tmp_dest
      }
      if(node1_time == offset) {
        node_position_new = node1_position
      } else if (node2_time == offset) {
        node_position_new = node2_position
      } else {
        node_position_new = interpolate_location(node1_position,node2_position,
                                                 node1_time,node2_time,offset)
      }
      offset_polygon_verts[[counter]] = c(num_polygons,node_position_new)
      new_node_info[[counter]] = matrix(c(num_polygons, tmp_source, tmp_dest, new_node_start,
                                          node_position_new, offset, node1_position, node2_position), nrow=1,ncol=11)

      new_node_start = new_node_start + 1
      colnames(new_node_info[[counter]]) = c("polygon_id", "source", "destination", "new_id",
                                             "x","y","time","x1","y1","x2","y2")
      new_poly = FALSE
      counter = counter + 1
    } else {
      if(new_poly) {
        first_node = tmp_source
        first_dest = tmp_dest
      }
    }
    if((offset < node1_time && offset > node2_time) ||
       nodes$edge[nodes$id == tmp_dest]){
      # last_pair = c(tmp_source, tmp_dest)
      new_source = tmp_dest
      new_dest = tmp_source
      tmp_source = new_source
      tmp_dest = new_dest
      next
    }
    v1 = node2_position-node1_position
    no_origin_links = links[!(links$source == tmp_source &
                              links$destination == tmp_dest) &
                            !(links$destination == tmp_source &
                              links$source == tmp_dest),]
    next_links = no_origin_links[(no_origin_links$source == tmp_dest |
                                  no_origin_links$destination == tmp_dest),]
    best_angle = 180
    best_dest = NA
    best_source = NA

    for(i in seq_len(nrow(next_links))) {
      if(next_links$destination[i] == tmp_dest) {
        candidate_source = which(next_links$destination[i] == nodes$id)
        candidate_dest = which(next_links$source[i] == nodes$id)
      } else {
        candidate_source = which(next_links$source[i] == nodes$id)
        candidate_dest = which(next_links$destination[i] == nodes$id)
      }
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
    # last_pair = c(tmp_source, tmp_dest)

    tmp_source = best_source
    tmp_dest = best_dest

  }
  all_new_nodes = do.call("rbind",new_node_info)
  new_links_poly = split(as.data.frame(all_new_nodes[,-1]), all_new_nodes[,1])
  #Need to ensure new_links_poly has doesn't have nodes that start and end on the offset value?
  return(insert_polygon_links_nodes(ss, new_links_poly))
}


#' Insert Polygon Links/Nodes
#'
#' @param ss
#' @param offset
#'
#' @return
#' @keywords internal
insert_polygon_links_nodes = function(ss, new_links) {
  links = ss$links
  nodes = ss$nodes
  new_nodes = do.call("rbind", new_links)[,3:6]
  colnames(new_nodes)[1] = "id"
  new_nodes$edge = FALSE
  new_nodes = new_nodes[order(new_nodes$id),]
  links_to_remove = list()
  new_link_list = list()
  counter = 1
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
      # if(length(link_remove_idx) != 1) {
      #   browser()
      # }
      # plot_skeleton(ss)
      stopifnot(length(link_remove_idx) == 1)
      old_link = links[link_remove_idx,]
      old_source_time = old_link$source_time
      old_destination_time = old_link$destination_time

      #Possibly because just going clockwise  misses exact hits like 23 to 14

      #The issue is you need to keep rotating through until you reach the original node--you can re-hit
      #contour lines when they lie on the edge
      links_to_remove[[counter]] = link_remove_idx
      new_link_list[[counter]] = data.frame(source = c(old_source,new_id, new_id),
                                        destination = c(new_id,old_destination, poly_id),
                                        source_time = c(old_source_time, new_time, new_time),
                                        destination_time = c(new_time, old_destination_time, new_time),
                                        edge = FALSE)
      counter = counter + 1
    }
  }
  # browser()
  links = rbind(links[-unlist(links_to_remove),],
                do.call("rbind",new_link_list))
  nodes = rbind(nodes,new_nodes)
  rownames(nodes) = NULL
  new_ss = list(nodes = nodes, links = links)
  class(new_ss) = "rayskeleton"
  attr(new_ss, "original_vertices") = attr(ss, "original_vertices")
  attr(new_ss, "original_holes") = attr(ss, "original_holes")
  new_ss = remove_node_duplicates(new_ss)

  return(new_ss)
}
