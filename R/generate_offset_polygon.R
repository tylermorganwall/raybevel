
#' Generate Offset Polygon
#'
#' @param ss
#' @param offset
#'
#' @return
#' @export
#'
#' @examples
generate_offset_polygon = function(ss, offset) {
  links = ss$links
  nodes = ss$nodes
  max_time = max(links$destination_time)
  if(offset >= max_time) {
    stop(sprintf("offset (%f) must not be greater than or equal to max time: %f", offset, max_time))
  }
  links$visited = links$edge
  links$away_from_offset = (links$source_time < offset & links$destination_time < offset) |
    (links$source_time > offset & links$destination_time > offset)
  first_node = links[which(!links$edge)[1],1]
  tmp_source = first_node
  first_dest = links[which(!links$edge)[1],2]
  tmp_dest = first_dest


  offset_polygon_verts = list()
  counter = 1
  num_polygons = 1
  first = TRUE
  new_poly = TRUE
  while(first || !(tmp_source == first_node && tmp_dest == first_dest) || sum(!links$visited) != 0) {
    # print(c(tmp_source,tmp_dest))
    if(tmp_source == first_node && tmp_dest == first_dest && !first) {
      new_poly = TRUE
      remaining_links = links[!links$visited & !links$edge & !links$away_from_offset,]
      if(nrow(remaining_links) == 0) {
        break
      }
      tmp_source = remaining_links[1,1]
      tmp_dest   = remaining_links[1,2]
      first_node = tmp_source
      first_dest = tmp_dest
      num_polygons = num_polygons + 1
    }
    first = FALSE
    node1_position = as.numeric(nodes[nodes$id == tmp_source,2:3])
    node2_position = as.numeric(nodes[nodes$id == tmp_dest,2:3])
    node1_time = nodes[nodes$id == tmp_source,4]
    node2_time = nodes[nodes$id == tmp_dest,4]
    links$visited[(links$source == tmp_source &
                   links$destination == tmp_dest) |
                  (links$destination == tmp_source &
                   links$source == tmp_dest)] = TRUE
    if(offset >= node1_time && offset <= node2_time) {
      if(new_poly) {
        first_node = tmp_source
        first_dest = tmp_dest
      }
      offset_polygon_verts[[counter]] = c(num_polygons,interpolate_location(node1_position,node2_position,
                                                                            node1_time,node2_time,offset))
      new_poly = FALSE
      counter = counter + 1
    } else {
      if(new_poly) {
        first_node = tmp_source
        first_dest = tmp_dest
      }
    }
    if((offset <= node1_time && offset >= node2_time) ||
       nodes$edge[nodes$id == tmp_dest]){
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
    tmp_source = best_source
    tmp_dest = best_dest
  }
  final_poly = as.data.frame(do.call("rbind",offset_polygon_verts))
  colnames(final_poly) = c("id","x","y")
  return(split(final_poly,final_poly$id))
}
