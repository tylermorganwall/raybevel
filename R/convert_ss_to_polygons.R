#' Get Face IDs
#'
#' @param ss
#'
#' @return
#' @keywords internal
#'
#' @examples
convert_ss_to_polygons = function(ss) {
  links = ss$links
  nodes = ss$nodes

  links$visited = FALSE

  first_node = links[1,1]
  tmp_source = first_node

  first_dest = links[1,2]
  tmp_dest = first_dest

  first = TRUE
  polygon_indices = 1
  total_polygons = 1
  single_polygon_indices = list()
  list_all_polygons = list()
  #Just loop through every node (only marking as visited when the node is an origin)
  #And then extract the unique nodes at the end by copying the indices, ordering them,
  #hashing them, and then saving only the unique hashes (which should correspond to unique polygons)
  while((sum(!links$visited) > 0)) {
    # print(c(tmp_source, tmp_dest))
    # if(tmp_source == 10 && tmp_dest == 7) {
    # browser()
    # }
    # Debug
    # src = nodes[tmp_source,]
    # des = nodes[tmp_dest,]
    # segments(src[1,2], src[1,3], des[1,2], des[1,3], col = "green")
    #end debug
    if(tmp_source == first_node && tmp_dest == first_dest && !first) {
    # if(tmp_dest == first_node && tmp_dest == first_dest && !first) {
      links$visited[(links$source      == tmp_source &
                     links$destination == tmp_dest) |
                    (links$destination == tmp_source &
                     links$source      == tmp_dest)] = TRUE
      remaining_links = links[!links$visited,]
      tmp_source = remaining_links[1,1]
      tmp_dest   = remaining_links[1,2]
      first_node = tmp_source
      first_dest = tmp_dest
      list_all_polygons[[total_polygons]] = single_polygon_indices
      if(sum(!links$visited) == 0) {
        break
      }
      total_polygons = total_polygons + 1
      single_polygon_indices = list()
      polygon_indices = 1
    }
    first = FALSE
    node1_position = as.numeric(nodes[nodes$id == tmp_source,2:3])
    node2_position = as.numeric(nodes[nodes$id == tmp_dest,2:3])
    # links$visited[(links$source      == tmp_source &
    #                links$destination == tmp_dest) |
    #               (links$destination == tmp_source &
    #                links$source      == tmp_dest)] = TRUE
    v1 = node2_position-node1_position

    no_origin_links = links[!(links$source      == tmp_source &
                              links$destination == tmp_dest) &
                            !(links$destination == tmp_source &
                              links$source      == tmp_dest),]

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
    single_polygon_indices[[polygon_indices]] = best_dest
    polygon_indices = polygon_indices + 1
  }
  final_polygons = lapply(list_all_polygons, unlist, recursive=FALSE)
  ordered_polygon_indices = lapply(final_polygons,(function(x) x[order(x)]))
  is_duplicated = duplicated(unlist(lapply(ordered_polygon_indices,digest::digest)))
  final_list = list()
  stopifnot(length(is_duplicated) == length(final_polygons))
  counter = 1
  for(i in seq_len(length(is_duplicated))) {
    if(!is_duplicated[i]) {
      final_list[[counter]] = final_polygons[[i]]
      counter = counter + 1
    }
  }
  return(final_list)
}
