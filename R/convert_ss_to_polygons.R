#' Get Face IDs
#'
#' @param ss
#'
#' @keywords internal
convert_ss_to_polygons = function(ss, progress = FALSE) {
  links = ss$links
  nodes = ss$nodes

  links$visited = FALSE

  first_node = links[1,1]
  tmp_source = first_node

  first_dest = links[1,2]
  tmp_dest = first_dest
  # x11()
  # plot_skeleton(ss)
  first = TRUE
  polygon_indices = 1
  total_polygons = 1
  single_polygon_indices = list()
  list_all_polygons = list()
  pb = progress::progress_bar$new(
    format = ":current/:total Generating polygons [:bar] eta: :eta",
    total = sum(nrow(links)), clear = TRUE, width = 60)
  #Just loop through every node (only marking as visited when the node is an origin)
  #And then extract the unique nodes at the end by copying the indices, ordering them,
  #hashing them, and then saving only the unique hashes (which should correspond to unique polygons)
  while((sum(!links$visited) > 0)) {
    # plot_skeleton(ss,xlim=c(0.79,0.80)+0.001,ylim=c(0.2,0.25)-0.05)
    # plot_skeleton(ss)
    # segments(nodes[tmp_source,2],nodes[tmp_source,3],
    #          nodes[tmp_dest,2],nodes[tmp_dest,3],
    #          col="yellow", lwd=10)
    # points(nodes[first_node,2],nodes[first_node,3],
    #          col="red", pch=19,cex=2)
    # points(nodes[first_dest,2],nodes[first_dest,3],
    #        col="pink",pch=19,cex=2)
    # for(j in rev(seq_len(length(list_all_polygons)))[1:5]) {
    #   polygon(ss$nodes[unlist(list_all_polygons[[j]]),c("x","y")], col = "#00000088")
    # }
    # Sys.sleep(0.1)
    if(tmp_source == first_node && tmp_dest == first_dest && !first) {
      links$visited[(links$source      == tmp_source &
                     links$destination == tmp_dest) |
                    (links$destination == tmp_source &
                     links$source      == tmp_dest)] = TRUE
      remaining_links = links[!links$visited,]
      if(progress) {
        n_ticks = sum((links$source      == tmp_source &
              links$destination == tmp_dest) |
            (links$destination == tmp_source &
               links$source      == tmp_dest))
        pb$tick(n_ticks)
      }
      tmp_source = remaining_links[1,1]
      tmp_dest   = remaining_links[1,2]
      first_node = tmp_source
      first_dest = tmp_dest
      list_all_polygons[[total_polygons]] = single_polygon_indices
      if(sum(!links$visited) == 0) {
        break
      }
      # print("Wrote new polygon!")
      total_polygons = total_polygons + 1
      single_polygon_indices = list()
      polygon_indices = 1
    }

    first = FALSE

    #Get current node direction vector
    node1_position = as.numeric(nodes[nodes$id == tmp_source,2:3])
    node2_position = as.numeric(nodes[nodes$id == tmp_dest,2:3])
    v1 = node2_position-node1_position

    #Remove potential links that are the current tmp_source and dest
    no_origin_links = links[!(links$source      == tmp_source &
                              links$destination == tmp_dest) &
                            !(links$destination == tmp_source &
                              links$source      == tmp_dest),]


    next_links = no_origin_links[(no_origin_links$source == tmp_dest |
                                  no_origin_links$destination == tmp_dest),]

    best_angle = 180
    best_dest = NA
    best_source = NA
    best_len = Inf
    for(i in seq_len(nrow(next_links))) {
      if(next_links$destination[i] == tmp_dest) {
        candidate_source = which(next_links$destination[i] == nodes$id)
        candidate_dest = which(next_links$source[i] == nodes$id)
      } else {
        candidate_source = which(next_links$source[i] == nodes$id)
        candidate_dest = which(next_links$destination[i] == nodes$id)
      }
      v2 = as.numeric(nodes[candidate_dest,2:3] - nodes[candidate_source,2:3])
      # segments(nodes[candidate_source,2],nodes[candidate_source,3],
      #          nodes[candidate_dest,2],nodes[candidate_dest,3],
      #          col="purple", lwd=10)

      det_val = determinant2x2(v1,v2)
      dot_val = dot(v1,v2)
      length_v2 = dot(v2,v2)
      angle = atan2(det_val, dot_val)*180/pi
      if((angle < best_angle) || (angle == best_angle && best_len > length_v2)) {
        best_angle = angle
        best_dest = nodes$id[candidate_dest]
        best_source = nodes$id[candidate_source]
        best_len = length_v2
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
