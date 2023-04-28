#' Title
#'
#' @param vertices DUM
#' @param holes DEE
#' @param tol DOO
#' @param debug
#'
#' @return Value
#' @export
#'
#' @examples
#' #Value
skeletonize = function(vertices, holes = list(), tol = 1e-10, debug = FALSE) {
  stopifnot(all(vertices[1,] == vertices[nrow(vertices),]))
  stopifnot(ncol(vertices) == 2)
  vertices_pad = rbind(vertices,vertices[2,])
  remove_verts = list()

  total_sum_det = 0
  #Remove vertices that come in straight lines
  for(i in seq_len(nrow(vertices_pad)-1)[-1]) {
    v1 = vertices_pad[i,]-vertices_pad[i-1,]
    v2 = vertices_pad[i+1,]-vertices_pad[i,]
    tmp_det = determinant2x2(v1,v2)
    if(abs(tmp_det) == 0) {
      remove_verts[[i]] = i
    }
  }
  for(i in seq_len(nrow(vertices_pad)-1)[-1]) {
    v1 = vertices_pad[i,]
    v2 = vertices_pad[i+1,]
    tmp_det = determinant2x2(v1,v2)
    total_sum_det = total_sum_det + tmp_det
  }
  if(total_sum_det < 0) {
    stop("`vertices` is not CCW polygon")
  }
  if(length(holes) > 0) {
    for(i in seq_len(length(holes))) {
      holes_pad = rbind(holes[[i]][nrow(holes[[i]])-1,], holes[[i]])

      total_sum_det_holes = 0
      for(i in seq_len(nrow(vertices_pad)-1)[-1]) {
        v1 = holes_pad[i,]
        v2 = holes_pad[i+1,]
        tmp_det = determinant2x2(v1,v2)
        total_sum_det_holes = total_sum_det_holes + tmp_det
      }
      if(total_sum_det_holes > 0) {
        stop(sprintf("`holes[[%i]]` is not CW polygon",i))
      }
    }
  }
  verts_to_remove = c(1,unlist(remove_verts))
  vertices = vertices_pad[-verts_to_remove,]
  return_data = skeletonize_rcpp(vertices, holes, tol)
  if(length(return_data) == 0) {
    warning("Polygon is not simple--not computing straight skeleton.")
    return()
  }
  ss = do.call("rbind", return_data$bisectors)
  colnames(ss) = c("end_x","end_y","start_x","start_y","time","time_start", "id", "id_start")
  #De-duplicate half edges
  ss = ss[ss[,"id_start"] < ss[,"id"],]
  #Re-connect first vertex so everything is CCW
  stopifnot(ss[1:2,"id_start"] == 0)
  ss[1,] = ss[1,c(3,4,1,2,5,6,8,7)]
  ss = unique(as.data.frame(ss))
  ss$edge = ss$time == 0 & ss$time_start == 0
  start_nodes = as.data.frame(ss[,c("id_start", "start_x", "start_y", "time_start")])
  start_nodes = dplyr::arrange(unique(start_nodes[start_nodes$time_start == 0,]),id_start)
  ss = dplyr::arrange(as.data.frame(ss), time_start, time)
  start_ss = ss[,c(8,3:4,6,5)]
  colnames(start_ss) = c("id","x","y","time","other_time")
  end_ss = ss[,c(7,1:2,5,6)]
  colnames(end_ss) = c("id","x","y","time","other_time")
  nodes = rbind(start_ss,end_ss)
  nodes$edge = nodes$time == nodes$other_time
  nodes = (nodes[nodes$time >= nodes$other_time,])
  nodes = unique(nodes[,c(1:4,6)])
  nodes = dplyr::arrange(nodes, id)
  ss2 = ss[,c("id_start", "id", "time","time_start")]
  colnames(ss2) = c("source","destination" ,"source_time","destination_time")
  ss2 = as.data.frame(ss2)
  ss2$edge = ss2$source_time == 0 & ss2$destination_time == 0
  ss2 = ss2[,c(1,2,5,3,4)]
  return(list(nodes = nodes, links = ss2))
}

#' Title
#'
#' @param node_start
#' @param node_end
#' @param height_start
#' @param height_end
#' @param height
#'
#' @return
#' @export
#'
#' @examples
interpolate_location = function(node_start,
                                node_end,
                                height_start,
                                height_end,
                                height) {
  t = (height - height_start)/(height_end - height_start)
  return(node_start * (1 - t) + node_end * t)
}

#' Title
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
  if(offset > max_time) {
    offset == max_time
  }
  links$visited = links$edge
  first_node = links[which(!links$edge)[1],1]
  tmp_source = first_node
  first_dest = links[which(!links$edge)[1],2]
  tmp_dest = first_dest

  first_vertex = c(Inf, Inf)

  offset_polygon_verts = list()
  counter = 1
  num_polygons = 1
  first = TRUE
  new_poly = TRUE
  while(first || !(tmp_source == first_node && tmp_dest == first_dest) || sum(!links$visited) != 0) {
    # print(c(tmp_source,tmp_dest))
    if(tmp_source == first_node && tmp_dest == first_dest && !first) {
      new_poly = TRUE
      remaining_links = links[!links$visited,]
      tmp_source = remaining_links[1,1]
      tmp_dest   = remaining_links[1,2]
      first_node = tmp_source
      first_dest = tmp_dest
      num_polygons = num_polygons + 1
    }
    first = FALSE
    node1_position = as.numeric(nodes[nodes$id == tmp_source,2:3])
    node2_position = as.numeric(nodes[nodes$id == tmp_dest,2:3])
    # segments(node1_position[1],node1_position[2],node2_position[1],node2_position[2], col="green")
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
      # if(length(offset_polygon_verts) > 1 && !new_poly) {
      #   print("printing")
      #   val1 = offset_polygon_verts[[counter-1]]
      #   val2 = offset_polygon_verts[[counter]]
      #   segments(val1[2],val1[3],val2[2],val2[3], col="red")
      # }
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

#' Title
#'
#' @param v1 Vector 1
#' @param v2 Vector 2
#'
#' @return Value
#' @keywords internal
#'
#' @examples
#' #None
determinant2x2 = function(v1,v2) {
  det(matrix(c(v1,v2), ncol=2,nrow=2))
}

#' Dot Product
#'
#' @param v1 Vector 1
#' @param v2 Vector 2
#'
#' @return Value
#' @keywords internal
#'
#' @examples
#' #None
dot = function(v1,v2) {
  sum(v1*v2)
}

#' Dot Product
#'
#' @param v1 Vector 1
#' @param v2 Vector 2
#'
#' @return Value
#' @keywords internal
#'
#' @examples
#' #None
unit_vector = function(v) {
  v/sqrt(sum(v*v))
}
