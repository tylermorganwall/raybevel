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
  vertices_pad = rbind(vertices[nrow(vertices)-1,], vertices)
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
  verts_to_remove = c(1,unlist(remove_verts))
  vertices = vertices_pad[-verts_to_remove,]
  return_data = skeletonize_rcpp(vertices, holes, tol)
  if(length(return_data) == 0) {
    warning("Polygon is not simple--not computing straight skeleton.")
    return()
  }
  ss = do.call("rbind", return_data$ss)
  colnames(ss) = c("end_x","end_y","start_x","start_y","time","split","id")
  from_split = ss[,6]
  ss = ss[,c(3:4,1:2,5)]
  dig = log10(tol)
  if(dig > 0) {
    dig = -dig
  }
  all_vertices = unique(rbind(ss[,c(1:2)],ss[,c(3:4)]))
  colnames(all_vertices) = c("x","y")
  ss = unique(round(ss, digits=-dig))
  ss = as.data.frame(ss)
  rounded_verts = as.data.frame(unique(round(vertices, digits=-dig)))
  rounded_holes = do.call("rbind",holes)
  rounded_verts = rbind(rounded_verts,rounded_holes)
  colnames(rounded_verts) = c("x","y")
  nodes = as.data.frame(unique(round(all_vertices, digits=-dig)))
  is_edge_nodes = which(nodes$x %in% rounded_verts$x & nodes$y %in% rounded_verts$y)
  nodes$edge = FALSE
  nodes$edge[is_edge_nodes] = TRUE
  nodes = nodes[order(!nodes$edge),]
  nodes$id = NA
  for(i in seq_len(length(is_edge_nodes))) {
    nodes$id[i] = which(rounded_verts$x == nodes$x[i] &
                        rounded_verts$y == nodes$y[i])
  }
  nodes = nodes[order(nodes$id),]
  max_id = max(nodes$id, na.rm=TRUE)
  internal_verts = sum(!nodes$edge)
  nodes$id[which(is.na(nodes$id))] = seq(max_id+1,max_id+internal_verts)
  ss$edge = FALSE
  ss$source = NA
  ss$destination = NA
  ss$source_time = 0
  for(i in seq_len(nrow(ss))) {
    idx = which(ss$start_x[i] == nodes$x &
                ss$start_y[i] == nodes$y)
    dest_idx = which(ss$end_x[i] == nodes$x &
                     ss$end_y[i] == nodes$y)
    ss$source[i] = nodes$id[idx]
    ss$destination[i] = nodes$id[dest_idx]
    ss$edge[i] = nodes$edge[idx]
  }
  internal_node_times = unique(ss[,c("time","destination")])
  for(i in seq_len(nrow(ss))) {
    if(!ss$edge[i]) {
      s_time = internal_node_times$time[ss$source[i] == internal_node_times$destination]
      if(length(s_time) == 1) {
        ss$source_time[i] = internal_node_times$time[ss$source[i] == internal_node_times$destination]
      }
    }
  }
  reorder_verts = order(ss$source, ss$source_time, ss$time)
  ss2 = ss[reorder_verts,]
  ss2 = ss2[,c(7,8,6,9,5)]
  rownames(ss2) = seq_len(nrow(ss2))
  colnames(ss2) = c("source","destination", "edge" ,"source_time","destination_time")
  node_with_times = unique(data.frame(id = c(ss2[,1],ss2[,2]),time = c(ss2[,4],ss2[,5])))
  nodes$time = 0
  for(i in seq_len(nrow(nodes))) {
    idx = which(nodes$id[i] == node_with_times$id)
    nodes$time[i] = node_with_times$time[idx]
  }
  nodes = nodes[,c("id","x","y","edge","time")]
  rownames(nodes) = seq_len(nrow(nodes))
  return_val = list(nodes = nodes,
                    links = ss2)
  if(debug) {
    dp = as.data.frame(return_data$dp)
    lavs = as.data.frame(return_data$lavs)
    debug = as.data.frame(return_data$int_debug_data)
    split_debug = as.data.frame(return_data$split_debug_data)
    oe = as.data.frame(return_data$original_edges)
    bisector_debug = as.data.frame(return_data$bisector_debug)
    debug_reflex = as.data.frame(return_data$debug_reflex)
    simple = return_data$simple


    names(dp) = c("x","y","vx","vy","id","reflex","split")
    names(lavs) = c("x","y","lav","counter","valid","len")
    names(debug) = c("o1x","o1y","d1x","d1y",
                     "o2x","o2y","d2x","d2y",
                     "intx","inty","valid", "id", "edge_id")
    names(split_debug) = c("p1x","p1y","d1x","d1y",
                           "p3x","p3y","bx","by",
                           "left","right","edge","id", "edge_id",
                           "vx","vy","d2x","d2y","bpointx","bpointy")
    names(bisector_debug) = c("box","boy","bdx","bdy")
    names(debug_reflex) = c("x","y","reflex")

    attr(return_val,"dp") = dp
    attr(return_val,"lavs") = lavs
    attr(return_val,"debug") = debug
    attr(return_val,"split_debug") = split_debug
    attr(return_val,"from_split") = from_split
    attr(return_val,"original_edges") = oe
    attr(return_val,"bisector_debug") = bisector_debug
    attr(return_val,"debug_reflex") = debug_reflex
    attr(return_val,"simple_polygon") = simple
  }
  return(return_val)
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
  # browser()
  links = ss$links
  nodes = ss$nodes
  max_time = max(links$destination_time)
  stopifnot(offset < max_time)
  links = rbind(links,data.frame(source = seq_len(sum(links$edge)),
                                 destination = c(seq(2,sum(links$edge)),1),
                                 edge = TRUE,
                                 source_time = 0,
                                 destination_time = 0))
  links$visited = links$edge
  first_node = links[1,1]
  tmp_source = first_node
  first_dest = links[1,2]
  tmp_dest = first_dest

  first_vertex = c(Inf, Inf)

  offset_polygon_verts = list()
  counter = 1
  num_polygons = 1
  first = TRUE
  new_poly = TRUE
  while(first || !(tmp_source == first_node && tmp_dest == first_dest) || sum(!links$visited) != 0) {
    # browser()
    # print(c(tmp_source,tmp_dest))
    if(tmp_source == first_node && tmp_dest == first_dest && !first) {
      # browser()
      new_poly = TRUE
      remaining_links = links[!links$visited,]
      tmp_source = remaining_links[1,1]
      tmp_dest   = remaining_links[1,2]
      first_node = tmp_source
      first_dest = tmp_dest
      num_polygons = num_polygons + 1
    }
    first = FALSE
    node1_position = as.numeric(nodes[tmp_source,2:3])
    node2_position = as.numeric(nodes[tmp_dest,2:3])
    segments(node1_position[1],node1_position[2],node2_position[1],node2_position[2], col="green")
    node1_time = nodes[tmp_source,5]
    node2_time = nodes[tmp_dest,5]
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
       nodes$edge[tmp_dest]){
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
        best_dest = candidate_dest
        best_source = candidate_source
      }
    }
    # print(best_angle)

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
