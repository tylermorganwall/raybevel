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
skeletonize = function(vertices, holes = list(), tol = 1e-10, debug = FALSE,
                       return_raw_ss = FALSE, use_cgal = TRUE) {
  stopifnot(all(vertices[1,] == vertices[nrow(vertices),]))
  stopifnot(ncol(vertices) == 2)
  if(!use_cgal) {
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
        for(j in seq_len(nrow(vertices_pad)-1)[-1]) {
          v1 = holes_pad[j,]
          v2 = holes_pad[j+1,]
          tmp_det = determinant2x2(v1,v2)
          total_sum_det_holes = total_sum_det_holes + tmp_det
        }
        if(total_sum_det_holes > 0) {
          stop(sprintf("`holes[[%i]]` is not CW polygon",i))
        }
      }
    }
    verts_to_remove = c(1,unlist(remove_verts))
    vertices = vertices_pad[-c(verts_to_remove,nrow(vertices_pad)),]
  } else {
    if(all(vertices[1,] == vertices[nrow(vertices),])) {
      vertices = vertices[-nrow(vertices),]
    }
    for(i in seq_len(length(holes))) {
      hole = holes[[i]]
      if(all(hole[1,] == hole[nrow(hole),])) {
        holes[[i]] = hole[-nrow(hole),]
      }
    }
  }
  return_data = skeletonize_rcpp(vertices, holes, tol)
  if(length(return_data) == 0) {
    warning("Polygon is not simple--not computing straight skeleton.")
    return()
  }
  ss = do.call("rbind", return_data$bisectors)
  colnames(ss) = c("end_x","end_y","start_x","start_y","time","time_start", "id", "id_start")
  if(return_raw_ss) {
    return(as.data.frame(ss))
  }
  #De-duplicate half edges
  ss = ss[ss[,"id_start"] < ss[,"id"] | (ss[,"time"] == 0  & ss[,"time_start"] == 0) ,]
  #Re-connect first vertex so everything is CCW
  # stopifnot(ss[1:2,"id_start"] == 0)
  # ss[1,] = ss[1,c(3,4,1,2,5,6,8,7)]
  ss = unique(as.data.frame(ss))
  ss$edge = ss$time == 0 & ss$time_start == 0
  # ss = ss[,c(1:8,9)]
  start_nodes = as.data.frame(ss[,c("id_start", "start_x", "start_y", "time_start")])
  start_nodes = dplyr::arrange(unique(start_nodes[start_nodes$time_start == 0,]),id_start)
  ss = dplyr::arrange(as.data.frame(ss), time_start, time)
  start_ss = ss[,c(8,3:4,6,5)]
  colnames(start_ss) = c("id","x","y","time","other_time")
  end_ss = ss[,c(7,1:2,5,6)]
  colnames(end_ss) = c("id","x","y","time","other_time")
  nodes = rbind(start_ss,end_ss)
  nodes$edge = nodes$time == 0 & nodes$other_time == 0
  nodes = (nodes[nodes$time >= nodes$other_time,])
  nodes = unique(nodes[,c(1:4,6)])
  nodes = dplyr::arrange(nodes, id)
  links = ss[,c("id_start", "id", "time","time_start")]
  colnames(links) = c("source","destination" ,"source_time","destination_time")
  links = as.data.frame(links)
  links$edge = links$source_time == 0 & links$destination_time == 0
  links = links[,c(1,2,5,3,4)]
  id_as_factor = as.factor(nodes$id)
  id_levels = levels(id_as_factor)
  nodes$id = as.factor(nodes$id)
  links$source = as.integer(factor(links$source, levels = id_levels))
  links$destination = as.integer(factor(links$destination, levels = id_levels))
  nodes$id = as.integer(id_as_factor)
  edge_links = links[links$edge,]
  edge_links = edge_links[rev(seq_len(nrow(edge_links))),c(2,1,3,5,4)]
  colnames(edge_links) = c("source","destination" ,"edge", "source_time","destination_time")
  links = rbind(edge_links, links[!links$edge,])
  return(list(nodes = nodes, links = links))
}

