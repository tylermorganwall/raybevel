#' Skeletonize a polygon
#'
#' This function generates a straight skeleton of a polygon, based on a set of vertices and holes.
#' It uses the CGAL library to create the straight skeleton using exact arithmetic,
#' and then parses that file into a more manageable format.
#'
#' @param vertices Default `NULL`. A matrix of x and y coordinates representing the vertices of the polygon in counter-clockwise (CCW) order.
#' @param holes Default `list()`. A list of matrices, each representing a hole in the polygon with x and y coordinates in clockwise (CW) order.
#' @param debug Default `FALSE`. A logical flag that controls whether debugging information should be printed.
#' @param return_raw_ss Default `FALSE`. A logical flag that controls whether the raw straight skeleton should be returned.
#' @param use_cgal Default `TRUE`. A logical flag that controls whether the CGAL library should be used for computation.
#'
#' @return If `return_raw_ss` is FALSE, a list with two data frames, 'nodes' and 'links', which represent the nodes and edges of the straight skeleton, respectively.
#' If `return_raw_ss` is TRUE, a data frame representing the raw straight skeleton is returned.
#' If the polygon is not simple, a warning is issued and NULL is returned.
#'
#' @export
#' @examples
#' # Example 1: Simple rectangle polygon with no holes
#' vertices1 = matrix(c(0,0, 4,0, 4,3, 0,3, 0,0), ncol = 2, byrow = TRUE)
#' skeleton1 = skeletonize(vertices1)
#' plot_skeleton(skeleton1)
#'
#' # Example 2: Triangle polygon with no holes
#' vertices2 = matrix(c(0,0, 2,0, 1,2, 0,0), ncol = 2, byrow = TRUE)
#' skeleton2 = skeletonize(vertices2)
#' plot_skeleton(skeleton2)
#'
#' # Example 3: Polygon with a hole
#' # Outer polygon
#' vertices3 = matrix(c(0,0, 5,0, 5,5, 0,5, 0,0), ncol = 2, byrow = TRUE)
#' # Hole inside the polygon
#' hole3 = matrix(c(1,1, 4,1, 4,4, 1,4, 1,1), ncol = 2, byrow = TRUE)[5:1,]
#' skeleton3 = skeletonize(vertices3, holes = list(hole3))
#' plot_skeleton(skeleton3)
#'
#' # Example 4: Polygon with multiple holes
#' # Outer polygon
#' vertices4 = matrix(c(0,0, 7,0, 7,7, 0,7, 0,0), ncol = 2, byrow = TRUE)
#' # Holes inside the polygon
#' hole4_1 = matrix(c(1,1, 2,1, 2,2, 1,2, 1,1), ncol = 2, byrow = TRUE)[5:1,]
#' hole4_2 = matrix(c(5,5, 6,5, 6,6, 5,6, 5,5), ncol = 2, byrow = TRUE)[5:1,]
#' skeleton4 = skeletonize(vertices4, holes = list(hole4_1, hole4_2))
#' plot_skeleton(skeleton4)
#'
#' # Example 5: Using debug and returning raw straight skeleton
#' vertices5 = matrix(c(0,0, 3,0, 3,3, 0,3, 0,0), ncol = 2, byrow = TRUE)
#' raw_skeleton5 = skeletonize(vertices5, debug = TRUE, return_raw_ss = TRUE)
skeletonize = function(vertices, holes = list(), debug = FALSE,
                       return_raw_ss = FALSE, use_cgal = TRUE,
                       progress = TRUE) {
  if(inherits(vertices, "sf")) {
    pb = progress::progress_bar$new(
      format = ":current/:total skeletonizing [:bar] eta: :eta",
      total = nrow(vertices), clear = TRUE, width = 60)
    all_coords = lapply(vertices$geometry, sf::st_coordinates)
    is_multipolygon = unlist(lapply(vertices$geometry, (function(x) inherits(x,"MULTIPOLYGON"))))
    is_polygon = unlist(lapply(vertices$geometry, (function(x) inherits(x,"POLYGON"))))

    ss_list = list()
    counter = 1
    for(i in seq_len(length(all_coords))) {
      if(progress) {
        pb$tick()
      }
      single_row_geometry = all_coords[[i]]
      if(is_polygon[i]) {
        all_features = split(as.data.frame(single_row_geometry)[,c("X","Y","L1")],
                             list(single_row_geometry[,c("L2")]))
        for(j in seq_len(length(all_features))) {
          single_feature = all_features[[j]]
          exterior_points = single_feature[,"L1"] == 1
          verts = as.matrix(single_feature[exterior_points,c("X","Y")])
          if(all(verts[1,] == verts[nrow(verts),])) {
            verts = verts[-nrow(verts),]
          }
          hole_mat = single_feature[!exterior_points,]
          if(nrow(hole_mat) > 0) {
            holes = split(as.data.frame(hole_mat)[,c("X","Y")],
                          hole_mat[,c("L1")])
          } else {
            holes = list()
          }
          if(!is_simple_polygon(verts)) {
            warning(sprintf("Row %i in `sf` object not simple polygon, skipping",i))
            next
          }
          #Fix orientations
          if(!is_ccw_polygon(as.matrix(verts))) {
            verts = verts[rev(seq_len(nrow(verts))),]
          }
          for(k in seq_len(length(holes))) {
            if(all(holes[[k]][1,] == holes[[k]][nrow(holes[[k]]),])) {
              holes[[k]] = holes[[k]][-nrow(holes[[k]]),]
            }
            holes[[k]] = as.matrix(holes[[k]])
            if(!is_simple_polygon(as.matrix(holes[[k]]))) {
              warning(sprintf("Hole in row %i in `sf` object not simple polygon, skipping", i))
              next
            }
            if(is_ccw_polygon(holes[[k]])) {
              holes[[k]] = holes[[k]][rev(seq_len(nrow(holes[[k]]))),]
            }
          }
          #Skeletonize
          ss_list[[counter]] = skeletonize(as.matrix(verts), debug = debug,
                                           holes = holes,
                                           return_raw_ss = return_raw_ss, use_cgal = use_cgal)
          counter = counter + 1
        }
      } else if (is_multipolygon[i]) {
        all_polygons = split(as.data.frame(single_row_geometry)[,c("X", "Y", "L1", "L2")],
                             list(single_row_geometry[,c("L3")]))
        for(ii in seq_len(length(all_polygons))) {
          single_polygon = all_polygons[[ii]]
          all_features = split(as.data.frame(single_polygon)[,c("X", "Y", "L1")],
                               list(single_polygon[,c("L2")]))
          for(j in seq_len(length(all_features))) {
            single_feature = all_features[[j]]
            exterior_points = single_feature[,"L1"] == 1
            verts = as.matrix(single_feature[exterior_points,c("X","Y")])
            if(all(verts[1,] == verts[nrow(verts),])) {
              verts = verts[-nrow(verts),]
            }
            hole_mat = single_feature[!exterior_points,]
            if(nrow(hole_mat) > 0) {
              holes = split(as.data.frame(hole_mat)[,c("X","Y")],
                            hole_mat[,c("L1")])
            } else {
              holes = list()
            }
            #Fix orientations
            if(!is_ccw_polygon(verts)) {
              verts = verts[rev(seq_len(nrow(verts))),]
            }
            for(k in seq_len(length(holes))) {
              if(!is_simple_polygon(as.matrix(holes[[k]]))) {
                warning(sprintf("Hole in row %i in `sf` object not simple polygon, skipping", i))
                next
              }
              if(is_ccw_polygon(holes[[k]])) {
                holes[[k]] = holes[[k]][rev(seq_len(nrow(holes[[k]]))),]
              }
              holes[[k]] = as.matrix(holes[[k]])
            }
            #Skeletonize
            ss_list[[counter]] = skeletonize(as.matrix(verts), debug = debug,
                                             holes = holes,
                                             return_raw_ss = return_raw_ss, use_cgal = use_cgal)
            counter = counter + 1
          }
        }
      }
    }
    class(ss_list) = c("rayskeleton_list", "list")
    return(ss_list)
  }
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
      hole = as.matrix(holes[[i]])
      if(all(hole[1,] == hole[nrow(hole),])) {
        holes[[i]] = hole[-nrow(hole),]
      } else {
        holes[[i]] = hole
      }
    }
  }
  return_data = skeletonize_rcpp(vertices, holes, 0)
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
  colnames(links) = c("source","destination" ,"destination_time","source_time")
  links = links[,c(1,2,4,3)]
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
  ss = list(nodes = nodes, links = links)
  class(ss) = "rayskeleton"
  attr(ss,"original_vertices") = vertices
  attr(ss,"original_holes") = holes
  return(ss)
}

