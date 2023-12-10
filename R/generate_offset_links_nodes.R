#' Generate Offset Polygon
#'
#' @keywords internal
generate_offset_links_nodes = function(ss, offsets, return_polys = FALSE, progress = FALSE, verbose = FALSE) {
  links = ss$links
  nodes = ss$nodes
  offsets = offsets[which(offsets < max(nodes$time)) ]

  node_maxima_ids = identify_maxima_nodes(ss)
  nodes$local_maxima = nodes$id %in% node_maxima_ids
  links$local_maxima_destination = links$destination %in% node_maxima_ids
  links$local_maxima_source = links$source %in% node_maxima_ids

  max_time = max(links$destination_time)
  if(all(offsets >= max_time)) {
    stop(sprintf("offsets `c(%s)` must not be greater than or equal to max time: %0.4f",
                 paste0(sprintf("%0.4f", offsets), collapse = ", "), max_time))
  }
  poly_list = generate_offset_links_nodes_rcpp(links, nodes, offsets = offsets, progress = progress)

  new_links_offset = list()
  for(ii in seq_along(poly_list)) {
    if(length(poly_list[[ii]]) > 0) {
      all_new_nodes = do.call("rbind",poly_list[[ii]])
      colnames(all_new_nodes) = c("polygon_id", "source", "destination", "new_id",
                                  "x","y","time","x1","y1","x2","y2", "new_node")
      if(return_polys) {
        just_poly = all_new_nodes[,c("polygon_id", "x","y")]
        names(just_poly) = c("id","x","y")
        just_poly = split(as.data.frame(just_poly[,-1]), just_poly[,1])
        return(just_poly)
      }
      new_links_poly = split(as.data.frame(all_new_nodes[,-1,drop=FALSE]), all_new_nodes[,1,drop=FALSE])

      new_links_offset[[ii]] = new_links_poly
    }
  }
  if(return_polys) {
    all_new_nodes = do.call("rbind",new_links_offset)
    just_poly = all_new_nodes[,c("polygon_id", "x","y")]
    names(just_poly) = c("id","x","y")
    just_poly = split(as.data.frame(just_poly[,-1]), just_poly[,1])
    return(just_poly)
  }
  print_time(verbose, "Inserting polygon links and nodes")

  return(insert_polygon_links_nodes(ss, new_links_offset))
}


#' Insert Polygon Links/Nodes
#'
#' @keywords internal
insert_polygon_links_nodes = function(ss, new_links_all) {
  links = ss$links
  nodes = ss$nodes
  counter = 1
  poly_counter = 1
  new_link_list = list()
  new_link_list_poly = list()
  new_node_list = list()
  links_to_remove = list()
  raw_df = function(list_vals) {
    structure(list_vals,
              class = "data.frame",
              row.names = seq_len(length(list_vals[[1]])))
  }
  # plot_skeleton(ss, label_ids = TRUE)
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
      prev_id = 0
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
        stopifnot(length(link_remove_idx) == 1)
        stopifnot(length(old_source) == 1)
        stopifnot(length(old_destination) == 1)
        stopifnot(length(poly_id) == 1)
        stopifnot(length(new_time) == 1)

        old_link = links[link_remove_idx,]
        old_source_time = old_link$source_time
        old_destination_time = old_link$destination_time

        if(old_source != new_id) {
          links_to_remove[[counter]] = link_remove_idx

          new_link_list[[counter]] = raw_df(list(source = c(old_source,new_id),
                                                 destination = c(new_id,old_destination),
                                                 source_time = c(old_source_time, new_time),
                                                 destination_time = c(new_time, old_destination_time),
                                                 edge = c(FALSE, FALSE),
                                                 link_remove = rep(link_remove_idx,2),
                                                 old_source_id = rep(old_source,2),
                                                 old_dest_id = rep(old_destination,2))) #We will remove this variable before joining
          counter = counter + 1
        }
        # if(prev_id == 0 || prev_id == new_id ) {
          new_link_list_poly[[poly_counter]] = raw_df(list(source = new_id,
                                                          destination = poly_id,
                                                          source_time = new_time,
                                                          destination_time = new_time,
                                                          edge = FALSE))
          poly_counter = poly_counter + 1

        # } else {
        #   new_link_list_poly[[poly_counter]] = data.frame(source = prev_id,
        #                                              destination = new_id,
        #                                              source_time = new_time,
        #                                              destination_time = new_time,
        #                                              edge = FALSE)
        #   poly_counter = poly_counter + 1
        #
        #   new_link_list_poly[[poly_counter]] = data.frame(source = new_id,
        #                                              destination = poly_id,
        #                                              source_time = new_time,
        #                                              destination_time = new_time,
        #                                              edge = FALSE)
        #   poly_counter = poly_counter + 1

        # }
          # prev_id = poly_id
        # if(j == nrow(single_poly) && poly_id != single_poly$new_id[1]) {
        #   browser()
        #   new_link_list_poly[[poly_counter]] = data.frame(source = poly_id,
        #                                                   destination = single_poly$new_id[1],
        #                                                   source_time = new_time,
        #                                                   destination_time = new_time,
        #                                                   edge = FALSE)
        #   poly_counter = poly_counter + 1
        # }
      }
    }
  }
  new_nodes = do.call("rbind", new_node_list)
  new_skeleton_links_raw = do.call("rbind",new_link_list)
  links_to_slice = split(new_skeleton_links_raw,new_skeleton_links_raw[,6])
  # browser()
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
