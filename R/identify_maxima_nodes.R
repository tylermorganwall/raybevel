#' Identify Local and Global Maxima Nodes
#'
#' @param straight_skeleton The straight skeleton object.
#'
#' @return Vector containting maxima ids
identify_maxima_nodes = function(straight_skeleton) {
  nodes = straight_skeleton$nodes
  links = straight_skeleton$links

  local_maxima = list()
  global_maxima = list()

  for(i in seq_len(nrow(nodes))) {
    node_id = nodes[i, 'id']
    node_time = nodes[i, 'time']

    # Find neighboring nodes
    neighbors = c(links[links$source == node_id, 'destination'],
                  links[links$destination == node_id, 'source'])

    neighbor_times = nodes[nodes$id %in% neighbors, 'time']

    # Check if the node is a local maximum
    if(all(node_time > neighbor_times)) {
      local_maxima[[i]] = node_id
    }
  }

  return(unlist(local_maxima))
}
