#' Discretize and Merge Nodes
#'
#' @keywords internal
find_dangling_nodes = function(skeleton) {
  # Extract node IDs from links dataframe
  source_nodes = unique(skeleton$links$source)
  destination_nodes = unique(skeleton$links$destination)

  # Find nodes that don't have any link pointing to them
  nodes_without_incoming_links = setdiff(skeleton$nodes$id, destination_nodes)

  # Find nodes that don't have any link originating from them
  nodes_without_outgoing_links = setdiff(skeleton$nodes$id, source_nodes)

  # Combine the two lists and identify unique IDs
  dangling_nodes = unique(c(nodes_without_incoming_links, nodes_without_outgoing_links))

  # Return the IDs of the dangling nodes
  return(dangling_nodes)
}
