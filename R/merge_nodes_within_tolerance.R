#' Discretize and Merge Nodes
#'
#' @param ss
#'
#' @keywords internal
discretize_and_merge_nodes = function(skeleton, tolerance = 1e-5) {
  # Calculate bounding box of the nodes
  x_range = range(skeleton$nodes$x)
  y_range = range(skeleton$nodes$y)
  width = x_range[2] - x_range[1]
  height = y_range[2] - y_range[1]

  # Snap each point to its nearest grid cell
  skeleton$nodes$grid_x = round((skeleton$nodes$x - x_range[1]) / width * 1/tolerance)
  skeleton$nodes$grid_y = round((skeleton$nodes$y - y_range[1]) / height * 1/tolerance)

  # Merge nodes based on their grid cells
  skeleton$nodes$grid_id = paste(skeleton$nodes$grid_x, skeleton$nodes$grid_y, sep="-")
  nodes_to_remove = duplicated(skeleton$nodes$grid_id)
  nodes_to_keep = which(!nodes_to_remove)

  # Map all nodes to their corresponding kept node
  id_map = stats::setNames(nodes_to_keep, skeleton$nodes$id[nodes_to_keep])
  for(node_id in skeleton$nodes$id) {
    grid_id = skeleton$nodes$grid_id[skeleton$nodes$id == node_id]
    id_map[as.character(node_id)] = skeleton$nodes$id[skeleton$nodes$grid_id == grid_id][1]
  }

  # Update links to reflect the merged nodes
  skeleton$links$source = id_map[as.character(skeleton$links$source)]
  skeleton$links$destination = id_map[as.character(skeleton$links$destination)]

  # Remove duplicate nodes
  skeleton$nodes = skeleton$nodes[!nodes_to_remove, ]
  skeleton$nodes = skeleton$nodes[, c("id", "x", "y", "time", "edge"), drop = FALSE]

  # Calculate new ids
  skeleton = recalculate_ordered_ids(skeleton)

  #Remove self-referencing links
  skeleton$links = skeleton$links[skeleton$links$source != skeleton$links$destination, ]

  #Remove duplicate links
  link_duplicates = duplicated(skeleton$links[, c("source", "destination")])
  skeleton$links = skeleton$links[!link_duplicates, ]

  return(skeleton)
}
