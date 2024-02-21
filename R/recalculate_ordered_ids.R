#' Recalculate Ordered IDs
#'
#' @keywords internal
recalculate_ordered_ids = function(skeleton) {
  nodes = skeleton$nodes
  links = skeleton$links
  id_as_factor = as.factor(nodes$id)
  id_levels = levels(id_as_factor)
  nodes$id = as.factor(nodes$id)
  links$source = as.integer(factor(links$source, levels = id_levels))
  links$destination = as.integer(factor(links$destination, levels = id_levels))
  nodes$id = as.integer(id_as_factor)
  skeleton$nodes = nodes
  skeleton$links = links
  return(skeleton)
}
