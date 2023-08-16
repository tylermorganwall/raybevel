#' Remove Node Duplicates
#'
#' @description Replace dup
#'
#' @param ss
#'
#' @return
#' @keywords internal
#'
#' @examples
remove_node_duplicates = function(ss) {
  row_hashes = apply(ss$nodes[,-1], 1, digest::digest)
  unique_row_hashes = unique(row_hashes)
  id_val = rep(0, length(unique_row_hashes))
  replace_node_list = list()
  for(i in seq_len(length(unique_row_hashes))) {
    tmp_id_val = which(unique_row_hashes[i] == row_hashes)
    id_val[i] = tmp_id_val[1]
    replace_node_list[[i]] = tmp_id_val[-1]
  }
  new_ss = ss
  for(i in seq_len(length(id_val))) {
    if(length(replace_node_list[[i]]) > 0) {
      #Replace link information with de-duplicated nodes
      new_ss$links$source[new_ss$links$source %in% replace_node_list[[i]]] = id_val[i]
      new_ss$links$destination[new_ss$links$destination %in% replace_node_list[[i]]] = id_val[i]
    }
  }
  new_ss$links = new_ss$links[!duplicated(new_ss$links),]
  new_ss$links = new_ss$links[new_ss$links$source != new_ss$links$destination,]

  new_ss$nodes = new_ss$nodes[!new_ss$nodes$id %in% unlist(replace_node_list),]
  fac_id = factor(new_ss$nodes$id)
  levels_id = levels(factor(new_ss$nodes$id))
  new_ss$nodes$id = as.integer(fac_id)

  new_ss$links$source = as.integer(factor(new_ss$links$source, levels = levels_id))
  new_ss$links$destination = as.integer(factor(new_ss$links$destination, levels = levels_id))

  return(new_ss)
}
