#' Remove Reversed Links
#'
#' @keywords internal
remove_reversed_links = function(skeleton) {
  # Create two new columns: min_id and max_id
  skeleton$links$min_id = pmin(skeleton$links$source, skeleton$links$destination)
  skeleton$links$max_id = pmax(skeleton$links$source, skeleton$links$destination)

  # Identify duplicates based on min_id and max_id
  dupes = duplicated(skeleton$links[, c("min_id", "max_id")])

  # Remove the duplicated links
  skeleton$links = skeleton$links[!dupes, ]

  # Remove the temporary columns
  skeleton$links$min_id = NULL
  skeleton$links$max_id = NULL

  return(skeleton)
}
