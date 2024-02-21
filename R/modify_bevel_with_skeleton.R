#' Modify bevel data using straight skeleton
#'
#'
#' @keywords internal
#' @return list. Modified bevel_height and bevel_offset vectors.
modify_bevel_with_skeleton = function(bevel_offsets, bevel_heights, straight_skeleton) {
  if(is.list(bevel_offsets) &&
     !is.null(bevel_offsets$x) &&
     !is.null(bevel_offsets$y)) {
    bevel_heights = bevel_offsets$y
    bevel_offsets = bevel_offsets$x
  }
  # Extract skeleton's nodes that are not already in the bevel
  unique_times = unique(straight_skeleton$nodes$time)
  unique_times = setdiff(unique_times, bevel_offsets)
  unique_times = unique_times[unique_times < max(bevel_offsets)]

  # Initialize new bevel data vectors
  new_bevel_height = bevel_heights
  new_bevel_offset = bevel_offsets

  for(time in unique_times) {
    # Identify the indices where new values should be inserted
    insert_idx = sum(new_bevel_offset < time) + 1

    # Compute interpolated bevel height
    if (insert_idx > 1 && insert_idx <= length(new_bevel_height)) {
      prev_height = new_bevel_height[insert_idx - 1]
      next_height = new_bevel_height[insert_idx]
      prev_offset = new_bevel_offset[insert_idx - 1]
      next_offset = new_bevel_offset[insert_idx]
      offset_range = next_offset - prev_offset
      height_range = next_height - prev_height
      if(height_range == 0) {
        next
      }
      t = (time - prev_offset)/offset_range
      interpolated_height = (1 - t) * prev_height + t * next_height
    } else {
      # If the new time is outside the existing range, no interpolation is needed
      interpolated_height = time
    }

    # Insert new data points into the bevel vectors
    new_bevel_height = c(new_bevel_height[1:(insert_idx - 1)], interpolated_height, new_bevel_height[insert_idx:length(new_bevel_height)])
    new_bevel_offset = c(new_bevel_offset[1:(insert_idx - 1)], time, new_bevel_offset[insert_idx:length(new_bevel_offset)])
  }

  return(list(x = new_bevel_offset, y = new_bevel_height))
}
