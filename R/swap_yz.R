#' Extrude Sides of Polygon
#'
#' @keywords internal
swap_yz = function(mesh) {
  for(i in seq_along(mesh$vertices)) {
    mesh$vertices[[i]] = mesh$vertices[[i]][,c(1,3,2)]
    mesh$vertices[[i]][,1] = -mesh$vertices[[i]][,1]
  }
  # for(i in seq_along(mesh$shapes)) {
  #   mesh$shapes[[i]]$indices = mesh$shapes[[i]]$indices[,3:1]
  #   mesh$shapes[[i]]$norm_indices = mesh$shapes[[i]]$norm_indices[,3:1]
  #   mesh$shapes[[i]]$tex_indices = mesh$shapes[[i]]$tex_indices[,3:1]
  # }
  return(mesh)
}
