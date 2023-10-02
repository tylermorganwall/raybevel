#' Extrude Sides of Polygon
#'
#' @keywords internal
extrude_sides = function(vertices, holes = list(), bottom = 0, top = 1) {
  vert_list = list()
  idx_list = list()
  #Need to ensure all polygons are closed
  close_poly = function(dat) {
    if(!all(dat[1L,] == dat[nrow(dat),])) {
      dat[c(seq_len(nrow(dat)), 1L),]
    } else dat
  }
  vertices = close_poly(vertices)
  holes = lapply(holes, close_poly)

  all_polys = c(list(vertices), holes)
  n_verts = 0
  for(j in seq_len(length(all_polys))) {
    polyv = all_polys[[j]]
    x = polyv[,1]
    y = polyv[,2]

    nr = nrow(polyv)-1

    vertices_top = cbind(polyv[-(nr+1),1:2],rep(top,nr))
    vertices_bottom = cbind(polyv[-(nr+1),1:2],rep(bottom,nr))
    colnames(vertices_top) = c("x","y","z")
    colnames(vertices_bottom) = c("x","y","z")

    vert_list[[j]] = rbind(vertices_top,vertices_bottom)

    # Find which direction polygon is wound by computing signed area,
    # assumes non-intersecting polygon (side is a closed polygon).  CW
    # outer polygon need to flip sides, as do CCW holes

    i = seq_len(nr)
    ii = seq_len(nr) + 1L   # i + 1
    area_s = sum(x[i] * y[ii] - x[ii] * y[i]) / 2

    ccw = area_s >= 0  # treat degenerates as counter-clockwise
    side_rev = (j == 1L && ccw) || !ccw
    #Get matrices to create triangles from polygon
    idx_mat = matrix(unlist(lapply(seq_len(nr),rep,2)), ncol = 3, nrow=2*nr)
    tri_idx_mat = matrix(c(0,nr,nr,
                           0,nr,0), nrow = 2*nr, ncol = 3, byrow=TRUE)
    vert_idx_mat = matrix(c(0,0,1,
                            0,1,1), nrow = 2*nr, ncol = 3, byrow=TRUE)
    end_idx_mat = matrix(0, nrow = 2*nr, ncol = 3, byrow=TRUE)
    end_idx_mat[seq(2*nr-1,2*nr,by=1),] = c(0,0,0,-nr,-nr,-nr)
    tri_idx = idx_mat + tri_idx_mat + vert_idx_mat + end_idx_mat
    if(!side_rev) {
      idx_list[[j]] = tri_idx[,3:1] + n_verts
    } else {
      idx_list[[j]] = tri_idx + n_verts
    }
    n_verts = n_verts + nrow(vert_list[[j]])
  }

  v_mat = do.call(rbind,vert_list)
  i_mat = do.call(rbind,idx_list)
  edge_mesh = rayvertex::construct_mesh(vertices = as.matrix(v_mat),
                                        indices = as.matrix(i_mat)-1)
  return(edge_mesh)
}
