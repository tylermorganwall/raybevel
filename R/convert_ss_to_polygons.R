#' Get Face IDs
#'
#' @param ss
#'
#' @keywords internal
get_n_cores = function() {
  n_cores = getOption("cores", default = getOption("Ncpus", default = parallel::detectCores()))
  if (is.null(n_cores) || length(n_cores) == 0) {
    return(1L)
  }
  n_cores = suppressWarnings(as.integer(n_cores[[1]]))
  if (is.na(n_cores) || n_cores < 1L) {
    return(1L)
  }
  n_cores
}

convert_ss_to_polygons = function(ss, progress = FALSE) {
  n_cores = get_n_cores()
  list_all_polygons = convert_ss_to_polygons_rcpp(ss, n_cores, progress = progress)
  return(list_all_polygons)
}
