#' Get Face IDs
#'
#' @param ss
#'
#' @keywords internal
convert_ss_to_polygons = function(ss, progress = FALSE) {
  n_cores = getOption("cores", default = getOption("Ncpus", default = parallel::detectCores()))
  list_all_polygons = convert_ss_to_polygons_rcpp(ss, n_cores, progress = progress)
  return(list_all_polygons)
}
