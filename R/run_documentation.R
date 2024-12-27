#' @title Run Documentation
#'
#' @description This function determines if the examples are being run in pkgdown. It is not meant to be called by the user.
#'
#' @export
#'
#' @return Boolean value.
#' @examples
#' # See if the documentation should be run.
#' run_docs_raybevel()
run_docs_raybevel = function() {
  return(identical(Sys.getenv("IN_PKGDOWN"), "true"))
}
