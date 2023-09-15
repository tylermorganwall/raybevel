#' Arrange or Reorder Rows of a Data Frame by Columns
#'
#' This function sorts a data frame based on the provided columns.
#'
#' @param data A data frame.
#' @param ... Column names to determine the sort order.
#'
#' @return A sorted data frame.
#'
#' @keywords internal
arrange_base = function(data, ...){
  colnames = as.character(substitute(list(...))[-1])
  order_args = lapply(colnames, function(col) data[[col]])
  sorted_indices = do.call(order, order_args)
  return(data[sorted_indices, ])
}
