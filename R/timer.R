#' Print time
#'
#' @return Nothing
#' @keywords internal
init_time = function() {
  assign("init_time", proc.time()[3], envir = ray_environment)
  assign("prev_time", proc.time()[3], envir = ray_environment)
}

#' Get time
#'
#' @return Nothing
#' @keywords internal
get_time = function(init = TRUE) {
  if(init) {
    get("init_time", envir = ray_environment)
  } else {
    get("prev_time", envir = ray_environment)
  }
}

#' Print time
#'
#' @return Nothing
#' @keywords internal
print_time = function(verbose = FALSE, message_text = "") {
  if(verbose) {
    time_now = proc.time()[3]
    message(sprintf("%-27s: %0.1f secs (Total: %0.1f secs)",
                    message_text,
                    time_now-get_time(FALSE),
                    time_now-get_time(TRUE)))
    assign("prev_time", time_now, envir = ray_environment)
  }
}
