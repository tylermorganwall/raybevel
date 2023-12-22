ray_environment = new.env(parent = emptyenv())

.onLoad = function(libname, pkgname) {
  assign("init_time", 0, envir = ray_environment)
  assign("prev_time", 0, envir = ray_environment)
  assign("final_polygon", TRUE, envir = ray_environment)
}
