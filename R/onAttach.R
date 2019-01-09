

#' @author Reto Stauffer
.onAttach <- function(libname, pkgname) {
   Sys.setenv("TZ" = "UTC")
   info <- "\nmosmix: Please note that the time zone of this session has been set to UTC!\n"
   packageStartupMessage(info)
}


