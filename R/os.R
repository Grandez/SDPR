#' Obtiene el tipo del sistema operativo
#' @export
os_type = function() {
   #Hay que usar .Platform, R.version y Sys.info()
   .Platform$OS.type
}
#' Indica si la plataforma de ejecucion es Windows
#' @return TRUE o FALSE
#' @export
os_windows = function() {
   .Platform$OS.type == "windows"
}
#' Indica si la plataforma de ejecucion es de tipo Unix
#' @return TRUE o FALSE
#' @export
os_unix = function() {
   .Platform$OS.type == "unix"
}
