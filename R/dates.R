#' Convertir un vector de valores interpretables como epoch a POSIXct
#'
#' @param epoch Un vector de valores interpretables como epoch
#' @return Un vector de valores POSIXct
#' @export
dt_as_posix   = function(epoch) { anytime(epoch)   }

#' Convertir un vector de valores interpretables como epoch a dates
#'
#' @param epoch Un vector de valores interpretables como epoch
#' @return Un vector de valores Date
#' @export
dt_as_date    = function(epoch) { anydate(epoch)   }

#' Convertir un vector de valores interpretables como epoch a cadenas de Timestamp
#'
#' @param epoch Un vector de valores interpretables como epoch
#' @return Un vector de cadenas con formato Timestamp
#' @export
dt_as_tms     = function(epoch) { format(epoch, format = "%Y-%m-%d-%H:%M:%S") }

#' Convertir fechas a formato numerico Unix
#'
#' @param date Valor de tipo fecha a convertir
#' @return Numero interpretable como un epoch Unix
#' @export
dt_as_unix    = function(date)  {
   if (("POSIXct" %in% class(date))) return (as.numeric(date))

   # if (class(date) == "Date") date = as.POSIXct(paste(date, "01:00:00 UTC"))
   if (inherits(date, "Date")) date = as.POSIXct(paste(date, "01:00:00 UTC"))
   hora = strftime(date, format="%H:%M:%S")
   if (hora == "00:00:00") hora = "01:00:00"
   as.numeric(anytime(paste(strftime(date, format="%Y/%m/%d"), hora)))
}

#' Redondea la fecha al intervalo dado
#'
#' @param date Fecha a redondear
#' @param interval Intervalo de redondeo
#' @param prev Redondear a la fecha anterior/siguiente o igual. Defecto: Anterior
#' @return La fecha redondeada en formato epoch
#' @export
dt_round2Epoch = function(date, interval, prev=TRUE) {
   org = dt_as_unix(date)
   dif = org %% interval
   res =  org - dif
   if (!prev) res = res + interval
   res
}

#' Redondea la fecha al intervalo dado
#'
#' @param date Fecha a redondear
#' @param interval Intervalo de redondeo
#' @param prev Redondear a la fecha anterior/siguiente o igual. Defecto: Anterior
#' @return La fecha redondeada en formato POSIXct
#' @export
dt_round2Posix = function(date, interval, prev=TRUE) {
   org = dt_as_unix(date)
   dif = org %% interval
   res =  org - dif
   if (!prev) res = res + interval
   as.POSIXct(res)
}

#' Calcula y formatea un intervalo de tiempo
#'
#' @param start Inicio del intervalo
#' @param end   Fin del intervalo
#' @return El tiempo consumido formateado
#' @export
dt_elapsed = function(start, end) {
   dsec    = as.numeric(difftime(end, start, units = "secs"))
   hours   = floor(dsec / 3600)
   minutes = floor((dsec - 3600 * hours) / 60)
   seconds = dsec - 3600*hours - 60*minutes
   paste0( sapply(c(hours, minutes, seconds),
                  function(x) {
                     formatC(x, width = 2, format = "d", flag = "0")
                 }), collapse = ":")
}
