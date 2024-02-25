#' @title Tratamiento de logs
#'
#' @description
#' Clase base/abstract para el manejo de logs de las aplicaciones
#'
#' Escribe a
#' - 0 = No se genera log
#' - 1 = Consola
#' - 2 = Archivo
#' - 4 = Base de datos
#'
#' Salvo para consola es necesario indicarle la informacion necesaria
#' respecto a ficheros y bases de datos
#'
#' @import R6
#' @import crayon
#' @export
#
# En fichero usamos tms;tipo_mensaje
# Segun sea el tipo del mensaje, asi es el registro
# Log Level
#   1 - Summary
#
# tipo:mensaje es:
#    1 - Process (Batch, Session, etc)
#    5 - Batch Process
#   10 - Logging/info:
#      tms;10;nivel;datos
#   99 - ERROR
# Salida (out) es
#   - 0 - Nada
#   - 1 - Fichero
#   - 2 - Consola
#
SDPLogger = R6::R6Class("SDPR.LOGGER"
   ,cloneable  = TRUE
   ,lock_class = TRUE
   ,portable   = TRUE
   ,active = list(
       #' @field level Nivel de log
       level  = function(value) {
          if (!missing(value)) private$.level = value
          private$.level
       }
       #' @field output Salidas de log activas como combinacion/suma de
      ,output = function(value) {
          if (!missing(value)) private$.output = value
          private$.output
      }
       #' @field coloured Valor logico. Formatear las salidas por consola
      ,coloured = function(value) {
          if (!missing(value)) private$.coloured = value
          private$.coloured
       }
   )
  ,public = list(
     #  valid   = TRUE
     # ,lastErr = NULL
#     ,print        = function() { message("Generic Logger class") }
     #' @description
     #' Crea una nueva instancia de [R6][R6::R6Class] la clase.
     #'
     #' Notese que esta clase se creara normalente como clase base de otra
     #'
     #' @param module (`character(1)`)\cr
     #'   Etiqueta del modulo que genera los mensajes de logging
     #'   Default value: "General"
     #' @param level (`integer(1)`)\cr
     #'   Nivel de logging que se aplicará a la instancia
     #'   Default value: 0
     #' @param output (`integer(1)`)\cr
     #'   Donde se generará la salida. Es una mascara de bits
     #'   - 0: No se genera log
     #'   - 1: Consola
     #'   - 2: Fichero
     #'   - 4: Base de datos
     #'   Default value: 0
     #' @param file (`character(1)`)\cr
     #'   Path al fichero de log si se va a utilizar
     #'   - 0: No se genera log
     #'   - 1: Consola
     #'   - 2: Fichero
     #'   - 4: Base de datos
     #'   Default value: NULL
     #' @param envvars (`character(1)`)\cr
     #'   Prefijo de variable de entorno
     #'   Si no se especifica `level`, `output` o `file` se intentara obtener los valores
     #'   de las variables de entorno:
     #'   - `ENVVARS`_LOG_LEVEL
     #'   - `ENVVARS`_LOG_OUTPUT
     #'   - `ENVVARS`_LOG_FILE
     #'
     #'   Es una manera alternativa de inicializar el logger cuando este es compartido por
     #'   varios modulos de una misma aplicacion
     initialize   = function(module="general", level, output, file, envvars) { #}, shared=FALSE) {
         private$module   = module
         private$initLogger(level, output, file, envvars, shared)
     }
     #' @description
     #' Destructor. Cierra ficheros y limpia memoria
     ,finalize     = function()       {
        tryCatch({
        if (!is.null(private$logFile) && isOpen(private$logFile)) close(private$logFile)
        }, error = function (e) {
           message("da error en el finalize")
           #Ocultar el error
        })

     }
     #' @description Establece el nuevo nivel de logging
     #' @param level (`integer(1)`).
     #' @return this
     ,setLogLevel  = function(level)  {
        private$.level = level
        invisible(self)
     }
     #' @description Establece las nuevas salidas
     #' @param output (`integer(1)`).
     #' @return this
     ,setLogOutput = function(output) {
        private$.output = output
        if (bitwAnd(private$.output, 1) > 0) private$toConsole = TRUE
        if (bitwAnd(private$.output, 2) > 0) private$toFile    = TRUE
        invisible(self)
     }
     #' @description Establece el fichero de logging
     #' @param file Nombre del fichero
     #' @return this
     ,setLogFile = function(file) {
        private$.file = file
        private$openFile()
        invisible(self)
     }
     #' @description Genera una linea de log en las salidas adecuadas
     #' @param level (`integer(1)`)   Minimo nivel de logging necesario.
     #' @param fmt   (`character(1)`) Mensaje o formato de tipo printf a generar.
     #' @param ...                    Parametros para formatear el mensaje.
     #' @return this
     #' @examples
     #' \dontrun{
     #' log(1,"Mensaje")
     #' log(1,"Numero %d",  3)
     #'}
     ,log          = function(level, fmt,...) {
         private$println(private$type$LOG, level, fmt, ...)
         invisible(self)
     }
     #' @description Escribe el mensaje de log en las salidas adecuadas
     #' **NO** finaliza la linea
     #' @param level (`integer(1)`)   Minimo nivel de logging necesario.
     #' @param fmt   (`character(1)`) Mensaje o formato de tipo printf a generar.
     #' @param ...                    Parametros para formatear el mensaje.
     #' @return this
     #' @examples
     #' \dontrun{
     #' log(1,"Mensaje")
     #' log(1,"Numero %d",  3)
     #'}
     ,logc         = function(level, fmt,...) {
         private$print(private$type$LOG, level, fmt, ...)
         invisible(self)
     }
     #' @description Genera una linea de error.
     #' Los errores en consola son dirigidos a stderr.
     #' @param fmt  Mensaje o formato de tipo printf a generar.
     #' @param ...  Parametros para formatear el mensaje.
     #' @return this
     ,err          = function(fmt, ...) {
        old = private$.toConsole
        private$.toConsole = TRUE
        private$print(private$type$ERR, 0, fmt, ...)
        private$.toConsole = old
        invisible(self)
      }
     #' @description Genera un mensaje de aviso.
     #' @param fmt  Mensaje o formato de tipo printf a generar.
     #' @param ...  Parametros para formatear el mensaje.
     #' @return this
       ,warn      = function(fmt, ...) {
           private$println(private$type$WARNING, 0, fmt, ...)
           invisible(self)
       }
    #  ,doing = function(level, fmt, ...) {
    #    # Proceso en marcha, espera un done. Fichero se guarda
    #      .print(self$type$ACT, level, .mountMessage(fmt, ...))
    #      private$.cont = TRUE
    #   }
    #  ,done = function(level, fmt, ...) {
    #     .println(self$type$ACT, level, .mountMessage(fmt, ...))
    #   }
    #  ,batch = function(fmt, ...) {
    #       .println(self$type$BATCH, 0, .mountMessage(fmt,...))
    #    }
    #  ,process   = function(level, fmt, ...) {
    #       if (level > level) return (invisible(self))
    #       msg = .mountMessage(fmt,...)
    #       .println(2, level, msg)
    #       invisible(self)
    #    }
    #    ,info      = function(level, fmt, ...) {
    #       if (level > level) return (invisible(self))
    #       msg = .mountMessage(fmt,...)
    #       .println(3, level, msg)
    #       invisible(self)
    #    }
    #    ,executed  = function(rc, begin, fmt, ...) {
    #       diff = as.numeric(Sys.time()) - begin
    #       diff = round(diff, 0)
    #       pattern = paste0("%d;%d;", fmt)
    #       .println(self$type$PROCESS, 0, .mountMessage(pattern, rc, diff, ...))
    #        if (level > 0) {
    #            .toConsole(self$type$SUMMARY, 1, paste("Elapsed time:", diff))
    #            .toConsole(self$type$SUMMARY, 1, paste("Return code :", rc))
    #       }
    #       invisible(self)
    #   }
    #    ,message   = function(fmt, ...) {
    #      .println(5, 3, .mountMessage(fmt, ...))
    #      invisible(self)
    #    }
    #    ,beg       = function(name, level = 0) {
    #        if (level > level) return (invisible(self))
    #        idx = length(logTimers)
    #        if (idx == 0) {
    #            private$logTimers = as.integer(Sys.time())
    #            private$logNames  = name
    #        } else {
    #            private$logTimers = c(logTimers, as.integer(Sys.time()))
    #            private$logNames  = c(logNames, name)
    #        }
    #        idx = length(logTimers)
    #        message("BEG - %d - %s", logTimers[idx], name)
    #        invisible(self)
    #    }
    #    ,end       = function(name) {
    #        idx = which.max(logNames)
    #        if (length(idx) == 0) return (invisible(self))
    #        idx = idx[1]
    #        from = length(longNames)
    #        while (from > idx ) {
    #           diff = as.integer(Sys.time()) - logTimers[from]
    #           message("END - %d - %d - %s", as.integer(Sys.time()), diff, logNames[from])
    #           from = from - 1
    #        }
    #        diff = as.integer(Sys.time()) - logTimers[idx]
    #        message("END - %d - %d - %s", as.integer(Sys.time()), diff, name)
    #        if (idx == 1) {
    #            private$logTimers = c()
    #            private$logNames  = c()
    #        } else {
    #            private$logTimers = logTimers[1:(idx - 1)]
    #            private$logNames  = logNames [1:(idx - 1)]
    #        }
    #        invisible(self)
    #    }
    #    ,fail = function(cond) {
    #       data=""
    #       tags = names(cond)
    #       for (idx in 1:length(tags)) {
    #          data=paste0(data,";",tags[idx],": ", cond[[idx]])
    #       }
    #       # .toFile(self$type$ERROR, 0, sprintf( "Class: %s;Message: %s;Fields: %d;%s"
    #       #                                     ,class(cond)[2], cond, length(tags),data))
    #    }
    #  ,running = function () {
    #     # Friendly method for processes active
    #     message(paste("Process",  modName, "already running"))
    #  }
    #  #######################################################################
    #  # Only for console
    #  #######################################################################
    # ,lbl = function(fmt, ...) {
    #     if ((output %% 2) == 1) cat(sprintf(fmt, ...))
    #     invisible(self)
    # }
    # ,out = function(fmt, ...) {  # message to stdout
    #    # message to stdout
    #     if ((output %% 2) == 1) cat(.mountMessage(fmt, ...))
    #     invisible(self)
    # }
    # ,outln = function(fmt, ...) {
    #    if ( missing(fmt)) out("\n")
    #    if (!missing(fmt)) out(paste0(sprintf(fmt, ...), "\n"))
    #    invisible(self)
    # }
    # ,ok = function (txt) {
    #    lbl = ifelse(missing(txt), "OK", txt)
    #    lbl = paste0("\t", lbl, "\n")
    #    if ((output %% 2) == 1) cat(crayon::bold(lbl))
    #    invisible(self)
    # }
    # ,ko = function (txt) {
    #    lbl = ifelse(missing(txt), "KO", txt)
    #    lbl = paste0("\t", lbl, "\n")
    #    if ((output %% 2) == 1) cat(crayon::bold(crayon::red(lbl)))
    #    invisible(self)
    # }

  )
    ,private = list(
        type = list(PROCESS =  1,BATCH   =  5,LOG     = 10,SUMMARY = 11, ACT=20, WARNING=30, ERROR=99)
       ,.level     = 0
       ,.output    = 0
       ,.file      = NULL
       ,.coloured  = TRUE
       ,.toConsole = FALSE
       ,.toFile    = FALSE
       ,module     = "SDPLogger"
       ,NLConsole  = TRUE
       ,NLFile     = TRUE
       ,logFile    = NULL

       # ,modName  = "YATA"
       # ,logTimers = NULL
       # ,logNames  = NULL
       ,println = function(type, level, fmt, ...) {
          if (level > private$.level) return()
          data = private$mountMessage(type, fmt, ...)
          if (substring(data$msg, nchar(data$msg)) != "\n") data$msg = paste0(data$msg, '\n')
          private$printMsg(data$type, data$msg, data$ansi)
       }
       ,print = function(type, level, fmt, ...) { # msg, ansi=private$void) {
          if (level > private$.level) return()
          data = private$mountMessage(type, fmt, ...)
          private$printMsg(data$type, data$msg, data$ansi)
       }
       ,printMsg = function(type, txt, ansi) { # msg, ansi=private$void) {
           msg  = substring(txt,1, nchar(txt) - 1)
           last = substring(txt, nchar(txt))
           if (private$.toFile)    private$toFile(type, msg, last)
           if (private$.toConsole) private$toConsole(type, msg, last, ansi)
       }
      ,toFile = function(type, msg, last) {
           # Verificar si el archivo existe cuando se va a usar
           if (is.null(private$logFile)) private$openFile()
           if (!private$.toFile) return()
           txt  = msg
           dt = paste(as.integer(Sys.time()),format(Sys.time(), "%Y/%m/%d %H:%M:%S"),sep=";")
           dt = paste(dt, private$module,type, sep=';')
           if (private$NLFile) txt = paste(dt, msg, sep=';')
           txt  = gsub("\\n",paste0("\n",dt,";"), txt)
           txt = paste0(txt, last)
           cat(txt, file=private$logFile, append=TRUE)
           private$NLFile = (last == "\n")
        }
        ,toConsole = function(type, msg, last, ansi) {
           txt  = msg
           dt = format(Sys.time(), "%H:%M:%S")
           if (private$NLConsole) txt = paste(format(Sys.time(), "%H:%M:%S"), "-", msg)
           txt  = gsub("\\n","\n           ", txt)

           txt = paste0(txt, last)
           if (private$isError(type)) {
               cat(crayon::red(txt), file = stderr())
           } else {
              cat(ansi(txt))
           }
           private$NLConsole = (last == "\n")
        }
       ,void = function(txt) { txt }

       ,initLogger = function (level, output, file, envvars, shared) {
           # Primero miramos en el entorno, luego sobreescribimos
           if (!missing(envvars)) {
              private$.level  = private$getEnvInteger(envvars, "_LOG_LEVEL")
              private$.output = private$getEnvInteger(envvars, "_LOG_OUTPUT")
              private$.file   = private$getEnvString (envvars, "_LOG_FILE")
           }
           if (!missing(level))  private$.level  = level
           if (!missing(output)) private$.output = output
           if (!missing(file))   private$.file   = file

           private$.toConsole = ifelse (bitwAnd(private$.output, 1) > 0, TRUE, FALSE)
           private$.toFile    = ifelse (bitwAnd(private$.output, 2) > 0, TRUE, FALSE)
           if (!is.null(private$.file)) private$openFile()
       }
       ,getEnvInteger = function(prfx, value) {
           value = Sys.getenv(paste0(prfx, value))
           value = suppressWarnings(as.integer(value))
           ifelse (is.na(value),0, value)
       }
       ,getEnvString = function(prfx, value) {
           Sys.getenv(paste0(prfx, value))
       }
       ,openFile = function () {
          if (is.null(private$.file)) {
             private$fileWarning(private$.file)
             return()
          }
          tryCatch({
            logfile = normalizePath(private$.file, mustWork = FALSE)
            private$logFile = file(logfile, open="at", blocking = FALSE)
          }, warning = function (e) {
             private$fileWarning(private$.file)
          }, error = function (e) {
             private$fileWarning(private$.file)
          })
       }
       ,mountMessage = function(logType, fmt, ...) {
          data =list(type = logType, msg = NULL, ansi = private$void)
          if (logType == private$type$WARNING) data$ansi = crayon::blue
          if (logType == private$type$ERROR)  data$ansi  = crayon::red

          data$msg =  tryCatch({
              txt = sprintf(fmt, ...)
              gsub("/t", "    ", txt, fixed=TRUE)
           }, error = function (e) {
              paste0("Malformed message: \'",fmt, "\'")
              data$type = private$type$ERROR
              data$ansi = crayon::red()
           })
          data
       }
       ,isError = function (type) { type == private$type$ERROR }
       ,fileWarning = function (file) {
          private$.toFile = FALSE
          if (is.null(file))  self$warn("No se ha especificado fichero de logging")
          if (!is.null(file)) self$warn(paste("No se ha podido abrir el fichero de logging:", file))
       }
    )
)

