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
         private$.module   = module
         private$initLogger(level, output, file, envvars, shared)
     }
     #' @description
     #' Destructor. Cierra ficheros y limpia memoria
     ,finalize     = function()       {
        if (!is.null(private$logFile) && isOpen(private$logFile)) close(private$logFile)
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
        private$print(private$type$ERR, 0, fmt, ...)
        invisible(self)
      }

    #  ,logn         = function(level, fmt,...) {
    #     .print(self$type$LOG, level, .mountMessage(fmt, ...))
    #  }
    #  ,warning      = function(fmt, ...) { message(.mountMessage(fmt, ...)) }
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
        type = list(PROCESS =  1,BATCH   =  5,LOG     = 10,SUMMARY = 11, ACT=20, ERROR=99)
       ,.level  = 0
       ,.output = 0
       ,.module = "SDPLogger"
       ,.file     = NULL
       ,.coloured = TRUE
       ,.toConsole = FALSE
       ,.toFile    = FALSE
       ,.cont = FALSE
       ,NL    = TRUE
       ,logFile  = NULL

       # ,modName  = "YATA"
       # ,logTimers = NULL
       # ,logNames  = NULL
       ,println = function(type, level, fmt, ...) {
          if (level > private$.level) return()
          private$.cont = FALSE
          data = private$mountMessage(type, fmt, ...)
          data$msg = paste0(data$msg, '\n')
          private$printMsg(data$type, data$msg, data$ansi)
       }
       ,print = function(type, level, fmt, ...) { # msg, ansi=private$void) {
          if (level > private$.level) return()
          private$.cont = TRUE
          data = private$mountMessage(type, fmt, ...)
          private$printMsg(data$type, data$msg, data$ansi)
       }
       ,printMsg = function(type, msg, ansi) { # msg, ansi=private$void) {
          if (private$.toFile)    private$toFile(type, msg)
          if (private$.toConsole) private$toConsole(type, msg, ansi)
       }

        ,toFile = function(type, txt, ...) {
#            if (level > level) return()
#            str = ""
#            if (!.cont) {
#                str = Sys.time()
#                str = sub(" ", "-", str)
#            }
#            line = paste(str,modName,type,level,txt, sep=";")
#            rest = paste(list(...), collapse=";")
#            if (nchar(rest) > 0) line = paste0(line, ";",rest)
#            cat(paste0(line, "\n"), file=logFile, append=TRUE)
        }
        ,toConsole = function(type, txt, ansi) {
           msg  = substring(txt,1, nchar(txt) - 1)
           last = substring(txt, nchar(txt))
           if (private$NL) msg = paste(format(Sys.time(), "%H:%M:%S"), "-", msg)
           msg  = gsub("\\n","\n           ", msg)
           msg = paste0(msg, last)
           private$NL = (last == "\n")
           if (private$isError(type)) {
               cat(crayon::red(msg), file = stderr())
           } else {
              cat(msg)
           }
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
           #     wd      = yataGetDirectory("log")
           #     logfile = ifelse(shared, "YATA", modName)

           # value = Sys.getenv("YATA_LOG_LEVEL")
           # value = suppressWarnings(as.integer(value))
           # if (!is.na(value)) private$level  = value
           #
           # if (!missing(level)) private$level  = level
           #
           # value = Sys.getenv("YATA_LOG_OUTPUT")
           # value = suppressWarnings(as.integer(value))
           # if (!is.na(value))    private$output = value
           # if (!missing(output)) private$output = output
           #
           # if (bitwAnd(output, 2) > 0) {
           #     wd      = yataGetDirectory("log")
           #     logfile = ifelse(shared, "YATA", modName)
           #     logfile = normalizePath(file.path(wd, paste0(logfile, ".log")), mustWork = FALSE)
           #     private$logFile = file(logfile, open="at", blocking = FALSE)
           # }
       }
       ,getEnvInteger = function(prfx, value) {
           value = Sys.getenv(paste0(prfx, value))
           value = suppressWarnings(as.integer(value))
           ifelse (is.na(value),0, value)
       }
       ,getEnvString = function(prfx, value) {
           Sys.getenv(paste0(prfx, value))
       }
       ,mountMessage = function(logType, fmt, ...) {
          data =list(type = logType, msg = NULL, ansi = private$void)
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
    )
)

