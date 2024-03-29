% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logger.R
\name{SDPLogger}
\alias{SDPLogger}
\title{Tratamiento de logs}
\description{
Clase base/abstract para el manejo de logs de las aplicaciones

Escribe a
\itemize{
\item 0 = No se genera log
\item 1 = Consola
\item 2 = Archivo
\item 4 = Base de datos
}

Salvo para consola es necesario indicarle la informacion necesaria
respecto a ficheros y bases de datos
}
\examples{

## ------------------------------------------------
## Method `SDPLogger$log`
## ------------------------------------------------

\dontrun{
log(1,"Mensaje")
log(1,"Numero \%d",  3)
}

## ------------------------------------------------
## Method `SDPLogger$logc`
## ------------------------------------------------

\dontrun{
log(1,"Mensaje")
log(1,"Numero \%d",  3)
}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{level}}{Nivel de log}

\item{\code{output}}{Salidas de log activas como combinacion/suma de}

\item{\code{coloured}}{Valor logico. Formatear las salidas por consola}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SDPR.LOGGER-new}{\code{SDPLogger$new()}}
\item \href{#method-SDPR.LOGGER-finalize}{\code{SDPLogger$finalize()}}
\item \href{#method-SDPR.LOGGER-setLogLevel}{\code{SDPLogger$setLogLevel()}}
\item \href{#method-SDPR.LOGGER-setLogOutput}{\code{SDPLogger$setLogOutput()}}
\item \href{#method-SDPR.LOGGER-setLogFile}{\code{SDPLogger$setLogFile()}}
\item \href{#method-SDPR.LOGGER-log}{\code{SDPLogger$log()}}
\item \href{#method-SDPR.LOGGER-logc}{\code{SDPLogger$logc()}}
\item \href{#method-SDPR.LOGGER-err}{\code{SDPLogger$err()}}
\item \href{#method-SDPR.LOGGER-warn}{\code{SDPLogger$warn()}}
\item \href{#method-SDPR.LOGGER-clone}{\code{SDPLogger$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SDPR.LOGGER-new"></a>}}
\if{latex}{\out{\hypertarget{method-SDPR.LOGGER-new}{}}}
\subsection{Method \code{new()}}{
Crea una nueva instancia de \link[R6:R6Class]{R6} la clase.

Notese que esta clase se creara normalente como clase base de otra
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SDPLogger$new(module = "general", level, output, file, envvars)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{module}}{(\code{character(1)})\cr
Etiqueta del modulo que genera los mensajes de logging
Default value: "General"}

\item{\code{level}}{(\code{integer(1)})\cr
Nivel de logging que se aplicará a la instancia
Default value: 0}

\item{\code{output}}{(\code{integer(1)})\cr
Donde se generará la salida. Es una mascara de bits
\itemize{
\item 0: No se genera log
\item 1: Consola
\item 2: Fichero
\item 4: Base de datos
Default value: 0
}}

\item{\code{file}}{(\code{character(1)})\cr
Path al fichero de log si se va a utilizar
\itemize{
\item 0: No se genera log
\item 1: Consola
\item 2: Fichero
\item 4: Base de datos
Default value: NULL
}}

\item{\code{envvars}}{(\code{character(1)})\cr
Prefijo de variable de entorno
Si no se especifica \code{level}, \code{output} o \code{file} se intentara obtener los valores
de las variables de entorno:
\itemize{
\item \code{ENVVARS}_LOG_LEVEL
\item \code{ENVVARS}_LOG_OUTPUT
\item \code{ENVVARS}_LOG_FILE
}

Es una manera alternativa de inicializar el logger cuando este es compartido por
varios modulos de una misma aplicacion}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SDPR.LOGGER-finalize"></a>}}
\if{latex}{\out{\hypertarget{method-SDPR.LOGGER-finalize}{}}}
\subsection{Method \code{finalize()}}{
Destructor. Cierra ficheros y limpia memoria
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SDPLogger$finalize()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SDPR.LOGGER-setLogLevel"></a>}}
\if{latex}{\out{\hypertarget{method-SDPR.LOGGER-setLogLevel}{}}}
\subsection{Method \code{setLogLevel()}}{
Establece el nuevo nivel de logging
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SDPLogger$setLogLevel(level)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{level}}{(\code{integer(1)}).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
this
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SDPR.LOGGER-setLogOutput"></a>}}
\if{latex}{\out{\hypertarget{method-SDPR.LOGGER-setLogOutput}{}}}
\subsection{Method \code{setLogOutput()}}{
Establece las nuevas salidas
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SDPLogger$setLogOutput(output)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{output}}{(\code{integer(1)}).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
this
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SDPR.LOGGER-setLogFile"></a>}}
\if{latex}{\out{\hypertarget{method-SDPR.LOGGER-setLogFile}{}}}
\subsection{Method \code{setLogFile()}}{
Establece el fichero de logging
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SDPLogger$setLogFile(file)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{file}}{Nombre del fichero}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
this
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SDPR.LOGGER-log"></a>}}
\if{latex}{\out{\hypertarget{method-SDPR.LOGGER-log}{}}}
\subsection{Method \code{log()}}{
Genera una linea de log en las salidas adecuadas
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SDPLogger$log(level, fmt, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{level}}{(\code{integer(1)})   Minimo nivel de logging necesario.}

\item{\code{fmt}}{(\code{character(1)}) Mensaje o formato de tipo printf a generar.}

\item{\code{...}}{Parametros para formatear el mensaje.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
this
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
log(1,"Mensaje")
log(1,"Numero \%d",  3)
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SDPR.LOGGER-logc"></a>}}
\if{latex}{\out{\hypertarget{method-SDPR.LOGGER-logc}{}}}
\subsection{Method \code{logc()}}{
Escribe el mensaje de log en las salidas adecuadas
\strong{NO} finaliza la linea
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SDPLogger$logc(level, fmt, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{level}}{(\code{integer(1)})   Minimo nivel de logging necesario.}

\item{\code{fmt}}{(\code{character(1)}) Mensaje o formato de tipo printf a generar.}

\item{\code{...}}{Parametros para formatear el mensaje.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
this
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
log(1,"Mensaje")
log(1,"Numero \%d",  3)
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SDPR.LOGGER-err"></a>}}
\if{latex}{\out{\hypertarget{method-SDPR.LOGGER-err}{}}}
\subsection{Method \code{err()}}{
Genera una linea de error.
Los errores en consola son dirigidos a stderr.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SDPLogger$err(fmt, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{fmt}}{Mensaje o formato de tipo printf a generar.}

\item{\code{...}}{Parametros para formatear el mensaje.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
this
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SDPR.LOGGER-warn"></a>}}
\if{latex}{\out{\hypertarget{method-SDPR.LOGGER-warn}{}}}
\subsection{Method \code{warn()}}{
Genera un mensaje de aviso.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SDPLogger$warn(fmt, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{fmt}}{Mensaje o formato de tipo printf a generar.}

\item{\code{...}}{Parametros para formatear el mensaje.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
this
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SDPR.LOGGER-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SDPR.LOGGER-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SDPLogger$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
