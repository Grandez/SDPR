#' Convertir cadenas a modo titulo
#'
#' @param texts Un vector de caracteres
#' @param locale Codigo de idioma a usar. Por defecto "es"
#' @return Un vector de caracteres de la misma longitud que `texts` en modo titulo
#' @export
str_title_case = function(texts, locale="es") { stringr::str_to_title(texts, locale) }

#' Convertir las cadenas a minusculas
#'
#' @param texts Un vector de caracteres
#' @return Un vector de caracteres de la misma longitud que `texts` con los caracteres en minusculas
#' @export
str_to_lower   = function(texts) { base::tolower(texts) }

#' Convertir las cadenas a mayusculas
#'
#' @param texts Un vector de caracteres
#' @return Un vector de caracteres de la misma longitud que `texts` con los caracteres en mayusculas
#' @export
str_to_upper   = function(texts) { base::toupper(texts) }
