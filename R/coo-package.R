#' Modown
#'
#' A minimalist syntax for morphometrics data
#'
#' To cite Modown in publications: `citation("Modown")`.
#'
#' @docType package
#' @name Modown
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# prevents "no visible binding for global variable"
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(c("."))
