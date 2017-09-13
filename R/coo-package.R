
##### We include magrittr pipes when loading Momocs

##### Package documentation and NAMESPACE import

#' coo
#'
#' A complete toolkit for morphometrics, from data extraction to multivariate analyses.
#' Most common 2D morphometrics approaches are included:
#' outlines, open outlines, configurations of landmarks, traditional morphometrics,
#' and facilities for data preparation, manipulation and visualization
#' with a consistent grammar throughout.
#' Momocs allows reproducible, complex morphometric analyses,
#' paves the way for a pure open-source workflow in R,
#' and other morphometrics approaches should be easy to plug in,
#' or develop from, on top of this canvas.
#'
#' To cite Momocs in publications: \code{citation("Momocs")}.
#'
#' @docType package
#' @name coo
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# prevents "no visible binding for global variable"
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(c("."))
