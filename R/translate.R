#' Translate mod to Momocs' Out
#'
#' Turns into
#' @param x simple or multiple `.mod`, as path(s) or a vector of `character`
#' @family translate
#' @export
translate_Out <- function(x){
  x <- group(x)
  Out <- list(coo=x$coo, fac=x$cov)
  class(Out) <- c("Out", "Coo")
  Out
}

#' Translate mod to Momocs' Opn
#'
#' Turns into
#' @param x simple or multiple `.mod`, as path(s) or a vector of `character`
#' @family translate
#' @export
translate_Opn <- function(x){
  x <- group(x)
  Opn <- list(coo=x$coo, fac=x$cov)
  class(Opn) <- c("Opn", "Coo")
  Opn
}


#' Translate mod to Momocs' Ldk
#'
#' Turns into
#' @param x simple or multiple `.mod`, as path(s) or a vector of `character`
#' @family translate
#' @export
translate_Ldk <- function(x){
  x <- group(x)
  Ldk <- list(coo=x$coo, fac=x$cov)
  class(Ldk) <- c("Ldk", "Coo")
  Ldk

}
