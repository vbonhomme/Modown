#' Translate coo to Momocs' Out
#'
#' Turns into
#' @param x simple or multiple coo(s), as path or character vector
#' @family translate
#' @export
translate_Out <- function(x){
  x <- group(x)
  Out <- list(coo=x$coo, fac=x$cov)
  class(Out) <- c("Out", "Coo")
  Out
}

#' Translate coo to Momocs' Opn
#'
#' Turns into
#' @param x simple or multiple coo(s), as path or character vector
#' @family translate
#' @export
translate_Opn <- function(x){
  x <- group(x)
  Opn <- list(coo=x$coo, fac=x$cov)
  class(Opn) <- c("Opn", "Coo")
  Opn
}


#' Translate coo to Momocs' Ldk
#'
#' Turns into
#' @param x simple or multiple coo(s), as path or character vector
#' @family translate
#' @export
translate_Ldk <- function(x){
  x <- group(x)
  Ldk <- list(x$coo, fac=x$cov)
  class(Ldk) <- c("Ldk", "Coo")
  Ldk

}
