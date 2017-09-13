#' Translate coo to other languages
#'
#' Plop
#' @param x simple or multiple coo(s), as path or character vector
#' @family translate
#' @export
translate <- function(x){
  # grab all coos
  coo <- x %>%
    sapply(`[`, "coo") %>%
    `names<-`(names(x))

  # grab all facs
  cov <- x %>%
    sapply(`[`, "cov") %>%
    do.call("rbind", .)

  # return this beauty
  list(coo=coo, cov=cov)
}

#' Translate coo to Momocs' Out
#'
#' Turns into
#' @param x simple or multiple coo(s), as path or character vector
#' @family translate
#' @export
translate_Out <- function(x){
  x <- translate(x)
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
  x <- translate(x)
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
  x <- translate(x)
  Ldk <- list(x$coo, fac=x$cov)
  class(Ldk) <- c("Ldk", "Coo")
  Ldk

}
