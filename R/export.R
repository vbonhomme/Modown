#' Export to coo from a single shape
#'
#' Nothing else than coordinates is supported
#' @param x \code{matrix}
#' @family export
#' @export
export_shp <- function(x){
  apply(x, 1, paste0, collapse=" ")
}

#' Export to coo from a single shape from Coo Momocs' objects
#'
#' @param x any \code{Coo} object from Momocs
#' @param id \code{numeric}, which shape to export
#' @family export
#' @export
export_Coo1 <- function(x, id){
  name <- paste0("~", names(x)[id])
  cov  <- paste(names(x$fac), x$fac[id, ] %>% unlist %>% as.character())
  coo  <- apply(x$coo[[id]], 1, paste, collapse=" ")
  c(name, cov, coo)
}

#' Export to coo a whole Coo Momocs' object
#' @param x any \code{Coo} object from Momocs
#' @family export
#' @export
export_Coo <- function(x){
  seq_along(x) %>%
    lapply(function(.) export_Coo1(x, .)) %>%
    do.call("c", .)
}
