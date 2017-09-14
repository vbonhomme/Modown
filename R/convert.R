#' Convert to coo from a single shape
#'
#' Returns a coo from coordinates, covariates or both.
#' @param x \code{matrix} of coordinates,
#' or \code{data.frame} of covariates,
#' or a \code{list} with
#' \code{$coo} (\code{matrix}) and \code{$cov} (\code{data.frame})
#' @family convert
#' @export
convert <- function(x){
  if (is.matrix(x))
   return(.mtx2str(x))
  if (is.data.frame(x))
    return(.df2str(x))
  if (is.list(x) && all(c("coo", "cov") %in% names(x)))
    return(c(.df2str(x$cov), .mtx2str(x$coo)))
}

#' Convert to coo from a single shape from Coo Momocs' objects
#'
#' @param x any \code{Coo} object from Momocs
#' @param id \code{numeric}, which shape to convert
#' @family convert
#' @export
convert_Coo1 <- function(x, id){
  name <- paste0("~", names(x)[id])
  cov  <- paste(names(x$fac), x$fac[id, ] %>% unlist %>% as.character())
  coo  <- apply(x$coo[[id]], 1, paste, collapse=" ")
  c(name, cov, coo)
}

#' Convert to coo a whole Coo Momocs' object
#' @param x any \code{Coo} object from Momocs
#' @family convert
#' @export
convert_Coo <- function(x){
  seq_along(x) %>%
    lapply(function(.) convert_Coo1(x, .)) %>%
    do.call("c", .)
}
