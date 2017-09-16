#' Convert to mod from a single shape
#'
#' Returns a mod from coordinates, covariates or both.
#' @param x `matrix` of coordinates,
#' or `data.frame` of covariates,
#' or a `list` with
#' `$coo` (`matrix`) and `$cov` (`data.frame`)
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

#' Convert to mod from a single shape from Coo Momocs' object
#'
#' @param x any `Coo` object from Momocs
#' @param id `numeric`, which shape to convert
#' @family convert
#' @export
convert_Coo1 <- function(x, id){
  name <- paste0("~", names(x)[id])
  cov  <- paste(names(x$fac), x$fac[id, ] %>% unlist %>% as.character())
  coo  <- .mtx2str(x$coo[[id]])
  c(name, cov, coo)
}

#' Convert to mod a whole Coo Momocs' object
#' @param x any `Coo` object from Momocs
#' @family convert
#' @export
convert_Coo <- function(x){
  seq_along(x) %>%
    lapply(function(.) convert_Coo1(x, .)) %>%
    do.call("c", .)
}
