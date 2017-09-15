#' Group elements of a list into a list of elements
#'
#' Useful when a list of coos (with \code{coo} and \code{cov}) must be
#' turned into a list with \code{$coo} as a list of \code{coo} and
#'  an rbinded \code{data.frame} of all \code{cov}s
#' @param x simple or multiple coo(s), as path or character vector
#' @family group
#' @export
group <- function(x){
  # grab all coos
  coo <- x %>%
    sapply(`[`, "coo") %>%
    `names<-`(names(x))

  # grab all facs
  cov <- x %>%
    sapply(`[`, "cov") %>%
    do.call("rbind", .) %>%
    `rownames<-`(NULL)

  # return this beauty
  list(coo=coo, cov=cov)
}
