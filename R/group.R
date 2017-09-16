#' Group coo and cov from a list
#'
#' Useful when a list of `mod`s (with `coo` and `cov`) must be
#' turned into a list with `$coo` as a list of `coo` and
#'  an rbinded `data.frame` of all `cov`s
#' @param x simple or multiple `mod`(s), as path or `character` vector
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
