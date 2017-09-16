#' Import a single mod
#'
#' @param x mod, as path or `character vector
#' @family import
#' @export
import_mod1 <- function(x){
  ### if a valid path is provided, import the file
  if (.is.path(x))
    x <- x %>% readLines(warn = FALSE)

  ### polish the file
  x <- .shave(x)

  ### filename
  # if more than one filename is provided,
  # the first is saved but all are removed
  x <- grep("^~", x, invert=TRUE, value=TRUE)

  ### covariates
  cov_pos <- grep("[[:alpha:]]+[[:alnum:]]* [[:alnum:]]", x)
  # if covariates are present, we fill a data.frame
  if (length(cov_pos)>0){
    cov     <- x[cov_pos] %>% .str2df()
    x       <- x[-cov_pos]
  } else {
    cov   <- data.frame()
  }

  ### coordinates
  part_pos   <- grep("[[:alpha:]]", x)
  # if partition(s) is(are) named
  if (length(part_pos)>0){
    part_start <- part_pos+1
    part_end   <- c(part_pos[-1]-1, length(x))
    coo <- vector("list", length(part_pos))
    for (i in seq_along(part_pos)){
      coo[[i]] <- x[part_start[i]:part_end[i]]
    }
    names(coo) <- x[part_pos]
    coo <- lapply(coo, .str2mtx)
  } else {
    coo <- .str2mtx(x)
  }
  # if no coordinates, coo will be NULL here,
  # so return an empty matrix instead
  if (is.null(x))
    coo <- matrix(nrow=0, ncol=0)

  ### a list is returned
  list(coo=coo, cov=cov)
}

#' Import multiple mod
#'
#' @param x mod, as paths or a character vector
#' @family import
#' @export
import_mod <- function(x){
  # if all paths are valid, import the files
  if (all(sapply(x, .is.path))){
    x <- lapply(x, readLines, warn=FALSE)
    # turns them into a single vector
    x <- do.call("c", x)
  }
  # extract names
  fn <- grep("^~", x, value=TRUE) %>% gsub("~", "", .)
  # split this vector into mod1s and name them
  x <- .cuts_into_list(x, "^~")
  names(x) <- fn
  # import all mod1s
  lapply(x, import_mod1)
}
