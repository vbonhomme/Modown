#' Write to single coo file from a shape
#'
#' Nothing else than coordinates is supported
#' @param x \code{matrix}
#' @param file \code{character} the name of the file to be created
#' @param force \code{logical} whether to replace file
#' @family write
#' @export
write <- function(x, file, force=FALSE){
  # checks if file exist and whether to force replacement
  if (file.exists(file) & !force)
    stop("file already exists: pick another name or remove it.")

  x %>%
    convert() %>%
    writeLines(con=file)
}

#' Write to a single coo file a single shape from Coo Momocs' objects
#'
#' @param x any \code{Coo} object from \code{Momocs}
#' @param id \code{numeric}, which shape to write
#' @param file \code{character} the name of the file to be created
#' @param force \code{logical} whether to replace file
#' @family write
#' @export
write_Coo1 <- function(x, id, file, force=FALSE){
  # if file is missing, inherits x[id] name
  if (missing(file))
    file <- paste0(names(x$coo)[id], ".coo")
  # if no extension yet, add it
  if (length(grep(".coo$", file))==0)
    file <- paste0(file, ".coo")
  # checks if file exist and whether to force replacement
  if (file.exists(file) & !force)
    stop("file already exists: pick another name or remove it.")
  # convert and write .coo file
  x %>%
    convert_Coo1(id) %>%
    writeLines(con=file)
}

#' Write to a single or separate coo file a whole Coo Momocs' object
#'

#' @param x any \code{Coo} object from \code{Momocs}
#' @param file \code{character} the name of the file(s) to be created
#' @param separate \code{logical} whether to create separate coo files
#' @param force \code{logical} whether to replace files
#' @family write
#' @examples
#' \dontrun{
#' library(Momocs)
#' bot %>%
#'    slice(c(1:5, 21:25)) %>% coo_sample(12) %>%
#'    mutate(size=coo_centsize(.)) %>%
#'    write_Coo("bot_lite")
#' }
#' @export
#'
write_Coo <- function(x, file,
                      separate=FALSE, force=FALSE){
  if (separate){
    silent <- lapply(seq_along(x$coo),
                     function(.) write_Coo1(x=x, id=.,
                                            file=names(x$coo)[.], force=force))
  } else {
    # if file is missing, inherits x name
    if (missing(file))
      file <- paste0(substitute(x), ".coo")
    # if no extension yet, add it
    if (length(grep(".coo$", file))==0)
      file <- paste0(file, ".coo")
    # checks if file exist and whether to force replacement
    if (file.exists(file) & !force)
      stop("file already exists: pick another name or remove it.")

    # convert and write .coo file
    x %>%
      convert_Coo() %>%
      writeLines(con=file)
  }
}
