#' Export to single coo file from a shape
#'
#' Nothing else than coordinates is supported
#' @param x \code{matrix}
#' @param file \code{character} the name of the file to be created
#' @family export
#' @export
export_shp <- function(x, file){
  x %>%
    convert() %>%
    writeLines(con=file)
}

#' Export to a single coo file a single shape from Coo Momocs' objects
#'
#' @param x any \code{Coo} object from \code{Momocs}
#' @param id \code{numeric}, which shape to export
#' @param file \code{character} the name of the file to be created
#' @param force \code{logical} whether to replace file
#' @family export
#' @export
export_Coo1 <- function(x, id, file, force=FALSE){
  # if file is missing, inherits x[id] name
  if (missing(file))
    file <- paste0(names(x)[id], ".coo")
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

#' Export to a single or separate coo file a whole Coo Momocs' object
#'

#' @param x any \code{Coo} object from \code{Momocs}
#' @param file \code{character} the name of the file(s) to be created
#' @param separate \code{logical} whether to create separate coo files
#' @param force \code{logical} whether to replace files
#' @param zip \code{logical} whether to zip the created file
#' @family export
#' @examples
#' \dontrun{
#' library(Momocs)
#' bot %>%
#'    slice(c(1:5, 21:25)) %>% coo_sample(12) %>%
#'    mutate(size=coo_centsize(.)) %>%
#'    export_Coo("bot_lite")
#' }
#' @export
#'
export_Coo <- function(x, file,
                      separate=FALSE, force=FALSE, zip=FALSE){
  if (separate){
    silent <- lapply(seq_along(x),
                     function(.) export_Coo1(x=x, id=., force=force))
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
    if (zip){
      suppressMessages(zip(gsub("coo", "zip", file), file, flags = "-r9Xq"))
      silent <- file.remove(file)
    }
  }
}
